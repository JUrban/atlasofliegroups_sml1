use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Coordinates.sml";
use "atlas-scripts-sml/induction.sml";

(*
  File: atlas-scripts-sml/G2_unitary_dual.sml

  Purpose
  - SML translation of `atlas-scripts/G2_unitary_dual.at`.
  - Explores families of parameters for split real `G2` and prints a table
    including unitary/hermitian data and “good-range induction” witnesses.

  What this file is (and isn’t)
  - This is an executable SML script/module, not a general-purpose library.
    It uses a small amount of local arithmetic to mimic the `.at` script’s
    computations and calls into the Atlas C++ library via `AtlasFFI`.
  - The “induced/conj/L” column is computed by `Induction.good_range_induced_from_first_text`.

  Ownership notes
  - Most helper functions here allocate temporary `AtlasFFI.param` handles; they
    are freed before returning from the higher-level routines.
  - If you call `p_s`/`p_l` directly, you own the returned parameter list and
    must free it (see `ParamFinals.freeTerms` and `AtlasFFI.atlas_param_free`).
*)
structure G2_unitary_dual = struct
  (* Minimal rational type used for the scan lines in the original `.at` script. *)
  type rat = {num: int, den: int}

  (* Inject an integer into `rat`. *)
  fun ratOfInt n : rat = {num = n, den = 1}

  (* Parse whitespace-separated integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("G2_unitary_dual: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Parse a rational weight in Atlas text form: `den n1 ... nk`. *)
  fun parseRatWeightText s : {den: int, nums: int list} =
    (case parseInts s of
       den :: rest => {den = den, nums = rest}
     | _ => raise Fail ("G2_unitary_dual: bad ratweight text: " ^ s))

  (* Serialize an int list using SML `Int.toString` (keeps `~` negatives). *)
  fun intsToText xs =
    String.concatWith " " (List.map Int.toString xs)

  (* Convert SML negative `~n` to C-style `-n` as expected by Atlas parsers. *)
  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  (* Serialize an int list in the Atlas C++ whitespace-separated format. *)
  fun intsToCText xs =
    String.concatWith " " (List.map intToCText xs)

  (* Serialize a `{den,nums}` rational weight as `den n1 ... nk`. *)
  fun ratWeightToText {den, nums} =
    Int.toString den ^ " " ^ intsToText nums

  (* Add two integer vectors (componentwise). *)
  fun addIntVec (xs: int list, ys: int list) =
    let
      fun loop ([], [], acc) = List.rev acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', (a + b) :: acc)
        | loop _ = raise Fail "G2_unitary_dual: addIntVec: length mismatch"
    in
      loop (xs, ys, [])
    end

  (* Dot product of integer vectors. *)
  fun dot (xs: int list, ys: int list) : int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "G2_unitary_dual: dot: length mismatch"
    in
      loop (xs, ys, 0)
    end

  (* FPP membership test on rational coordinate lists (delegates to Coordinates). *)
  fun in_fpp_rat (xs: rat list) : bool = Coordinates.in_fpp_rat xs

  (* FPP membership test for a parameter via its `gamma` coordinate vector. *)
  fun in_fpp_param (g: AtlasFFI.group) (p: AtlasFFI.param) : bool =
    let
      val gamma = Coordinates.parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)
      val coords = Coordinates.coordsRat g gamma
    in
      Coordinates.in_fpp_rat coords
    end

  (* Greatest common divisor (nonnegative result). *)
  fun gcd (a: int, b: int) : int =
    let
      val a = Int.abs a
      val b = Int.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, x mod y)
    in
      if a = 0 then b else loop (a, b)
    end

  (* Normalize a rational number: reduce by gcd and force positive denominator. *)
  fun normRat ({num, den}: rat) : rat =
    if den = 0 then
      raise Fail "G2_unitary_dual: normRat: zero denom"
    else
      let
        val sign = if den < 0 then ~1 else 1
        val num' = num * sign
        val den' = den * sign
        val g = gcd (num', den')
      in
        {num = num' div g, den = den' div g}
      end

  (* Rational addition. *)
  fun addRat (a: rat, b: rat) : rat =
    normRat {num = #num a * #den b + #num b * #den a, den = #den a * #den b}

  (* Rational subtraction. *)
  fun subRat (a: rat, b: rat) : rat =
    normRat {num = #num a * #den b - #num b * #den a, den = #den a * #den b}

  (* Multiply a rational by an integer. *)
  fun mulRatInt (a: rat, k: int) : rat =
    normRat {num = #num a * k, den = #den a}

  (* Divide a rational by a nonzero integer. *)
  fun divRatInt (a: rat, k: int) : rat =
    if k = 0 then raise Fail "G2_unitary_dual: divRatInt: div by 0"
    else normRat {num = #num a, den = #den a * k}

  (* Pretty-print a rational. *)
  fun ratToString (a: rat) : string =
    let
      val a = normRat a
    in
      if #den a = 1 then Int.toString (#num a)
      else Int.toString (#num a) ^ "/" ^ Int.toString (#den a)
    end

  (* Pretty-print a list of rationals (space-separated). *)
  fun ratListToString (xs: rat list) : string =
    "[" ^ String.concatWith "," (List.map ratToString xs) ^ "]"

  (* Convert a pair of rationals into a rank-2 ratweight `{den, nums=[..]}`. *)
  fun ratToRatvec2 (a: rat, b: rat) : {den: int, nums: int list} =
    let
      val a = normRat a
      val b = normRat b
      val den = #den a * #den b
      val n1 = #num a * #den b
      val n2 = #num b * #den a
      val g = gcd (gcd (n1, n2), den)
    in
      {den = den div g, nums = [n1 div g, n2 div g]}
    end

  (* `gamma` parameterization for the short-root Cartan line (matches `.at`). *)
  fun gamma_s (m: rat, v: rat) : {den: int, nums: int list} =
    let
      val v1 = v
      val v2 = divRatInt (subRat (m, v), 2)
    in
      ratToRatvec2 (v1, v2)
    end

  (* `gamma` parameterization for the long-root Cartan line (matches `.at`). *)
  fun gamma_l (m: rat, v: rat) : {den: int, nums: int list} =
    let
      val a1 = divRatInt (addRat (m, mulRatInt (v, 3)), 2)
      val a2 = mulRatInt (v, ~1)
    in
      ratToRatvec2 (a1, a2)
    end

  (* KGB indices for the short-/long-root Cartan classes in split G2 (script convention). *)
  val x_s = 4
  val x_l = 3

  (* Local parser for `atlas_group_kgb_involution_matrix_text`. Kept here for
     readability; identical intent to `AllParameters.parseInvolutionMatrixText`. *)
  fun parseInvolutionMatrixText s : Lattice.mat =
    let
      val ns = parseInts s
    in
      case ns of
        rank :: rest =>
          let
            val need = rank * rank
            val () =
              if length rest <> need then
                raise Fail "G2_unitary_dual: parseInvolutionMatrixText: bad size"
              else
                ()
            fun row i =
              List.take (List.drop (rest, i * rank), rank)
          in
            List.tabulate (rank, row)
          end
      | _ => raise Fail "G2_unitary_dual: parseInvolutionMatrixText: empty"
    end

  (* Add an integral vector to a denominator-1 rational weight. *)
  fun ratvecAddIntVec (u: {den: int, nums: int list}, v: int list) : {den: int, nums: int list} =
    if #den u <> 1 then
      raise Fail "G2_unitary_dual: ratvecAddIntVec: expected denom=1"
    else
      {den = 1, nums = ListPair.mapEq (op +) (#nums u, v)}

  (* Return one (arbitrary) parameter for `(x,gamma)`, if any exist. *)
  fun all_parameters_x_gamma_one (g: AtlasFFI.group, x: int, gamma: {den: int, nums: int list}) :
    AtlasFFI.param option =
    (case AllParameters.all_parameters_x_gamma_raw (g, x, gamma) of
       [] => NONE
     | p :: _ => SOME p)

  (* Enumerate parameters for `(x,gamma)` (may return multiple). *)
  fun all_parameters_x_gamma (g: AtlasFFI.group, x: int, gamma: {den: int, nums: int list}) :
    AtlasFFI.param list =
    AllParameters.all_parameters_x_gamma_raw (g, x, gamma)

  (* Parameters along the short-root Cartan line at `(m,v)`; raises if empty. *)
  fun p_s (g: AtlasFFI.group) (m: rat, v: rat) : AtlasFFI.param list =
    let
      val all = all_parameters_x_gamma (g, x_s, gamma_s (m, v))
      val () = if null all then raise Fail "G2_unitary_dual: p_s: not found" else ()
    in
      all
    end

  (* Parameters along the long-root Cartan line at `(m,v)`; raises if empty. *)
  fun p_l (g: AtlasFFI.group) (m: rat, v: rat) : AtlasFFI.param list =
    let
      val all = all_parameters_x_gamma (g, x_l, gamma_l (m, v))
      val () = if null all then raise Fail "G2_unitary_dual: p_l: not found" else ()
    in
      all
    end

  (* Construct the split principal series parameter (open KGB element) with
     infinitesimal character `rho+epsilon*gamma` and given `nu`.
     This mirrors the `.at` helper used for locating complementary series. *)
  fun ps (g: AtlasFFI.group) (epsilon: int, nu: {den: int, nums: int list}) : AtlasFFI.param =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
      val x_open = kgbSize - 1

      val rho = parseRatWeightText (AtlasFFI.atlas_group_rho_text g)
      val () =
        if #den rho <> 1 then
          raise Fail ("G2_unitary_dual: expected rho denominator 1, got " ^ Int.toString (#den rho))
        else
          ()
      val () =
        if length (#nums rho) <> rank then
          raise Fail "G2_unitary_dual: rho length mismatch"
        else
          ()
      val () =
        if length (#nums nu) <> rank then
          raise Fail "G2_unitary_dual: nu length mismatch"
        else
          ()

      val lambdaNums = addIntVec (#nums rho, epsilon :: List.tabulate (rank - 1, fn _ => 0))
      val p =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          (g, x_open, intsToText lambdaNums, 1, intsToText (#nums nu), #den nu)
    in
      if p = Foreign.Memory.null then
        raise Fail ("G2_unitary_dual: parameter construction failed: " ^ AtlasFFI.atlas_last_error ())
      else
        p
    end

  (* Compact string form for debugging/printing a parameter triple. *)
  fun short_format (p: AtlasFFI.param) : string =
    let
      val x = AtlasFFI.atlas_param_x p
      val lam = AtlasFFI.atlas_param_lambda_text p
      val nu = AtlasFFI.atlas_param_nu_text p
    in
      "(x=" ^ Int.toString x ^ "," ^ lam ^ "," ^ nu ^ ")"
    end

  (* Infinitesimal character coordinates (simple coroot basis) as rationals. *)
  fun coords_infchar (g: AtlasFFI.group) (p: AtlasFFI.param) : rat list =
    let
      val cors = Coordinates.parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)
      val gamma = parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)
    in
      List.map
        (fn {num, den} => {num = num, den = den})
        (Coordinates.coordsRatFromCoroots cors gamma)
    end

  (* Integer quotient `a/b` (rounded toward 0) assuming the result is integral in
     the contexts used by the scan loops. *)
  fun ratDiv (a: rat, b: rat) : int =
    let
      val a = normRat a
      val b = normRat b
      val () = if #num b = 0 then raise Fail "G2_unitary_dual: ratDiv: div by 0" else ()
      val num = #num a * #den b
      val den = #den a * #num b
      val () = if den <= 0 then raise Fail "G2_unitary_dual: ratDiv: expected positive divisor" else ()
    in
      num div den
    end

  (* Print a simple tab-separated table. *)
  fun tabulate (header: string list, rows: string list list) : unit =
    let
      fun joinRow xs = String.concatWith "\t" xs ^ "\n"
    in
      TextIO.print (joinRow header);
      List.app (fn r => TextIO.print (joinRow r)) rows
    end

  (* Extract the “conj/L” information used by the `.at` script’s report:
       - `conj` is `"true"` when the parameter is unitary on the Levi witness and
         the Levi has smaller semisimple rank than `G`
       - `L` is a short textual description of the Levi witness
     This is derived from `Induction.good_range_induced_from_first_text`. *)
  fun induced_conj_info (g: AtlasFFI.group) (p: AtlasFFI.param) : (string * string) =
    let
      val txt = Induction.good_range_induced_from_first_text (p, g)
      val parts = String.fields (fn c => c = #"|") txt
    in
      case parts of
        "0" :: _ => ("false", "")
      | "1" :: same :: unitary :: desc :: _ =>
          if same = "1" then ("false", desc) else (Bool.toString (unitary = "1"), desc)
      | _ => raise Fail ("G2_unitary_dual: unexpected induced info: " ^ txt)
    end

  (* Reproduce the “short-root Cartan line” scan table for fixed integer `m` and
     `v` range. Prints rows with unitary/FPP status and induction witness. *)
  fun test_s (m: int, v0: rat, v1: rat, step_size: rat) : unit =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val number_steps = ratDiv (subRat (v1, v0), step_size)

      fun row i =
        let
          val v = addRat (v0, mulRatInt (step_size, i))
          val out =
            (case (SOME (p_s g (ratOfInt m, v)) handle Fail _ => NONE) of
               NONE => [ratToString v, "[]", "none", "false", "false", "", ""]
             | SOME ps =>
                 let
                   val p = hd ps
                   val unitary = AtlasFFI.atlas_param_is_unitary p = 1
                   val coords = coords_infchar g p
                   val inFPP = in_fpp_rat coords
                   val (check, L) =
                     if inFPP orelse not unitary then ("", "") else induced_conj_info g p
                   val out =
                     [ ratToString v
                     , ratListToString coords
                     , short_format p
                     , Bool.toString unitary
                     , Bool.toString inFPP
                     , check
                     , L
                     ]
                 in
                   List.app AtlasFFI.atlas_param_free ps;
                   out
                 end)
        in
          out
        end

      val rows = List.tabulate (number_steps + 1, row)
      val () = tabulate (["v", "coordinates", "p", "unitary", "in FPP", "conj", "L"], rows)
      val () = AtlasFFI.atlas_group_free g
    in
      ()
    end

  (* Reproduce the “long-root Cartan line” scan table for fixed integer `m` and
     `v` range. *)
  fun test_l (m: int, v0: rat, v1: rat, step_size: rat) : unit =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val number_steps = ratDiv (subRat (v1, v0), step_size)

      fun row i =
        let
          val v = addRat (v0, mulRatInt (step_size, i))
          val out =
            (case (SOME (p_l g (ratOfInt m, v)) handle Fail _ => NONE) of
               NONE => [ratToString v, "[]", "none", "false", "false", "", ""]
             | SOME ps =>
                 let
                   val p = hd ps
                   val unitary = AtlasFFI.atlas_param_is_unitary p = 1
                   val coords = coords_infchar g p
                   val inFPP = in_fpp_rat coords
                   val (check, L) =
                     if inFPP orelse not unitary then ("", "") else induced_conj_info g p
                   val out =
                     [ ratToString v
                     , ratListToString coords
                     , short_format p
                     , Bool.toString unitary
                     , Bool.toString inFPP
                     , check
                     , L
                     ]
                 in
                   List.app AtlasFFI.atlas_param_free ps;
                   out
                 end)
        in
          out
        end

      val rows = List.tabulate (number_steps + 1, row)
      val () = tabulate (["v", "coordinates", "p", "unitary", "in FPP", "conj", "L"], rows)
      val () = AtlasFFI.atlas_group_free g
    in
      ()
    end

  (* Small smoke-demo showing how the helper constructors behave and printing a
     few diagnostics (rho, finals, etc.). *)
  fun demo () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val rho = AtlasFFI.atlas_group_rho_text g
      val () = print ("G2_s rho=" ^ rho ^ "\n")

      val p = ps g (0, {den = 1, nums = [0, 0]})
      val finals = ParamFinals.finals p
      val () = AtlasFFI.atlas_param_free p
      val () = print ("finals_for(ps) terms=" ^ Int.toString (length finals) ^ "\n")
      val () =
        List.app
          (fn (q, mult) =>
             print
               ("mult="
                ^ Int.toString mult
                ^ " gamma="
                ^ AtlasFFI.atlas_param_gamma_text q
                ^ " final="
                ^ Int.toString (AtlasFFI.atlas_param_is_final q)
                ^ " hermitian="
                ^ Int.toString (AtlasFFI.atlas_param_is_hermitian q)
                ^ " unitary="
                ^ Int.toString (AtlasFFI.atlas_param_is_unitary q)
                ^ " in_fpp="
                ^ Bool.toString (in_fpp_param g q)
                ^ "\n"))
          finals

      val gs = gamma_s ({num = 2, den = 1}, {num = 0, den = 1})
      val gl = gamma_l ({num = 2, den = 1}, {num = 0, den = 1})

      val () =
        let
          val ps = all_parameters_x_gamma (g, x_s, gs)
        in
          case ps of
            [] => print "p_s: none\n"
          | q :: rest =>
              (print ("p_s.count=" ^ Int.toString (length ps) ^ " gamma=" ^ AtlasFFI.atlas_param_gamma_text q
                      ^ " final=" ^ Int.toString (AtlasFFI.atlas_param_is_final q)
                      ^ " unitary=" ^ Int.toString (AtlasFFI.atlas_param_is_unitary q) ^ "\n");
               List.app AtlasFFI.atlas_param_free (q :: rest))
        end

      val () =
        let
          val ps = all_parameters_x_gamma (g, x_l, gl)
        in
          case ps of
            [] => print "p_l: none\n"
          | q :: rest =>
              (print ("p_l.count=" ^ Int.toString (length ps) ^ " gamma=" ^ AtlasFFI.atlas_param_gamma_text q
                      ^ " final=" ^ Int.toString (AtlasFFI.atlas_param_is_final q)
                      ^ " unitary=" ^ Int.toString (AtlasFFI.atlas_param_is_unitary q) ^ "\n");
               List.app AtlasFFI.atlas_param_free (q :: rest))
        end

      val () = ParamFinals.freeTerms finals
      val () = AtlasFFI.atlas_group_free g
    in
      ()
    end
end

(* Intentionally no toplevel side-effects; run `G2_unitary_dual.demo()` from a driver. *)
