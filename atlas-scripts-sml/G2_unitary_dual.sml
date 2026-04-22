use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Coordinates.sml";

structure G2_unitary_dual = struct
  type rat = {num: int, den: int}

  fun ratOfInt n : rat = {num = n, den = 1}

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("G2_unitary_dual: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun parseRatWeightText s : {den: int, nums: int list} =
    (case parseInts s of
       den :: rest => {den = den, nums = rest}
     | _ => raise Fail ("G2_unitary_dual: bad ratweight text: " ^ s))

  fun intsToText xs =
    String.concatWith " " (List.map Int.toString xs)

  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  fun intsToCText xs =
    String.concatWith " " (List.map intToCText xs)

  fun ratWeightToText {den, nums} =
    Int.toString den ^ " " ^ intsToText nums

  fun addIntVec (xs: int list, ys: int list) =
    let
      fun loop ([], [], acc) = List.rev acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', (a + b) :: acc)
        | loop _ = raise Fail "G2_unitary_dual: addIntVec: length mismatch"
    in
      loop (xs, ys, [])
    end

  fun dot (xs: int list, ys: int list) : int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "G2_unitary_dual: dot: length mismatch"
    in
      loop (xs, ys, 0)
    end

  fun in_fpp_rat (xs: rat list) : bool = Coordinates.in_fpp_rat xs

  fun in_fpp_param (g: AtlasFFI.group) (p: AtlasFFI.param) : bool =
    let
      val gamma = Coordinates.parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)
      val coords = Coordinates.coordsRat g gamma
    in
      Coordinates.in_fpp_rat coords
    end

  fun gcd (a: int, b: int) : int =
    let
      val a = Int.abs a
      val b = Int.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, x mod y)
    in
      if a = 0 then b else loop (a, b)
    end

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

  fun addRat (a: rat, b: rat) : rat =
    normRat {num = #num a * #den b + #num b * #den a, den = #den a * #den b}

  fun subRat (a: rat, b: rat) : rat =
    normRat {num = #num a * #den b - #num b * #den a, den = #den a * #den b}

  fun mulRatInt (a: rat, k: int) : rat =
    normRat {num = #num a * k, den = #den a}

  fun divRatInt (a: rat, k: int) : rat =
    if k = 0 then raise Fail "G2_unitary_dual: divRatInt: div by 0"
    else normRat {num = #num a, den = #den a * k}

  fun ratToString (a: rat) : string =
    let
      val a = normRat a
    in
      if #den a = 1 then Int.toString (#num a)
      else Int.toString (#num a) ^ "/" ^ Int.toString (#den a)
    end

  fun ratListToString (xs: rat list) : string =
    "[" ^ String.concatWith "," (List.map ratToString xs) ^ "]"

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

  fun gamma_s (m: rat, v: rat) : {den: int, nums: int list} =
    let
      val v1 = v
      val v2 = divRatInt (subRat (m, v), 2)
    in
      ratToRatvec2 (v1, v2)
    end

  fun gamma_l (m: rat, v: rat) : {den: int, nums: int list} =
    let
      val a1 = divRatInt (addRat (m, mulRatInt (v, 3)), 2)
      val a2 = mulRatInt (v, ~1)
    in
      ratToRatvec2 (a1, a2)
    end

  val x_s = 4
  val x_l = 3

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

  fun ratvecAddIntVec (u: {den: int, nums: int list}, v: int list) : {den: int, nums: int list} =
    if #den u <> 1 then
      raise Fail "G2_unitary_dual: ratvecAddIntVec: expected denom=1"
    else
      {den = 1, nums = ListPair.mapEq (op +) (#nums u, v)}

  fun all_parameters_x_gamma_one (g: AtlasFFI.group, x: int, gamma: {den: int, nums: int list}) :
    AtlasFFI.param option =
    (case AllParameters.all_parameters_x_gamma_raw (g, x, gamma) of
       [] => NONE
     | p :: _ => SOME p)

  fun all_parameters_x_gamma (g: AtlasFFI.group, x: int, gamma: {den: int, nums: int list}) :
    AtlasFFI.param list =
    AllParameters.all_parameters_x_gamma_raw (g, x, gamma)

  fun p_s (g: AtlasFFI.group) (m: rat, v: rat) : AtlasFFI.param list =
    let
      val all = all_parameters_x_gamma (g, x_s, gamma_s (m, v))
      val () = if null all then raise Fail "G2_unitary_dual: p_s: not found" else ()
    in
      all
    end

  fun p_l (g: AtlasFFI.group) (m: rat, v: rat) : AtlasFFI.param list =
    let
      val all = all_parameters_x_gamma (g, x_l, gamma_l (m, v))
      val () = if null all then raise Fail "G2_unitary_dual: p_l: not found" else ()
    in
      all
    end

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

  fun short_format (p: AtlasFFI.param) : string =
    let
      val x = AtlasFFI.atlas_param_x p
      val lam = AtlasFFI.atlas_param_lambda_text p
      val nu = AtlasFFI.atlas_param_nu_text p
    in
      "(x=" ^ Int.toString x ^ "," ^ lam ^ "," ^ nu ^ ")"
    end

  fun coords_infchar (g: AtlasFFI.group) (p: AtlasFFI.param) : rat list =
    let
      val cors = Coordinates.parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)
      val gamma = parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)
    in
      List.map
        (fn {num, den} => {num = num, den = den})
        (Coordinates.coordsRatFromCoroots cors gamma)
    end

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

  fun tabulate (header: string list, rows: string list list) : unit =
    let
      fun joinRow xs = String.concatWith "\t" xs ^ "\n"
    in
      TextIO.print (joinRow header);
      List.app (fn r => TextIO.print (joinRow r)) rows
    end

  fun induced_conj_info (p: AtlasFFI.param) : (string * string) =
    let
      val txt = AtlasFFI.atlas_param_good_range_induced_from_first_text p
      val parts = String.fields (fn c => c = #"|") txt
    in
      case parts of
        "0" :: _ => ("false", "")
      | "1" :: same :: unitary :: desc :: _ =>
          if same = "1" then ("false", desc) else (Bool.toString (unitary = "1"), desc)
      | _ => raise Fail ("G2_unitary_dual: unexpected induced info: " ^ txt)
    end

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
                     if inFPP orelse not unitary then ("", "") else induced_conj_info p
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
                     if inFPP orelse not unitary then ("", "") else induced_conj_info p
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
