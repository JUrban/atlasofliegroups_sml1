use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";

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

  fun parseSimpleCorootsText s : int list list =
    let
      val ns = parseInts s
    in
      case ns of
        ssRank :: rank :: rest =>
          let
            fun takeVec (0, xs, acc) = (List.rev acc, xs)
              | takeVec (n, x :: xs, acc) = takeVec (n - 1, xs, x :: acc)
              | takeVec _ = raise Fail "G2_unitary_dual: parseSimpleCorootsText: truncated"
            fun loop (0, xs, acc) = (List.rev acc, xs)
              | loop (k, xs, acc) =
                  let
                    val (v, xs') = takeVec (rank, xs, [])
                  in
                    loop (k - 1, xs', v :: acc)
                  end
            val (cors, leftover) = loop (ssRank, rest, [])
          in
            if null leftover then cors else raise Fail "G2_unitary_dual: parseSimpleCorootsText: extra ints"
          end
      | _ => raise Fail ("G2_unitary_dual: parseSimpleCorootsText: bad header: " ^ s)
    end

  fun coordsFromCoroots (coroots: int list list) (w: {den: int, nums: int list}) : int list =
    let
      val den = #den w
      val nums = #nums w
      val () = if den <= 0 then raise Fail "G2_unitary_dual: coords: non-positive denom" else ()
    in
      List.map (fn cor => dot (cor, nums)) coroots
    end

  fun in_fpp_coords (den: int, evals: int list) : bool =
    List.all (fn e => 0 <= e andalso e <= den) evals

  fun in_fpp_rat (xs: rat list) : bool =
    let
      fun leq (a: rat, b: rat) =
        #num a * #den b <= #num b * #den a
      val zero = ratOfInt 0
      val one = ratOfInt 1
    in
      List.all (fn x => leq (zero, x) andalso leq (x, one)) xs
    end

  fun in_fpp_param (g: AtlasFFI.group) (p: AtlasFFI.param) : bool =
    let
      val cors = parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)
      val gamma = parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)
      val evals = coordsFromCoroots cors gamma
    in
      in_fpp_coords (#den gamma, evals)
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

  fun coordsRatFromCoroots (coroots: int list list) (w: {den: int, nums: int list}) : rat list =
    let
      val den = #den w
      val nums = #nums w
      val () = if den <= 0 then raise Fail "G2_unitary_dual: coordsRat: non-positive denom" else ()
    in
      List.map (fn cor => normRat {num = dot (cor, nums), den = den}) coroots
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
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val th1 = Lattice.matAdd (Lattice.identity rank, theta)
      val rho = parseRatWeightText (AtlasFFI.atlas_group_rho_text g)
      val u = Lattice.matVecMulRatvec th1 (Lattice.ratvecSub (gamma, rho))
      val lrOpt = Lattice.vec_solve (th1, u)
    in
      case lrOpt of
        NONE => NONE
      | SOME lr =>
          let
            val lambda = ratvecAddIntVec (rho, lr)
            val oneMinusTheta =
              ListPair.mapEq
                (fn (ri, ti) => ListPair.mapEq (op -) (ri, ti))
                (Lattice.identity rank, theta)
            val nu = Lattice.ratvecScale (Lattice.matVecMulRatvec oneMinusTheta gamma, 1, 2)
            val p =
              AtlasFFI.atlas_param_new_from_lambda_nu_text
                (g, x, intsToCText (#nums lambda), #den lambda, intsToCText (#nums nu), #den nu)
          in
            if p = Foreign.Memory.null then
              raise Fail ("G2_unitary_dual: parameter construction failed: " ^ AtlasFFI.atlas_last_error ())
            else
              let
                val q = AtlasFFI.atlas_param_normalise p
                val () = AtlasFFI.atlas_param_free p
              in
                if q = Foreign.Memory.null then
                  raise Fail ("G2_unitary_dual: normalise failed: " ^ AtlasFFI.atlas_last_error ())
                else
                  let
                    val finals = ParamFinals.finals q
                    val () = AtlasFFI.atlas_param_free q
                  in
                    case finals of
                      [] => raise Fail "G2_unitary_dual: finals_for returned empty list"
                    | (f, _) :: rest =>
                        (ParamFinals.freeTerms rest;
                         SOME f)
                  end
              end
          end
    end

  fun all_parameters_x_gamma (g: AtlasFFI.group, x: int, gamma: {den: int, nums: int list}) :
    AtlasFFI.param list =
    let
      fun addUnique (p: AtlasFFI.param, acc: AtlasFFI.param list) : AtlasFFI.param list =
        if List.exists (fn q => AtlasFFI.atlas_param_equivalent (p, q) = 1) acc then
          (AtlasFFI.atlas_param_free p; acc)
        else
          p :: acc

      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val th1 = Lattice.matAdd (Lattice.identity rank, theta)
      val rho = parseRatWeightText (AtlasFFI.atlas_group_rho_text g)
      val u = Lattice.matVecMulRatvec th1 (Lattice.ratvecSub (gamma, rho))
      val lrOpt = Lattice.vec_solve (th1, u)
    in
      case lrOpt of
        NONE => []
      | SOME lr =>
          let
            val lambda = ratvecAddIntVec (rho, lr)
            val oneMinusTheta =
              ListPair.mapEq
                (fn (ri, ti) => ListPair.mapEq (op -) (ri, ti))
                (Lattice.identity rank, theta)
            val nu = Lattice.ratvecScale (Lattice.matVecMulRatvec oneMinusTheta gamma, 1, 2)

            val twists = LambdaDifferential0.all (g, x)

            fun addTwist (v: int list, acc: AtlasFFI.param list) =
              let
                val lambda2 = ratvecAddIntVec (lambda, v)
                val p0 =
                  AtlasFFI.atlas_param_new_from_lambda_nu_text
                    (g, x, intsToCText (#nums lambda2), #den lambda2, intsToCText (#nums nu), #den nu)
                val () =
                  if p0 = Foreign.Memory.null then
                    raise Fail ("G2_unitary_dual: parameter construction failed: " ^ AtlasFFI.atlas_last_error ())
                  else
                    ()
                val p1 = AtlasFFI.atlas_param_normalise p0
                val () = AtlasFFI.atlas_param_free p0
                val () =
                  if p1 = Foreign.Memory.null then
                    raise Fail ("G2_unitary_dual: normalise failed: " ^ AtlasFFI.atlas_last_error ())
                  else
                    ()
                val finals = ParamFinals.finals p1
                val () = AtlasFFI.atlas_param_free p1

                fun addFinal ((q, mult), acc2) =
                  if mult = 0 then
                    (AtlasFFI.atlas_param_free q; acc2)
                  else
                    let
                      val q1 = AtlasFFI.atlas_param_normalise q
                      val () = AtlasFFI.atlas_param_free q
                      val () =
                        if q1 = Foreign.Memory.null then
                          raise Fail ("G2_unitary_dual: normalise(final) failed: " ^ AtlasFFI.atlas_last_error ())
                        else
                          ()
                    in
                      addUnique (q1, acc2)
                    end
              in
                List.foldl addFinal acc finals
              end
          in
            List.rev (List.foldl addTwist [] twists)
          end
    end

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
      val cors = parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)
      val gamma = parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)
    in
      coordsRatFromCoroots cors gamma
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
                   val unitary = AtlasFFI.atlas_param_is_unitary_c_form p = 1
                   val coords = coords_infchar g p
                   val inFPP = in_fpp_rat coords
                   val check = "" (* TODO: is_good_range_induced_from *)
                   val L = "" (* TODO: Levi real form *)
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
                   val unitary = AtlasFFI.atlas_param_is_unitary_c_form p = 1
                   val coords = coords_infchar g p
                   val inFPP = in_fpp_rat coords
                   val check = "" (* TODO: is_good_range_induced_from *)
                   val L = "" (* TODO: Levi real form *)
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
                ^ " unitary_c_form="
                ^ Int.toString (AtlasFFI.atlas_param_is_unitary_c_form q)
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
                      ^ " unitary=" ^ Int.toString (AtlasFFI.atlas_param_is_unitary_c_form q) ^ "\n");
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
                      ^ " unitary=" ^ Int.toString (AtlasFFI.atlas_param_is_unitary_c_form q) ^ "\n");
               List.app AtlasFFI.atlas_param_free (q :: rest))
        end

      val () = ParamFinals.freeTerms finals
      val () = AtlasFFI.atlas_group_free g
    in
      ()
    end
end

val () = G2_unitary_dual.demo ();
