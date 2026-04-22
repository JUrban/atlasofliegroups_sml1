use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure G2_unitary_dual = struct
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

  fun in_fpp_param (g: AtlasFFI.group) (p: AtlasFFI.param) : bool =
    let
      val cors = parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)
      val gamma = parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)
      val evals = coordsFromCoroots cors gamma
    in
      in_fpp_coords (#den gamma, evals)
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

  fun demo () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val rho = AtlasFFI.atlas_group_rho_text g
      val () = print ("G2_s rho=" ^ rho ^ "\n")

      val p = ps g (0, {den = 1, nums = [0, 0]})
      val () = print ("p.gamma=" ^ AtlasFFI.atlas_param_gamma_text p ^ "\n")
      val () = print ("p.lambda=" ^ AtlasFFI.atlas_param_lambda_text p ^ "\n")
      val () = print ("p.nu=" ^ AtlasFFI.atlas_param_nu_text p ^ "\n")
      val () = print ("hermitian=" ^ Int.toString (AtlasFFI.atlas_param_is_hermitian p) ^ "\n")
      val () = print ("unitary_c_form=" ^ Int.toString (AtlasFFI.atlas_param_is_unitary_c_form p) ^ "\n")
      val () = print ("in_fpp=" ^ Bool.toString (in_fpp_param g p) ^ "\n")

      val () = AtlasFFI.atlas_param_free p
      val () = AtlasFFI.atlas_group_free g
    in
      ()
    end
end

val () = G2_unitary_dual.demo ();
