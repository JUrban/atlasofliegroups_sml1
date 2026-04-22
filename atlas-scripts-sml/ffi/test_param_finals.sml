use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamFinals.sml";

structure TestParamFinals = struct
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("test_param_finals: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun intsToCText xs =
    let
      fun intToCText n =
        let
          val s = Int.toString n
        in
          if String.size s > 0 andalso String.sub (s, 0) = #"~" then
            "-" ^ String.extract (s, 1, NONE)
          else
            s
        end
    in
      String.concatWith " " (List.map intToCText xs)
    end

  fun rhoNums (g: AtlasFFI.group) : int list =
    (case parseInts (AtlasFFI.atlas_group_rho_text g) of
       den :: nums =>
         if den <> 1 then
           raise Fail ("test_param_finals: expected rho denom=1, got " ^ Int.toString den)
         else
           nums
     | _ => raise Fail "test_param_finals: bad rho text")

  fun zeros n = List.tabulate (n, fn _ => 0)

  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
      val rank = AtlasFFI.atlas_group_rank g
      val rho = rhoNums g
      val () = if length rho <> rank then raise Fail "test_param_finals: rho length mismatch" else ()

      val pFinal = AtlasFFI.atlas_param_trivial g
      val finals0 = ParamFinals.finals pFinal
      val () =
        if length finals0 <> 1 then
          raise Fail ("test_param_finals: expected 1 final term for trivial, got " ^ Int.toString (length finals0))
        else
          ()
      val (q0, m0) = hd finals0
      val () = if m0 <> 1 then raise Fail "test_param_finals: expected multiplicity 1 for trivial" else ()
      val () =
        if AtlasFFI.atlas_param_equal (pFinal, q0) <> 1 then
          raise Fail "test_param_finals: trivial param not equal to its finals_for term"
        else
          ()
      val () = AtlasFFI.atlas_param_free pFinal
      val () = ParamFinals.freeTerms finals0

      val lambdaNums =
        (case rho of
           a :: rest => (a - 10) :: rest
         | _ => raise Fail "test_param_finals: empty rho")
      val nuNums = zeros rank
      val p =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          (g, 0, intsToCText lambdaNums, 1, intsToCText nuNums, 1)
      val () =
        if p = Foreign.Memory.null then
          raise Fail ("test_param_finals: param construction failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()

      val () =
        if AtlasFFI.atlas_param_is_final p = 1 then
          raise Fail "test_param_finals: expected constructed param to be non-final"
        else
          ()

      val finals = ParamFinals.finals p
      val () = if null finals then raise Fail "test_param_finals: finals_for returned empty list" else ()
      val () =
        List.app
          (fn (q, _) =>
             if AtlasFFI.atlas_param_is_final q <> 1 then
               raise Fail "test_param_finals: finals_for term is not final"
             else
               ())
          finals

      val () = AtlasFFI.atlas_param_free p
      val () = ParamFinals.freeTerms finals
      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestParamFinals.run ();

