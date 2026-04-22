use "atlas-scripts-sml/BigUnitaryHash.sml";
use "atlas-scripts-sml/ParamHash.sml";

structure F4_FPP_points = struct
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("F4_FPP_points: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun rstripNewlines s =
    let
      val n = String.size s
    in
      if n = 0 then s
      else
        case String.sub (s, n - 1) of
          #"\n" => rstripNewlines (String.substring (s, 0, n - 1))
        | #"\r" => rstripNewlines (String.substring (s, 0, n - 1))
        | _ => s
    end

  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  fun intsToText xs =
    String.concatWith " " (List.map intToCText xs)

  fun loadInto (g: AtlasFFI.group, (hash: BigUnitaryHash.t)) =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_points.txt"
      val rows = ref 0

      fun handleRow ns =
        (case ns of
           [x, lamDen, l1, l2, l3, l4, nuDen, n1, n2, n3, n4] =>
             let
               val expectedLam = Int.toString lamDen ^ " " ^ intsToText [l1, l2, l3, l4]
               val expectedNu = Int.toString nuDen ^ " " ^ intsToText [n1, n2, n3, n4]
               val p =
                 AtlasFFI.atlas_param_new_from_lambda_nu_text
                   ( g
                   , x
                   , intsToText [l1, l2, l3, l4]
                   , lamDen
                   , intsToText [n1, n2, n3, n4]
                   , nuDen
                   )
             in
               if p = Foreign.Memory.null then
                 raise Fail ("F4_FPP_points: param construction failed: " ^ AtlasFFI.atlas_last_error ())
               else
                 let
                   val () = rows := !rows + 1
                   val gotX = AtlasFFI.atlas_param_x p
                   val gotLam = AtlasFFI.atlas_param_lambda_text p
                   val gotNu = AtlasFFI.atlas_param_nu_text p
                   val () =
                     if gotX <> x then
                       raise Fail ("F4_FPP_points: x mismatch: expected=" ^ Int.toString x ^ " got=" ^ Int.toString gotX)
                     else
                       ()
                   val () =
                     if gotLam <> expectedLam then
                       raise Fail ("F4_FPP_points: lambda mismatch: expected=" ^ expectedLam ^ " got=" ^ gotLam)
                     else
                       ()
                   val () =
                     if gotNu <> expectedNu then
                       raise Fail ("F4_FPP_points: nu mismatch: expected=" ^ expectedNu ^ " got=" ^ gotNu)
                     else
                       ()
                 in
                   if BigUnitaryHash.insert hash p then
                     ()
                   else
                     (AtlasFFI.atlas_param_free p;
                      raise Fail ("F4_FPP_points: duplicate parameter in data file at row " ^ Int.toString (!rows)))
                 end
             end
         | _ => raise Fail "F4_FPP_points: unexpected row length")

      fun loop () =
        case TextIO.inputLine input of
          NONE => ()
        | SOME line =>
            let
              val s = rstripNewlines line
            in
              if s = "" orelse (String.size s > 0 andalso String.sub (s, 0) = #"#")
              then loop ()
              else (handleRow (parseInts s); loop ())
            end
    in
      (loop (); TextIO.closeIn input) handle e => (TextIO.closeIn input; raise e)
    end

  fun loadIntoParamHash (g: AtlasFFI.group, (hash: ParamHash.t)) =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_points.txt"
      val rows = ref 0

      fun handleRow ns =
        (case ns of
           [x, lamDen, l1, l2, l3, l4, nuDen, n1, n2, n3, n4] =>
             let
               val expectedLam = Int.toString lamDen ^ " " ^ intsToText [l1, l2, l3, l4]
               val expectedNu = Int.toString nuDen ^ " " ^ intsToText [n1, n2, n3, n4]
               val p =
                 AtlasFFI.atlas_param_new_from_lambda_nu_text
                   ( g
                   , x
                   , intsToText [l1, l2, l3, l4]
                   , lamDen
                   , intsToText [n1, n2, n3, n4]
                   , nuDen
                   )
             in
               if p = Foreign.Memory.null then
                 raise Fail ("F4_FPP_points: param construction failed: " ^ AtlasFFI.atlas_last_error ())
               else
                 let
                   val () = rows := !rows + 1
                   val gotX = AtlasFFI.atlas_param_x p
                   val gotLam = AtlasFFI.atlas_param_lambda_text p
                   val gotNu = AtlasFFI.atlas_param_nu_text p
                   val () =
                     if gotX <> x then
                       raise Fail ("F4_FPP_points: x mismatch: expected=" ^ Int.toString x ^ " got=" ^ Int.toString gotX)
                     else
                       ()
                   val () =
                     if gotLam <> expectedLam then
                       raise Fail ("F4_FPP_points: lambda mismatch: expected=" ^ expectedLam ^ " got=" ^ gotLam)
                     else
                       ()
                   val () =
                     if gotNu <> expectedNu then
                       raise Fail ("F4_FPP_points: nu mismatch: expected=" ^ expectedNu ^ " got=" ^ gotNu)
                     else
                       ()
                   val already = ParamHash.lookup hash p >= 0
                   val () = ignore (ParamHash.match hash p)
                   val () = AtlasFFI.atlas_param_free p
                 in
                   if already then
                     raise Fail ("F4_FPP_points: duplicate parameter in data file at row " ^ Int.toString (!rows))
                   else
                     ()
                 end
             end
         | _ => raise Fail "F4_FPP_points: unexpected row length")

      fun loop () =
        case TextIO.inputLine input of
          NONE => ()
        | SOME line =>
            let
              val s = rstripNewlines line
            in
              if s = "" orelse (String.size s > 0 andalso String.sub (s, 0) = #"#")
              then loop ()
              else (handleRow (parseInts s); loop ())
            end
    in
      (loop (); TextIO.closeIn input) handle e => (TextIO.closeIn input; raise e)
    end
end
