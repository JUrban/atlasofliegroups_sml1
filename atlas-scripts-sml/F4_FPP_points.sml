use "atlas-scripts-sml/BigUnitaryHash.sml";

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
    if n < 0 then "-" ^ Int.toString (~n) else Int.toString n

  fun intsToText xs =
    String.concatWith " " (List.map intToCText xs)

  fun loadInto (g: AtlasFFI.group, (hash: BigUnitaryHash.t)) =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_points.txt"

      fun handleRow ns =
        (case ns of
           [x, lamDen, l1, l2, l3, l4, nuDen, n1, n2, n3, n4] =>
             let
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
                 (if BigUnitaryHash.insert hash p
                  then ()
                  else AtlasFFI.atlas_param_free p)
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
