structure F4_FPP_barycenters = struct
  type ratvec_text = {numsText: string, denom: int}

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("F4_FPP_barycenters: bad int token: " ^ tok)
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

  fun load () : ratvec_text list =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_barycenters.txt"

      fun handleRow line =
        (case parseInts line of
           [den, a, b, c, d] => {numsText = intsToText [a, b, c, d], denom = den}
         | _ => raise Fail ("F4_FPP_barycenters: unexpected row: " ^ line))

      fun loop acc =
        case TextIO.inputLine input of
          NONE => List.rev acc
        | SOME line =>
            let
              val s = rstripNewlines line
            in
              if s = "" orelse (String.size s > 0 andalso String.sub (s, 0) = #"#")
              then loop acc
              else loop (handleRow s :: acc)
            end
      val result = loop []
    in
      (TextIO.closeIn input; result) handle e => (TextIO.closeIn input; raise e)
    end
end
