structure F4_FPP_lambdas = struct
  type ratvec_text = {numsText: string, denom: int}

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("F4_FPP_lambdas: bad int token: " ^ tok)
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

  fun load (kgbSize: int) : ratvec_text list array =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_lambdas.txt"
      val buckets = Array.array (kgbSize, ([]: ratvec_text list))

      fun handleRow line =
        (case parseInts line of
           [x, den, a, b, c, d] =>
             if x < 0 orelse x >= kgbSize then
               raise Fail ("F4_FPP_lambdas: x out of range: " ^ Int.toString x)
             else
               Array.update
                 ( buckets
                 , x
                 , {numsText = intsToText [a, b, c, d], denom = den} :: Array.sub (buckets, x)
                 )
         | _ => raise Fail ("F4_FPP_lambdas: unexpected row: " ^ line))

      fun loop () =
        case TextIO.inputLine input of
          NONE => ()
        | SOME line =>
            let
              val s = rstripNewlines line
            in
              if s = "" orelse (String.size s > 0 andalso String.sub (s, 0) = #"#")
              then loop ()
              else (handleRow s; loop ())
            end

      val () = (loop (); TextIO.closeIn input) handle e => (TextIO.closeIn input; raise e)
      val result = Array.tabulate (kgbSize, fn i => List.rev (Array.sub (buckets, i)))
    in
      result
    end
end

