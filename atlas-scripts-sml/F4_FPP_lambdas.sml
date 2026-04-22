(* 
  File: atlas-scripts-sml/F4_FPP_lambdas.sml

  Purpose
  - Load the precomputed `F4_FPP_lambdas` table from the fixture file
    `atlas-scripts-sml/data/F4_FPP_lambdas.txt`.

  Notes
  - The modern pipeline computes lambdas via `FPP_lambdas_fold`; this fixture is
    retained for debugging/regression comparisons.
*)
structure F4_FPP_lambdas = struct
  type ratvec_text = {numsText: string, denom: int}

  (* Parse whitespace-separated integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("F4_FPP_lambdas: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Strip trailing newline/CR from a line. *)
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

  (* Convert SML `~` negatives to C-style `-` negatives. *)
  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  (* Serialize an int list in Atlas C++ parser format. *)
  fun intsToText xs =
    String.concatWith " " (List.map intToCText xs)

  (* Load the lambda table as an array indexed by KGB index `x`. *)
  fun load (kgbSize: int) : ratvec_text list array =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_lambdas.txt"
      val buckets = Array.array (kgbSize, ([]: ratvec_text list))

      (* Parse one row and append it to the appropriate bucket. *)
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
