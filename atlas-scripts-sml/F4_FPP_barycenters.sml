(* 
  File: atlas-scripts-sml/F4_FPP_barycenters.sml

  Purpose
  - Load the precomputed list of F4 folded-FPP barycenters from the fixture file
    `atlas-scripts-sml/data/F4_FPP_barycenters.txt`.

  Notes
  - This fixture predates the fully computed folded-FPP barycenter generator and
    is kept for debugging and cross-checks.
*)
use "atlas-scripts-sml/Lattice.sml";
structure F4_FPP_barycenters = struct
  type ratvec = Lattice.ratvec
  type ratvec_text = {numsText: string, denom: int}

  (* Parse whitespace-separated integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("F4_FPP_barycenters: bad int token: " ^ tok)
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

  (* Load all barycenters from disk. *)
  fun load () : ratvec_text list =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_barycenters.txt"

      (* Parse one data row into `{denom, numsText}`. *)
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

  (*
    Load all barycenters from disk as normalized `Lattice.ratvec` values.

    This is the representation used by the folded-FPP code (`FPP_barycenters_fold`)
    and is suitable for high-level algorithms like `FPP_localDirac.create_ctx`.
  *)
  fun loadRatvecs () : ratvec list =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_barycenters.txt"

      fun handleRow line =
        (case parseInts line of
           [den, a, b, c, d] => Lattice.ratvecNormalize {den = den, nums = [a, b, c, d]}
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
