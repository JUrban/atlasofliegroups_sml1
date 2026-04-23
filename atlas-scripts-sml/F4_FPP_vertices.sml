(* 
  File: atlas-scripts-sml/F4_FPP_vertices.sml

  Purpose
  - Load the precomputed list of folded-FPP vertices for split F4 from the
    fixture file `atlas-scripts-sml/data/F4_FPP_vertices.txt`.

  Why this exists
  - Computing folded-FPP vertices via affine-orbit enumeration is relatively
    expensive. Several higher-level algorithms (notably `FPP_localDirac`) need
    the global vertex table but do not otherwise depend on Atlas’ `.at`
    interpreter. Keeping this fixture in the repo allows fast startup and
    fully-SML execution.

  Format
  - One vertex per line as five integers:
        den  n1  n2  n3  n4
    representing the rational vector `(n1,n2,n3,n4)/den`.
*)
use "atlas-scripts-sml/Lattice.sml";

structure F4_FPP_vertices = struct
  type ratvec = Lattice.ratvec

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("F4_FPP_vertices: bad int token: " ^ tok)
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

  fun loadRatvecs () : ratvec list =
    let
      val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_vertices.txt"

      fun handleRow line =
        (case parseInts line of
           [den, a, b, c, d] => Lattice.ratvecNormalize {den = den, nums = [a, b, c, d]}
         | _ => raise Fail ("F4_FPP_vertices: unexpected row: " ^ line))

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

