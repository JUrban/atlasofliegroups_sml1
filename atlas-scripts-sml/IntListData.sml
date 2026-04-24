use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/IntListData.sml

  Purpose
  - Small utility for loading large integer lists (and lists-of-lists) from
    repo-local text fixtures under `atlas-scripts-sml/data/`.
  - This is used for ports of `.at` scripts that are *pure data*, where the
    original `.at` file contains a huge literal like:
        set cells = [[...],[...],...]
    and we want an SML port that:
      - does not depend on the Atlas interpreter, and
      - does not embed multi-megabyte literals directly in SML source.

  Format
  - One list per line, space-separated integers.
  - Blank lines are ignored.
  - Lines beginning with `#` are treated as comments and ignored.
*)

structure IntListData = struct
  fun parseInt tok =
    case Int.fromString tok of
      SOME n => n
    | NONE => raise Fail ("IntListData: bad int token: " ^ tok)

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

  fun isCommentLine s =
    String.size s > 0 andalso String.sub (s, 0) = #"#"

  fun sanitize (s: string) : string =
    let
      fun mapc c =
        if c = #"," orelse c = #"[" orelse c = #"]" then
          #" "
        else
          c
    in
      String.implode (List.map mapc (String.explode s))
    end

  fun parseLineInts (line: string) : int list =
    List.map parseInt (String.tokens Char.isSpace (sanitize line))

  fun loadIntLists (path: string) : int list list =
    let
      val input = TextIO.openIn path

      fun loop acc =
        case TextIO.inputLine input of
          NONE => List.rev acc
        | SOME line =>
            let
              val s = rstripNewlines line
            in
              if s = "" orelse isCommentLine s then
                loop acc
              else
                loop (parseLineInts s :: acc)
            end
      val result = loop []
    in
      (TextIO.closeIn input; result) handle e => (TextIO.closeIn input; raise e)
    end

  (* Load a flat list of integers from a fixture file. This accepts either:
       - one integer per line, or
       - multiple integers per line.
     (comment/blank lines are ignored as in `loadIntLists`). *)
  fun loadInts (path: string) : int list =
    List.concat (loadIntLists path)
end
