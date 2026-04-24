use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/StringListData.sml

  Purpose
  - Utility for loading lists of strings from repo-local fixtures.
  - Mirrors the comment/blank-line conventions used by `IntListData`.

  Format
  - One string per non-empty, non-comment line.
  - Lines beginning with `#` are ignored.
  - Trailing `\\r`/`\\n` is stripped.
*)

structure StringListData = struct
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

  fun loadLines (path: string) : string list =
    let
      val input = TextIO.openIn path
      fun loop acc =
        case TextIO.inputLine input of
          NONE => List.rev acc
        | SOME line =>
            let
              val s = rstripNewlines line
            in
              if s = "" orelse isCommentLine s then loop acc else loop (s :: acc)
            end
      val result = loop []
    in
      (TextIO.closeIn input; result) handle e => (TextIO.closeIn input; raise e)
    end
end

