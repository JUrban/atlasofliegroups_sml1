use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/tabulate.sml

  Purpose
  - SML translation of `atlas-scripts/tabulate.at`.
  - Pretty-print a rectangular string table with per-column alignments.

  API
  - `Tabulate.tabulate(data, alignments, cellPadding, padChars)`
      prints `data` row-by-row to stdout.
  - `Tabulate.tabulateDefault(data)` uses left alignment, padding=2, padChars=" ".
*)

structure Tabulate = struct
  fun mapi f xs =
    let
      fun loop ([], _, acc) = List.rev acc
        | loop (x :: rest, i, acc) = loop (rest, i + 1, f (i, x) :: acc)
    in
      loop (xs, 0, [])
    end

  fun repeatChar (c: char, n: int) : string =
    if n <= 0 then "" else String.implode (List.tabulate (n, fn _ => c))

  fun l_adjust (w: int, s: string, padChar: char) : string =
    let
      val d = w - String.size s
    in
      if d <= 0 then s else s ^ repeatChar (padChar, d)
    end

  fun r_adjust (w: int, s: string, padChar: char) : string =
    let
      val d = w - String.size s
    in
      if d <= 0 then s else repeatChar (padChar, d) ^ s
    end

  fun c_adjust (w: int, s: string, padChar: char) : string =
    let
      val d = w - String.size s
    in
      if d <= 0 then
        s
      else
        let
          val h = d div 2
        in
          repeatChar (padChar, h) ^ s ^ repeatChar (padChar, d - h)
        end
    end

  fun pad (s: string, alignment: char, width: int, padChar: char) : string =
    (case alignment of
       #"l" => l_adjust (width, s, padChar)
     | #"r" => r_adjust (width, s, padChar)
     | _ => c_adjust (width, s, padChar))

  fun strip_trailing_spaces (str: string) : string =
    let
      val l = String.size str
      fun lastNonSpace i =
        if i < 0 then ~1
        else if String.sub (str, i) <> #" " then i else lastNonSpace (i - 1)
      val idx = lastNonSpace (l - 1) + 1
    in
      if idx = l then str else String.substring (str, 0, Int.max (0, idx))
    end

  fun tabulate (data: string list list, alignments: string, cellPadding: int, padChars: string) : unit =
    let
      val n_cols = String.size alignments
      val () = if n_cols = 0 then raise Fail "Tabulate.tabulate: empty alignments" else ()
      val () = if String.size padChars = 0 then raise Fail "Tabulate.tabulate: empty padChars" else ()

      fun rowWidth row =
        if length row = n_cols then () else raise Fail "Tabulate.tabulate: row width mismatch"

      val () = List.app rowWidth data

      fun maxCol j =
        List.foldl Int.max 0 (List.map (fn row => String.size (List.nth (row, j))) data)
      val max_per_column = List.tabulate (n_cols, maxCol)

      fun padCharAt j = String.sub (padChars, j mod String.size padChars)

      fun cell (x: string, j: int) : string =
        let
          val c = padCharAt j
          val left = if j = 0 then "" else repeatChar (c, cellPadding)
          val a = String.sub (alignments, j)
        in
          left ^ pad (x, a, List.nth (max_per_column, j), c)
        end

      fun line row =
        strip_trailing_spaces
          (String.concat (mapi (fn (j, x) => cell (x, j)) row))
    in
      List.app (fn row => (print (line row); print "\n")) data
    end

  fun tabulateDefault (data: string list list) : unit =
    let
      val n_cols =
        (case data of
           [] => 0
         | r :: _ => length r)
      val alignments = String.implode (List.tabulate (n_cols, fn _ => #"l"))
    in
      if n_cols = 0 then () else tabulate (data, alignments, 2, " ")
    end
end
