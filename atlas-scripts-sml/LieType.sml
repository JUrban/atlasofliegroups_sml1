(*
  File: atlas-scripts-sml/LieType.sml

  Purpose
  - Minimal Lie type utilities used by the port (printing/group type reporting,
    and a small parser used mainly in tests and diagnostics).

  Representation
  - A Lie type is a list of simple factors `(letter, rank)`, e.g.
      [(#"A",2),(#"D",4),(#"A",1)]
*)
structure LieType = struct
  type simple_factor = char * int
  type t = simple_factor list

  (* Total semisimple rank. *)
  fun semisimple_rank (lt: t) : int =
    List.foldl (fn ((_, r), acc) => acc + r) 0 lt

  (* Identity projection: list of simple factors. *)
  fun simple_factors (lt: t) : simple_factor list = lt

  (* Parse strings like "A2D4A1" into a list [(#"A",2),(#"D",4),(#"A",1)].
     This is only a convenience for tests. *)
  fun parse (s: string) : t =
    let
      val n = String.size s
      fun isLetter c = Char.isAlpha c
      fun isDigit c = Char.isDigit c
      fun toInt chars =
        case Int.fromString (String.implode chars) of
          SOME k => k
        | NONE => raise Fail "LieType.parse: bad int"

      fun loop (i, acc) =
        if i >= n then List.rev acc
        else
          let
            val c = String.sub (s, i)
          in
            if not (isLetter c) then
              raise Fail "LieType.parse: expected letter"
            else
              let
                fun takeDigits (j, ds) =
                  if j < n andalso isDigit (String.sub (s, j)) then
                    takeDigits (j + 1, String.sub (s, j) :: ds)
                  else
                    (j, List.rev ds)
                val (j, ds) = takeDigits (i + 1, [])
                val () = if null ds then raise Fail "LieType.parse: missing rank" else ()
                val r = toInt ds
              in
                loop (j, (c, r) :: acc)
              end
          end
    in
      loop (0, [])
    end
end
