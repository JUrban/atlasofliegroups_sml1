use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure LambdaDifferential0 = struct
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("LambdaDifferential0: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun all (g: AtlasFFI.group, x: int) : int list list =
    let
      val s = AtlasFFI.atlas_kgb_all_lambda_differential_0_text (g, x)
      val ns = parseInts s
    in
      case ns of
        count :: rank :: rest =>
          let
            fun takeVec (0, xs, acc) = (List.rev acc, xs)
              | takeVec (n, x :: xs, acc) = takeVec (n - 1, xs, x :: acc)
              | takeVec _ = raise Fail "LambdaDifferential0: truncated vectors"

            fun loop (0, xs, acc) = (List.rev acc, xs)
              | loop (k, xs, acc) =
                  let
                    val (v, xs') = takeVec (rank, xs, [])
                  in
                    loop (k - 1, xs', v :: acc)
                  end

            val (vs, leftover) = loop (count, rest, [])
          in
            if null leftover then vs else raise Fail "LambdaDifferential0: extra ints"
          end
      | _ => raise Fail "LambdaDifferential0: bad header"
    end
end

