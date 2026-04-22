use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure KTypePol = struct
  type ktypepol = AtlasFFI.ktypepol

  type term = {e: int, s: int, x: int, height: int, lambdaRho: int list}

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("KTypePol: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun terms (pol: ktypepol, rank: int) : term list =
    let
      val n = AtlasFFI.atlas_ktypepol_num_terms pol
      val () = if n < 0 then raise Fail ("KTypePol.terms: bad poly: " ^ AtlasFFI.atlas_last_error ()) else ()

      fun one i =
        let
          val ns = parseInts (AtlasFFI.atlas_ktypepol_term_text (pol, i))
        in
          case ns of
            e :: s :: x :: height :: rest =>
              if length rest <> rank then
                raise Fail "KTypePol.terms: wrong lambda_rho length"
              else
                {e = e, s = s, x = x, height = height, lambdaRho = rest}
          | _ => raise Fail "KTypePol.terms: truncated term"
        end
    in
      List.tabulate (n, one)
    end

  fun free (pol: ktypepol) : unit = AtlasFFI.atlas_ktypepol_free pol
end

