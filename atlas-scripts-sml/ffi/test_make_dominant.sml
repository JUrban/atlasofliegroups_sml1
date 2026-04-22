use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Dominant.sml";

structure TestMakeDominant = struct
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("test_make_dominant: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun parseSimpleCorootsText s : int list list =
    let
      val ns = parseInts s
    in
      case ns of
        ssRank :: rank :: rest =>
          let
            fun takeVec (0, xs, acc) = (List.rev acc, xs)
              | takeVec (n, x :: xs, acc) = takeVec (n - 1, xs, x :: acc)
              | takeVec _ = raise Fail "test_make_dominant: truncated"
            fun loop (0, xs, acc) = (List.rev acc, xs)
              | loop (k, xs, acc) =
                  let
                    val (v, xs') = takeVec (rank, xs, [])
                  in
                    loop (k - 1, xs', v :: acc)
                  end
            val (cors, leftover) = loop (ssRank, rest, [])
          in
            if null leftover then cors else raise Fail "test_make_dominant: extra"
          end
      | _ => raise Fail "test_make_dominant: bad header"
    end

  fun dot (xs: int list, ys: int list) =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "test_make_dominant: dot mismatch"
    in
      loop (xs, ys, 0)
    end

  fun parseRatWeightText s : (int * int list) =
    case parseInts s of
      den :: nums => (den, nums)
    | _ => raise Fail "test_make_dominant: bad ratweight"

  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val coroots = parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)

      val w = "1 -3 1"
      val wDom = Dominant.makeDominantText g w
      val (den, nums) = parseRatWeightText wDom
      val () = if den <= 0 then raise Fail "test_make_dominant: nonpositive denom" else ()

      val evals = List.map (fn cor => dot (cor, nums)) coroots
      val () = if List.all (fn e => e >= 0) evals then () else raise Fail "test_make_dominant: not dominant"

      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestMakeDominant.run ();
