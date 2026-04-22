use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";

structure RootDatum = struct
  type t = AtlasFFI.rootdatum

  fun newSimple (typeLetter: char, rank: int, preferCoroots: bool) : t =
    let
      val h = AtlasFFI.atlas_rootdatum_new_simple (typeLetter, rank, if preferCoroots then 1 else 0)
    in
      if h = Foreign.Memory.null then
        raise Fail ("RootDatum.newSimple: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        h
    end

  fun newFromSimpleMats (simpleRoots: IntMatrix.mat, simpleCoroots: IntMatrix.mat, preferCoroots: bool) : t =
    let
      val h =
        AtlasFFI.atlas_rootdatum_new_from_simple_mats_text
          ( IntMatrix.matToText simpleRoots
          , IntMatrix.matToText simpleCoroots
          , if preferCoroots then 1 else 0
          )
    in
      if h = Foreign.Memory.null then
        raise Fail ("RootDatum.newFromSimpleMats: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        h
    end

  val free = AtlasFFI.atlas_rootdatum_free

  fun rank (h: t) : int =
    let
      val r = AtlasFFI.atlas_rootdatum_rank h
    in
      if r < 0 then raise Fail ("RootDatum.rank: failed: " ^ AtlasFFI.atlas_last_error ()) else r
    end

  fun rhoText (h: t) : string = AtlasFFI.atlas_rootdatum_rho_text h

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("RootDatum: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun parseColumnVectorsText s : int list list =
    let
      val ns = parseInts s
    in
      case ns of
        count :: dim :: rest =>
          let
            val need = count * dim
            val () =
              if count < 0 orelse dim < 0 then raise Fail "RootDatum: negative header"
              else if length rest <> need then raise Fail "RootDatum: truncated vectors"
              else ()
            fun vec j = List.take (List.drop (rest, j * dim), dim)
          in
            List.tabulate (count, vec)
          end
      | _ => raise Fail "RootDatum: truncated header"
    end

  fun matFromColumns (cols: int list list) : IntMatrix.mat =
    (case cols of
       [] => []
     | c0 :: cs =>
         let
           val n = length c0
           val () = if List.all (fn c => length c = n) cs then () else raise Fail "RootDatum.matFromColumns: ragged"
           fun row i = List.map (fn c => List.nth (c, i)) cols
         in
           List.tabulate (n, row)
         end)

  fun simpleRootsCols (h: t) : int list list =
    parseColumnVectorsText (AtlasFFI.atlas_rootdatum_simple_roots_text h)

  fun simpleCorootsCols (h: t) : int list list =
    parseColumnVectorsText (AtlasFFI.atlas_rootdatum_simple_coroots_text h)

  fun simpleRootsMat (h: t) : IntMatrix.mat =
    matFromColumns (simpleRootsCols h)

  fun simpleCorootsMat (h: t) : IntMatrix.mat =
    matFromColumns (simpleCorootsCols h)

  fun posRootsCols (h: t) : int list list =
    parseColumnVectorsText (AtlasFFI.atlas_rootdatum_posroots_text h)

  fun posCorootsCols (h: t) : int list list =
    parseColumnVectorsText (AtlasFFI.atlas_rootdatum_poscoroots_text h)

  fun rootsCols (h: t) : int list list =
    parseColumnVectorsText (AtlasFFI.atlas_rootdatum_roots_text h)

  fun corootsCols (h: t) : int list list =
    parseColumnVectorsText (AtlasFFI.atlas_rootdatum_coroots_text h)

  fun corootOfRoot (h: t) (root: int list) : int list option =
    let
      val rs = rootsCols h
      val cs = corootsCols h
      val () = if length rs = length cs then () else raise Fail "RootDatum.corootOfRoot: mismatch"
      fun loop ([], [], _) = NONE
        | loop (r :: rs, c :: cs, i) = if r = root then SOME c else loop (rs, cs, i + 1)
        | loop _ = raise Fail "RootDatum.corootOfRoot: mismatch"
    in
      loop (rs, cs, 0)
    end
end
