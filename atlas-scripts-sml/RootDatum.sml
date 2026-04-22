use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/diagram.sml";

structure RootDatum = struct
  type t = AtlasFFI.rootdatum

  fun dot (xs: int list, ys: int list) : int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "RootDatum.dot: length mismatch"
    in
      loop (xs, ys, 0)
    end

  fun dual (h: t) : t =
    let
      val d = AtlasFFI.atlas_rootdatum_dual h
    in
      if d = Foreign.Memory.null then
        raise Fail ("RootDatum.dual: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        d
    end

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

  fun coroot (h: t) (root: int list) : int list =
    case corootOfRoot h root of
      SOME c => c
    | NONE => raise Fail "RootDatum.coroot: root not found"

  fun semisimpleRank (h: t) : int =
    length (simpleRootsCols h)

  fun rootExpression (h: t) (root: int list) : int list =
    let
      val a = simpleRootsMat h (* rank x ssRank, columns are simple roots *)
    in
      case Lattice.solve (a, root) of
        NONE => raise Fail "RootDatum.rootExpression: no solution"
      | SOME coeffs => coeffs
    end

  fun highestRoot (h: t) : int list =
    let
      val pos = posRootsCols h
      fun height v = List.foldl (op +) 0 (rootExpression h v)
      fun pick (x, best) = if height x > height best then x else best
    in
      case pos of
        [] => raise Fail "RootDatum.highestRoot: no posroots"
      | x :: xs => List.foldl pick x xs
    end

  fun highestShortRoot (h: t) : int list =
    let
      val d = dual h
      val cr = highestRoot d
      val () = free d
      val rs = rootsCols h
      val cs = corootsCols h
      val () = if length rs = length cs then () else raise Fail "RootDatum.highestShortRoot: mismatch"
      fun loop ([], [], _) = raise Fail "RootDatum.highestShortRoot: not found"
        | loop (r :: rs, c :: cs, i) = if c = cr then r else loop (rs, cs, i + 1)
        | loop _ = raise Fail "RootDatum.highestShortRoot: mismatch"
    in
      loop (rs, cs, 0)
    end

  fun cartanMatrix (h: t) : int list list =
    let
      val roots = simpleRootsCols h
      val coroots = simpleCorootsCols h
      val n = length roots
      val () = if length coroots = n then () else raise Fail "RootDatum.cartanMatrix: mismatch"
      fun entry (i, j) = dot (List.nth (roots, i), List.nth (coroots, j))
      fun row i = List.tabulate (n, fn j => entry (i, j))
    in
      List.tabulate (n, row)
    end

  fun matColumns (m: IntMatrix.mat) : int list list =
    let
      val (_, nCols) = IntMatrix.matShape m
      fun col j = List.map (fn row => List.nth (row, j)) m
    in
      List.tabulate (nCols, col)
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

  fun selectColumns (cols: int list, m: IntMatrix.mat) : IntMatrix.mat =
    let
      val (nRows, nCols) = IntMatrix.matShape m
      val () =
        if List.all (fn j => 0 <= j andalso j < nCols) cols then () else raise Fail "RootDatum.selectColumns: oob"
      val allCols = matColumns m
      val picked = List.map (fn j => List.nth (allCols, j)) cols
    in
      if null picked then List.tabulate (nRows, fn _ => []) else matFromColumns picked
    end

  fun numberSimpleFactors (h: t) : int =
    let
      val c = cartanMatrix h
      val n = length c
      fun entry i j = List.nth (List.nth (c, i), j)

      val seen = Array.array (n, false)
      fun neighbors i =
        List.filter
          (fn j => j <> i andalso (entry i j < 0 orelse entry j i < 0))
          (List.tabulate (n, fn k => k))

      fun bfs (queue: int list) =
        (case queue of
           [] => ()
         | i :: rest =>
             if Array.sub (seen, i) then
               bfs rest
             else
               (Array.update (seen, i, true);
                bfs (rest @ neighbors i)))

      fun loop (i, count) =
        if i >= n then count
        else if Array.sub (seen, i) then loop (i + 1, count)
        else (bfs [i]; loop (i + 1, count + 1))
    in
      if n = 0 then 0 else loop (0, 0)
    end

  fun simpleFactorIndexSets (h: t) : int list list =
    let
      val c = cartanMatrix h
      val n = length c
      fun entry i j = List.nth (List.nth (c, i), j)

      val seen = Array.array (n, false)
      fun neighbors i =
        List.filter
          (fn j => j <> i andalso (entry i j < 0 orelse entry j i < 0))
          (List.tabulate (n, fn k => k))

      fun bfs (queue: int list, acc: int list) : int list =
        (case queue of
           [] => List.rev acc
         | i :: rest =>
             if Array.sub (seen, i) then
               bfs (rest, acc)
             else
               (Array.update (seen, i, true);
                bfs (rest @ neighbors i, i :: acc)))

      fun loop (i, comps) =
        if i >= n then List.rev comps
        else if Array.sub (seen, i) then loop (i + 1, comps)
        else loop (i + 1, bfs ([i], []) :: comps)
    in
      if n = 0 then [] else loop (0, [])
    end

  (* Return simple factors as root data embedded in the same ambient lattice
     (i.e. same `rank`, fewer simple roots). *)
  fun simpleFactorsEmbedded (h: t) : t list =
    let
      val comps = simpleFactorIndexSets h
      val roots = simpleRootsMat h
      val coroots = simpleCorootsMat h
      fun factor idxs =
        newFromSimpleMats (selectColumns (idxs, roots), selectColumns (idxs, coroots), false)
    in
      List.map factor comps
    end

  fun derived_is_simple (h: t) : bool =
    numberSimpleFactors h = 1

  (* One highest root per simple factor, in ambient coordinates. *)
  fun highestRoots (h: t) : int list list =
    let
      val factors = simpleFactorsEmbedded h
      val hrs = List.map highestRoot factors
      val () = List.app free factors
    in
      hrs
    end

  fun highestShortRoots (h: t) : int list list =
    let
      val factors = simpleFactorsEmbedded h
      val hsrs = List.map highestShortRoot factors
      val () = List.app free factors
    in
      hsrs
    end

  fun lieType (h: t) : LieType.t =
    let
      val toks = String.tokens Char.isSpace (AtlasFFI.atlas_rootdatum_simple_factors_text h)
    in
      case toks of
        [] => raise Fail "RootDatum.lieType: empty"
      | kTok :: rest =>
          (case Int.fromString kTok of
             NONE => raise Fail "RootDatum.lieType: bad header"
           | SOME k =>
               let
                 fun loop (0, xs, acc) = if null xs then List.rev acc else raise Fail "RootDatum.lieType: extra tokens"
                   | loop (n, cTok :: rTok :: xs, acc) =
                       let
                         val () = if String.size cTok = 1 then () else raise Fail "RootDatum.lieType: bad letter"
                         val c = String.sub (cTok, 0)
                         val r =
                           (case Int.fromString rTok of
                              SOME rr => rr
                            | NONE => raise Fail "RootDatum.lieType: bad rank")
                       in
                         loop (n - 1, xs, (c, r) :: acc)
                       end
                   | loop _ = raise Fail "RootDatum.lieType: truncated"
               in
                 loop (k, rest, [])
               end)
    end

  fun diagramAutomorphismMatrices (h: t) : IntMatrix.mat list =
    let
      val lt = lieType h
      val perms = Diagram.diagram_automorphisms lt
      val ss = LieType.semisimple_rank lt
      val r = rank h
      val () =
        if r = ss then ()
        else raise Fail "RootDatum.diagramAutomorphismMatrices: unsupported (rank != semisimple_rank)"
    in
      List.map (fn pi => MatrixAT.permutation_matrix pi) perms
    end

  fun mul (a: t, b: t) : t =
    let
      val r = MatrixAT.block_matrix (simpleRootsMat a, simpleRootsMat b)
      val cr = MatrixAT.block_matrix (simpleCorootsMat a, simpleCorootsMat b)
    in
      newFromSimpleMats (r, cr, false)
    end

  fun fromLieType (lt: LieType.t) : t =
    let
      fun fold ([], acc) = acc
        | fold ((c, r) :: rest, NONE) = fold (rest, SOME (newSimple (c, r, false)))
        | fold ((c, r) :: rest, SOME acc) =
            let
              val rd = newSimple (c, r, false)
              val prod = mul (acc, rd)
              val () = free acc
              val () = free rd
            in
              fold (rest, SOME prod)
            end
    in
      case fold (lt, NONE) of
        NONE => raise Fail "RootDatum.fromLieType: empty"
      | SOME rd => rd
    end
end
