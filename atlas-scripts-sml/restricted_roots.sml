use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/parameters.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/restricted_roots.sml

  Purpose
  - Standard ML port of `atlas-scripts/restricted_roots.at`.
  - Computes (a reduced version of) the restricted root datum of the maximal
    split torus `A` in a real form `G` where `H = T A` is the “most split”
    Cartan.

  High-level algorithm (as in the `.at` script)
  - Let `delta` be the Cartan involution attached to the most split Cartan.
    In the `.at` code this is taken as `G.trivial.x.involution`.
  - Compute a basis `inj` for the cocharacter lattice `X_*(A)` as the `(-1)`
    eigenlattice of the transpose action on cocharacters:
      `inj = eigen_lattice(^delta, -1)`.
    This gives an injective lattice map `inj : X_*(A) -> X_*(H)`.
  - Restrict roots to `A` via `proj = ^inj` and prune duplicates/zeros.
  - Compute injected coroots via the `pullback = left_inverse(inj)` map and
    the correction formula for complex pairs, then prune duplicates/zeros.
  - Optionally reduce a nonreduced root system by keeping either:
      - short roots with long coroots (`restricted_roots_short`), or
      - long roots with short coroots (`restricted_roots_long`).
  - Finally, construct a `RootDatum` from the resulting positive roots/coroots
    by extracting a simple system.

  Implementation notes
  - Matrices are row-major `int list list`, but are interpreted as mathematical
    matrices with *columns* representing root/coroot vectors, matching the
    rest of the SML port (see `RootDatum.newFromSimpleMats`).
  - The `eigen_lattice` and `kernel` routines in the shim return matrices whose
    columns are basis vectors (validated by smoke tests elsewhere).
*)

structure RestrictedRoots = struct
  type group = AtlasFFI.group
  type mat = IntMatrix.mat
  type vec = int list

  fun fail where' msg = raise Fail ("RestrictedRoots." ^ where' ^ ": " ^ msg)

  fun vecNeg (v: vec) : vec = List.map (fn x => ~x) v
  fun vecAdd (a: vec, b: vec) : vec = ListPair.mapEq (op +) (a, b)
  fun vecSub (a: vec, b: vec) : vec = ListPair.mapEq (op -) (a, b)
  fun vecScale (k: int, v: vec) : vec = List.map (fn x => k * x) v

  fun isZeroVec (v: vec) : bool = List.all (fn x => x = 0) v

  fun columnsOfMat (m: mat) : vec list =
    let
      val (nRows, nCols) = IntMatrix.matShape m
      fun col j = List.tabulate (nRows, fn i => List.nth (List.nth (m, i), j))
    in
      List.tabulate (nCols, col)
    end

  fun matFromColumns (cols: vec list) : mat =
    (case cols of
       [] => []
     | c0 :: _ =>
         let
           val nRows = length c0
           val () = if List.all (fn c => length c = nRows) cols then () else fail "matFromColumns" "ragged"
           fun row i = List.map (fn c => List.nth (c, i)) cols
         in
           List.tabulate (nRows, row)
         end)

  fun remove_duplicates (xs: vec list) : vec list =
    let
      fun loop ([], acc) = List.rev acc
        | loop (v :: rest, acc) =
            if List.exists (fn w => w = v) acc then loop (rest, acc) else loop (rest, v :: acc)
    in
      loop (xs, [])
    end

  fun remove_zeros (xs: vec list) : vec list =
    List.filter (fn v => not (isZeroVec v)) xs

  fun pruneColumns (cols: vec list) : vec list =
    remove_zeros (remove_duplicates cols)

  fun keep_long (cols: vec list) : vec list =
    let
      fun has v = List.exists (fn w => w = v) cols
      fun step v = if has (vecScale (2, v)) then NONE else SOME v
    in
      List.mapPartial step cols
    end

  fun allEven (v: vec) : bool = List.all (fn x => x mod 2 = 0) v

  fun keep_short (cols: vec list) : vec list =
    let
      fun has v = List.exists (fn w => w = v) cols
      fun half v = List.map (fn x => x div 2) v
      fun step v =
        if allEven v andalso has (half v) then NONE else SOME v
    in
      List.mapPartial step cols
    end

  fun keep_short_roots_long_coroots (roots: mat, coroots: mat) : mat * mat =
    let
      val (rRows, _) = IntMatrix.matShape roots
      val (cRows, _) = IntMatrix.matShape coroots
      val () = if rRows = cRows then () else ()
      val rs = keep_short (columnsOfMat roots)
      val cs = keep_long (columnsOfMat coroots)
    in
      (matFromColumns rs, matFromColumns cs)
    end

  fun keep_long_roots_short_coroots (roots: mat, coroots: mat) : mat * mat =
    let
      val rs = keep_long (columnsOfMat roots)
      val cs = keep_short (columnsOfMat coroots)
    in
      (matFromColumns rs, matFromColumns cs)
    end

  fun matTranspose (m: mat) : mat = IntMatrix.transpose m

  fun matAdd (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op +) (ra, rb)) (a, b)

  fun matSub (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op -) (ra, rb)) (a, b)

  fun identity (n: int) : mat = IntMatrix.identity n

  fun dot (a: vec, b: vec) : int =
    List.foldl (op +) 0 (ListPair.mapEq (op *) (a, b))

  (* Extract the “delta” matrix used by `restricted_roots.at`:
       delta = G.trivial.x.involution.
     Here we compute the trivial parameter, read its `x`, and then read the KGB
     involution matrix of that `x`. *)
  fun delta_most_split (g: group) : mat =
    let
      val p = AtlasFFI.atlas_param_trivial g
      val () = if p = Foreign.Memory.null then fail "delta_most_split" (AtlasFFI.atlas_last_error ()) else ()
      val x = AtlasFFI.atlas_param_x p
      val () = AtlasFFI.atlas_param_free p
    in
      Parameters.theta_matrix (g, x)
    end

  (* Basis of `X_*(H^{-delta})` (max split torus cocharacters), as a matrix with
     columns giving the basis vectors, matching `basis_max_split_torus` in `.at`. *)
  fun basis_max_split_torus (g: group) : mat =
    let
      val delta = delta_most_split g
      val deltaT = matTranspose delta
    in
      IntMatrix.eigenLattice (deltaT, ~1)
    end

  (* Return the matrices (A,B) from `restricted_roots_raw(G)` in `.at`:
     - A: columns are positive restricted roots in `X^*(A)`
     - B: columns are positive injected coroots in `X_*(A)` *)
  fun restricted_roots_raw (g: group) : mat * mat =
    let
      val rdH = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rdH = Foreign.Memory.null then
          fail "restricted_roots_raw" ("rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val rank = RootDatum.rank rdH

      val delta = delta_most_split g

      val inj = basis_max_split_torus g (* rank x dimA, columns basis in X_*(H) *)
      val proj = matTranspose inj (* dimA x rank *)
      val pullback = MatrixAT.left_inverse inj (* dimA x rank *)

      val posRoots = RootDatum.posRootsCols rdH
      val posCoroots = RootDatum.posCorootsCols rdH
      val () =
        if length posRoots = length posCoroots then ()
        else fail "restricted_roots_raw" "posroots/poscoroots mismatch"

      val posRootsMat = matFromColumns posRoots (* rank x npr *)
      val restrictedMat = IntMatrix.matMul (proj, posRootsMat) (* dimA x npr *)
      val restrictedCols = pruneColumns (columnsOfMat restrictedMat)
      val restrictedRoots = matFromColumns restrictedCols

      val id = identity rank
      val oneMinusDelta = matSub (id, delta)
      val deltaT = matTranspose delta
      val oneMinusDeltaT = matTranspose oneMinusDelta

      (* Build a rank x t matrix whose columns are the coroot contributions in X_*(H). *)
      fun corootContribution (alphav: vec, alpha: vec) : vec option =
        let
          val alphavDelta = Lattice.matVecMulInt deltaT alphav (* alphav * delta, represented as col *)
        in
          if alphavDelta = vecNeg alphav then
            SOME alphav
          else if alphavDelta = alphav then
            NONE
          else
            let
              val c = 1 + dot (alphavDelta, alpha)
              val base = Lattice.matVecMulInt oneMinusDeltaT alphav (* alphav*(1-delta) as col *)
            in
              SOME (vecScale (c, base))
            end
        end

      fun buildCorootCols ([], [], acc) = List.rev acc
        | buildCorootCols (alphav :: restV, alpha :: restR, acc) =
            (case corootContribution (alphav, alpha) of
               NONE => buildCorootCols (restV, restR, acc)
             | SOME v => buildCorootCols (restV, restR, v :: acc))
        | buildCorootCols _ = fail "restricted_roots_raw" "posroots length mismatch"

      val contribCols = buildCorootCols (posCoroots, posRoots, [])
      val contribMat = matFromColumns contribCols (* rank x t *)
      val injectedMat = IntMatrix.matMul (pullback, contribMat) (* dimA x t *)
      val injectedCols = pruneColumns (columnsOfMat injectedMat)
      val injectedCoroots = matFromColumns injectedCols

      val () = RootDatum.free rdH
    in
      (restrictedRoots, injectedCoroots)
    end

  (* Extract a simple system from a positive root list:
     simple roots are those not expressible as sum of two positive roots. *)
  fun simple_system (pos: vec list) : vec list =
    let
      fun has v = List.exists (fn w => w = v) pos
      fun decomposable v =
        List.exists
          (fn a =>
             let
               val b = vecSub (v, a)
             in
               has b andalso not (isZeroVec b)
             end)
          pos
    in
      List.filter (fn v => not (decomposable v)) pos
    end

  fun root_datum_from_positive (posRootsMat: mat, posCorootsMat: mat, preferCoroots: bool) : RootDatum.t =
    let
      val posRoots = columnsOfMat posRootsMat
      val posCoroots = columnsOfMat posCorootsMat
      val simRoots = simple_system posRoots
      val simCoroots0 = simple_system posCoroots

      fun findCoroot alpha =
        let
          val cands = List.filter (fn alphav => dot (alphav, alpha) = 2) simCoroots0
        in
          case cands of
            [v] => v
          | [] => fail "root_datum_from_positive" "no matching coroot for simple root"
          | _ => fail "root_datum_from_positive" "non-unique matching coroot for simple root"
        end

      val simCoroots = List.map findCoroot simRoots
      val rMat = matFromColumns simRoots
      val cMat = matFromColumns simCoroots
    in
      RootDatum.newFromSimpleMats (rMat, cMat, preferCoroots)
    end

  fun restricted_roots_short (g: group) : RootDatum.t =
    let
      val (a, b) = restricted_roots_raw g
      val (a2, b2) = keep_short_roots_long_coroots (a, b)
    in
      root_datum_from_positive (a2, b2, false)
    end

  fun restricted_roots_long (g: group) : RootDatum.t =
    let
      val (a, b) = restricted_roots_raw g
      val (a2, b2) = keep_long_roots_short_coroots (a, b)
    in
      root_datum_from_positive (a2, b2, false)
    end

  val restricted_roots = restricted_roots_short
end
