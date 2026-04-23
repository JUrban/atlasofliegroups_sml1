use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/MatReduc.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/center.sml

  Purpose
  - Partial SML port of `atlas-scripts/center.at`.
  - Focuses on the complex-group (`RootDatum`) parts needed by many scripts:
      - computing the radical (maximal central torus) at the Lie algebra level
      - extracting invariant factors for the (finite) center of a semisimple
        group using Atlas’ `adapted_basis`

  What is implemented
  - `lie_radical` / `lie_radical_hat`:
      Q-bases for the Lie algebra of the radical (resp. its dual), given by
      integer kernel computations.
  - `filter_units`:
      helper used throughout `center.at` to discard invariant factors equal to 1.
      This mirrors the Atlas interpreter’s `filter_units` (see
      `atlas-scripts/atlas-functions.help`).
  - `type_center`, `order_center`, `has_cyclic_center`:
      invariant factors / order of the finite center for semisimple root data.
  - `Z_hat` (RootDatum case):
      a decomposition of the dual of the center into finite part + radical part,
      matching the return-shape in `center.at` for the `RootDatum` overload.

  Not yet implemented
  - InnerClass/RealForm versions of `Z_hat`, printing helpers, and explicit
    lists of center elements (`elements_of_center`), which depend on additional
    real-form/torus-factor machinery not yet exposed in the SML port.

  Matrix conventions
  - SML matrices are row-major (`int list list`), whereas `.at` matrices are
    stored column-major. All operations here are expressed in terms of the
    mathematical matrix entries, so results match the `.at` behavior.
*)

structure Center = struct
  type mat = IntMatrix.mat
  type rootdatum = RootDatum.t

  (* Select column `j` (0-based) from a row-major matrix. *)
  fun col (m: mat, j: int) : int list = List.map (fn row => List.nth (row, j)) m

  fun matFromCols (nRows: int, cols: int list list) : mat =
    if null cols then
      MatrixAT.null (nRows, 0)
    else
      let
        val () = if List.all (fn c => length c = nRows) cols then () else raise Fail "Center.matFromCols: ragged cols"
        fun row i = List.map (fn c => List.nth (c, i)) cols
      in
        List.tabulate (nRows, row)
      end

  fun selectCols (m: mat, js: int list) : mat =
    let
      val (nRows, nCols) = IntMatrix.matShape m
      val () = List.app (fn j => if 0 <= j andalso j < nCols then () else raise Fail "Center.selectCols: oob") js
      val cols = List.map (fn j => col (m, j)) js
    in
      matFromCols (nRows, cols)
    end

  (*
    `filter_units` from the Atlas interpreter.

    Input `(B, v)` typically comes from `Smith_Cartan` or `adapted_basis`.
    For each position `i` where `v[i] = 1`, that entry and the corresponding
    column of `B` is discarded. If `v` is shorter than the number of columns
    in `B`, missing entries are treated as “not 1” and their columns are kept.

    For compatibility with `center.at` usage, we also reorder the kept columns
    so that all kept `(v[i] <> 1)` columns from the paired prefix come first;
    this makes `firstCols(length vFiltered, BFiltered)` pick out the torsion
    generators as in `.at`.
  *)
  fun filter_units (b: mat, v: int list) : mat * int list =
    let
      val (_, nCols) = IntMatrix.matShape b
      val k = length v
      val paired = List.tabulate (Int.min (k, nCols), fn i => i)
      val torsionIdxs = List.filter (fn i => List.nth (v, i) <> 1) paired
      val extraIdxs =
        if nCols > k then List.tabulate (nCols - k, fn t => k + t) else []
      val keepIdxs = torsionIdxs @ extraIdxs
      val b' = selectCols (b, keepIdxs)
      val v' = List.map (fn i => List.nth (v, i)) torsionIdxs
    in
      (b', v')
    end

  (*
    Q-basis of Lie algebra of the radical (maximal central torus); columns are
    coweights (in the Atlas coordinate conventions).

    `.at`: `lie_radical(rd) = kernel(^simple_roots(rd))`.
  *)
  fun lie_radical (rd: rootdatum) : mat =
    IntMatrix.kernel (IntMatrix.transpose (RootDatum.simpleRootsMat rd))

  (*
    Q-basis of the dual of the radical; columns are weights.

    `.at`: `lie_radical_hat(rd) = kernel(^simple_coroots(rd))`.
  *)
  fun lie_radical_hat (rd: rootdatum) : mat =
    IntMatrix.kernel (IntMatrix.transpose (RootDatum.simpleCorootsMat rd))

  (*
    Invariant factors of the (finite) center for semisimple `rd`.

    `.at`: `type_center(rd) = filter_units(adapted_basis(^simple_roots(rd))).snd`
  *)
  fun type_center (rd: rootdatum) : int list =
    let
      val () =
        if RootDatum.rank rd = RootDatum.semisimpleRank rd then ()
        else raise Fail "Center.type_center: RootDatum is not semisimple"
      val (_, v) = MatReduc.adaptedBasis (IntMatrix.transpose (RootDatum.simpleRootsMat rd))
    in
      List.filter (fn d => d <> 1) v
    end

  fun order_center (rd: rootdatum) : int =
    List.foldl (op *) 1 (type_center rd)

  fun has_cyclic_center (rd: rootdatum) : bool =
    let
      val () =
        if RootDatum.rank rd = RootDatum.semisimpleRank rd then ()
        else raise Fail "Center.has_cyclic_center: RootDatum is not semisimple"
    in
      length (type_center rd) <= 1
    end

  (*
    Dual of the center for a complex group given by `rd`.

    Return value matches `center.at` (RootDatum overload):
      `Z_hat(rd) = ((semisimple_part_hat, radical_hat))`
    where
      - `semisimple_part_hat` is `(generators, orders)` for the finite part
      - `radical_hat` is `lie_radical(rd)` (columns are a Q-basis of the Lie
        algebra of the radical, in coweight coordinates)
  *)
  fun Z_hat (rd: rootdatum) : ((mat * int list) * mat) =
    let
      val (b, v) = MatReduc.adaptedBasis (RootDatum.simpleRootsMat rd)
      val (b', v') = filter_units (b, v)
      val torsionGens = IntMatrix.firstCols (length v', b')
      val rad = lie_radical rd
    in
      ((torsionGens, v'), rad)
    end
end
