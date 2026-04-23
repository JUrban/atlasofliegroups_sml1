use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/groups_at.sml

  Purpose
  - Partial SML translation of `atlas-scripts/groups.at` focusing on the
    *complex root datum* constructors (as opposed to real forms).
  - Many Atlas `.at` scripts use `groups.at` primarily to obtain standard
    root data in "nice coordinates" for the classical groups (GL/SL/Sp/SO).
    This file ports those constructors so that further `.at`→`.sml` ports can
    avoid depending on the Atlas interpreter.

  Scope (incremental port)
  - Implemented here:
      - `torus_datum` (central torus root datum)
      - Type A root data: `GL_roots`, `SL_roots`, `SL_coroots`, `GL`, `SL`, `PSL`
      - Type B/C/D simple-root matrices in standard coordinates:
          `type_B_roots`, `type_C_roots`, `type_D_roots`
      - Type C root data: `Sp`, `PSp`
      - Type B/D root data: `SO`
  - Not yet implemented:
      - Real-form constructors (`SU`, `U`, `SL_R`, ...), which in the SML port
        are provided via `atlas-scripts-sml/groups.sml` and the C++ shim.
      - General isogeny/quotient constructions (`GSp`, `Spin`, `PSO`, ...)
        which require additional lattice-quotient support beyond the current
        SML/FFI layer.

  Matrix conventions
  - Unlike the `.at` language (which stores matrices column-major), the SML
    port uses row-major matrices (`int list list`) throughout.
  - All constructors below are written directly as entry formulas in the usual
    mathematical (row, column) sense, so they match the `.at` matrices as
    *mathematical* matrices despite the different in-memory representation.

  Ownership
  - Each function returning a `RootDatum.t` allocates a fresh handle.
  - Callers must free it with `RootDatum.free`.
*)

structure GroupsAT = struct
  type mat = IntMatrix.mat
  type rootdatum = RootDatum.t

  fun requireNonNeg (name: string, n: int) =
    if n < 0 then raise Fail ("GroupsAT." ^ name ^ ": expected nonnegative int") else ()

  fun requirePos (name: string, n: int) =
    if n <= 0 then raise Fail ("GroupsAT." ^ name ^ ": expected positive int") else ()

  fun delta (i: int, j: int) : int = if i = j then 1 else 0

  (* Central torus root datum of rank `n` (no roots). *)
  fun torus_datum (n: int) : rootdatum =
    let
      val () = requireNonNeg ("torus_datum", n)
      val empty = MatrixAT.null (n, 0)
    in
      RootDatum.newFromSimpleMats (empty, empty, false)
    end

  val trivial_datum : rootdatum = torus_datum 0

  (*
    Type A root matrices (in the `groups.at` "nice coordinates").

    - `GL_roots(n)` is an `n x (n-1)` matrix whose columns are `e_j - e_{j+1}`.
    - `SL_coroots(n)` is an `(n-1) x (n-1)` matrix with the same pattern.
    - `SL_roots(n)` is `SL_coroots(n)` with `+1` in the final column (as in
      `groups.at`), giving e.g. `SL_roots(2) = [[2]]`.
  *)

  fun GL_roots (n: int) : mat =
    let
      val () = requireNonNeg ("GL_roots", n)
    in
      if n = 0 then
        MatrixAT.id_mat 0
      else
        Basic.matrix ((n, n - 1), fn (i, j) => delta (i, j) - delta (i, j + 1))
    end

  fun SL_coroots (n: int) : mat =
    let
      val () = requirePos ("SL_coroots", n)
      val r = n - 1
    in
      Basic.matrix ((r, r), fn (i, j) => delta (i, j) - delta (i, j + 1))
    end

  fun SL_roots (n: int) : mat =
    let
      val () = requirePos ("SL_roots", n)
      val r = n - 1
    in
      Basic.matrix
        ((r, r), fn (i, j) => delta (i, j) - delta (i, j + 1) + delta (j, r - 1))
    end

  fun GL (n: int) : rootdatum =
    let
      val () = requireNonNeg ("GL", n)
      val r = GL_roots n
    in
      RootDatum.newFromSimpleMats (r, r, false)
    end

  fun SL (n: int) : rootdatum =
    let
      val () = requirePos ("SL", n)
    in
      RootDatum.newFromSimpleMats (SL_roots n, SL_coroots n, false)
    end

  fun PSL (n: int) : rootdatum =
    let
      val () = requirePos ("PSL", n)
    in
      RootDatum.newFromSimpleMats (SL_coroots n, SL_roots n, false)
    end

  (* Type B roots in standard coordinates: identical to `SL_coroots(n+1)`. *)
  fun type_B_roots (n: int) : mat =
    let
      val () = requireNonNeg ("type_B_roots", n)
    in
      SL_coroots (n + 1)
    end

  (* Type C roots in standard coordinates. *)
  fun type_C_roots (n: int) : mat =
    let
      val () = requireNonNeg ("type_C_roots", n)
    in
      Basic.matrix
        ((n, n), fn (i, j) =>
          if n > 0 andalso i = n - 1 andalso j = n - 1 then 2
          else delta (i, j) - delta (i, j + 1))
    end

  (* Type D roots in standard coordinates. For `n <= 1`, it is toral (no roots). *)
  fun type_D_roots (n: int) : mat =
    let
      val () = requireNonNeg ("type_D_roots", n)
    in
      if n <= 1 then
        MatrixAT.null (n, 0)
      else
        Basic.matrix
          ((n, n), fn (i, j) =>
            if i = n - 2 andalso j = n - 1 then 1
            else delta (i, j) - delta (i, j + 1))
    end

  (*
    Type C root data.

    In `groups.at`:
      Sp(n)  = root_datum(type_C_roots(m), type_B_roots(m), true)  for n=2m
      PSp(n) = root_datum(id_mat(m), ^type_C_roots(m)*type_B_roots(m), false)
  *)

  fun Sp (n: int) : rootdatum =
    let
      val () = requirePos ("Sp", n)
      val () = if n mod 2 = 0 then () else raise Fail "GroupsAT.Sp: odd symplectic datum"
      val m = n div 2
    in
      RootDatum.newFromSimpleMats (type_C_roots m, type_B_roots m, true)
    end

  fun PSp (n: int) : rootdatum =
    let
      val () = requirePos ("PSp", n)
      val () = if n mod 2 = 0 then () else raise Fail "GroupsAT.PSp: odd symplectic datum"
      val m = n div 2
      val coroots = IntMatrix.matMul (IntMatrix.transpose (type_C_roots m), type_B_roots m)
    in
      RootDatum.newFromSimpleMats (MatrixAT.id_mat m, coroots, false)
    end

  (*
    Type B/D root data: use the standard basis for SO(n) (as in `groups.at`).

    - If `n = 2m+1` (odd), then type is B_m and:
        roots = type_B_roots(m), coroots = type_C_roots(m)
    - If `n = 2m` (even), then type is D_m and roots = coroots = type_D_roots(m)
  *)
  fun SO (n: int) : rootdatum =
    let
      val () = requireNonNeg ("SO", n)
      val m = n div 2
    in
      if n mod 2 = 1 then
        RootDatum.newFromSimpleMats (type_B_roots m, type_C_roots m, false)
      else
        let
          val roots = type_D_roots m
        in
          RootDatum.newFromSimpleMats (roots, roots, false)
        end
    end
end

