use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/K.sml

  Purpose
  - Incremental SML translation of `atlas-scripts/K.at`.
  - The `.at` script provides a large amount of infrastructure for working
    with the compact group `K` (or `K_0`) attached to a real form:
      - coordinate conversions between `H` and `T_{K_0}`
      - K-root data construction
      - `KHighestWeight` and related utilities (highest-weight parameters)
      - K-norm helpers (some of which are already handled elsewhere in this repo)

  Status / scope of this port
  - This file currently ports only the *matrix/linear-algebra plumbing* that is
    relatively self-contained and useful for downstream ports:
      - `KHighestWeight` datatype and constructor `k_highest_weight`
      - `cocharacter_lattice_K_*` (the +1 eigenlattice basis for the relevant involution)
      - `projection_to_K_matrix_*`, `project_K_*`, `inject_K_*`

  Design notes (coordinate conventions)
  - The Atlas `.at` environment overloads `*` in a way that mixes “row-vector”
    and “column-vector” viewpoints.
  - In this SML port:
      - we represent integer matrices as row-major `int list list` (same as `IntMatrix.mat`)
      - for `project_K`, we interpret `.at`’s `v*B` as a row-vector multiply,
        implemented as `transpose(B) * v` with `v` treated as a column vector.
      - for `inject_K`, we interpret `.at`’s `B*v` as a standard matrix×column
        multiplication.
    This matches the informal explanations in `K.at` around the projection and
    injection matrices, and is consistent with the way `projection_to_K_matrix`
    is defined there as `^cocharacter_lattice_K`.

  Not yet implemented
  - The majority of `K.at` (K-root datum construction, `rho_K`, `rho_c`, K-type
    conversion, etc.) remains unported and will require additional RootDatum and
    “root classification w.r.t. KGB” primitives.
*)

structure K = struct
  type group = AtlasFFI.group
  type vec = int list
  type mat = IntMatrix.mat
  type ratvec = Lattice.ratvec

  (* Minimal SML analogue of `KHighestWeight = ((),KGBElt x, vec mu)`.
     In SML we must explicitly carry the group handle as well as the KGB index. *)
  type KHighestWeight = {g: group, x: int, mu: vec}

  fun k_highest_weight (g: group, x: int, mu: vec) : KHighestWeight =
    {g = g, x = x, mu = mu}

  fun failFFI where' = raise Fail ("K." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  (* Parse `atlas_group_kgb_involution_matrix_text` output:
     format: `rank a11 a12 ... a_rr` (row-major). *)
  fun parseInvolutionMatrixText (s: string) : mat =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("K.parseInvolutionMatrixText: bad int token: " ^ tok)
      val ns = List.map toInt (String.tokens Char.isSpace s)
    in
      case ns of
        r :: rest =>
          let
            val need = r * r
            val () = if r < 0 then raise Fail "K.parseInvolutionMatrixText: negative rank" else ()
            val () = if length rest = need then () else raise Fail "K.parseInvolutionMatrixText: truncated"
            fun row i = List.take (List.drop (rest, i * r), r)
          in
            List.tabulate (r, row)
          end
      | _ => raise Fail "K.parseInvolutionMatrixText: empty"
    end

  (* Cocharacter lattice for K_0: columns are a Z-basis of the +1 eigenspace of
     the relevant involution (distinguished involution for the group, or the
     KGB involution matrix for a chosen x). *)

  fun cocharacter_lattice_K_group (g: group) : mat =
    let
      val delta = IntMatrix.parseMatText (AtlasFFI.atlas_group_distinguished_involution_text g)
    in
      IntMatrix.eigenLattice (IntMatrix.transpose delta, 1)
    end
    handle Fail _ => failFFI "cocharacter_lattice_K_group"

  fun cocharacter_lattice_K_kgb (g: group, x: int) : mat =
    let
      val theta = parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
    in
      IntMatrix.eigenLattice (IntMatrix.transpose theta, 1)
    end
    handle Fail _ => failFFI "cocharacter_lattice_K_kgb"

  fun projection_to_K_matrix_group (g: group) : mat =
    IntMatrix.transpose (cocharacter_lattice_K_group g)

  fun projection_to_K_matrix_kgb (g: group, x: int) : mat =
    IntMatrix.transpose (cocharacter_lattice_K_kgb (g, x))

  (* Row-vector multiply `v * B` (implemented as `transpose(B) * v`). *)
  fun rowVecMulMatInt (v: vec, B: mat) : vec =
    IntMatrix.matVecMul (IntMatrix.transpose B, v)

  fun rowRatvecMulMat (v: ratvec, B: mat) : ratvec =
    let
      val nums2 = rowVecMulMatInt (#nums v, B)
    in
      Lattice.ratvecNormalize {den = #den v, nums = nums2}
    end

  fun ratvec_as_vec (v: ratvec) : vec =
    (case Lattice.ratvecToIntegral v of
       SOME xs => xs
     | NONE => raise Fail "K.ratvec_as_vec: not integral")

  fun project_K_vec_group (g: group, v: vec) : vec =
    rowVecMulMatInt (v, cocharacter_lattice_K_group g)

  fun project_K_vec_kgb (g: group, x: int, v: vec) : vec =
    rowVecMulMatInt (v, cocharacter_lattice_K_kgb (g, x))

  fun project_K_ratvec_group (g: group, v: ratvec) : vec =
    ratvec_as_vec (rowRatvecMulMat (v, cocharacter_lattice_K_group g))

  fun project_K_ratvec_kgb (g: group, x: int, v: ratvec) : vec =
    ratvec_as_vec (rowRatvecMulMat (v, cocharacter_lattice_K_kgb (g, x)))

  fun inject_K_ratvec_group (g: group, v: ratvec) : vec =
    let
      val inj = cocharacter_lattice_K_group g
      val w = Lattice.matVecMulRatvec inj v
    in
      ratvec_as_vec w
    end

  fun inject_K_ratvec_kgb (g: group, x: int, v: ratvec) : vec =
    let
      val inj = cocharacter_lattice_K_kgb (g, x)
      val w = Lattice.matVecMulRatvec inj v
    in
      ratvec_as_vec w
    end
end

