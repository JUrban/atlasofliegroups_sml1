use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/VertexData.sml";

(*
  File: atlas-scripts-sml/FPP_localDirac.sml

  Purpose
  - Initial SML port scaffold for `atlas-scripts/FPP_localDirac.at`.
  - The original `.at` file implements several sophisticated algorithms for
    enumerating and certifying all unitary representations in a “local FPP”
    slice attached to a fixed lowest K-type `(x,lambda)`, using:
      - affine involution symmetry on FPP vertices/faces
      - multiple “unitary-to-height” pruning levels (`to_ht.at`)
      - Dirac inequality / bottom-layer tests
      - cached expensive unitarity checks

  What is implemented so far (baseline)
  - The core *affine involution on FPP vertices* induced by `(theta(x),lambda)`:
        v |-> -theta*v + (I+theta)*lambda
    and the derived “local vertex list” consisting of:
      - fixed vertices `v` (where the involution fixes the vertex index), and
      - midpoints `(v+w)/2` for transposed vertex pairs `{v,w}`.
    This corresponds to the conceptual `Perm`/`mapAct` discussion at the top of
    `FPP_localDirac.at` and is a prerequisite for face-by-face local searches.

  Important limitation
  - This module does NOT yet implement the main `local_test_GEO_hash*` algorithms
    nor the FPP-local Dirac/bottom-layer pruning logic; those require additional
    bindings and supporting SML ports (e.g. richer `to_ht`/c-form helpers and
    face enumeration pipelines).

  Ownership and conventions
  - Parameters returned by constructors here are owned handles; callers must free
    them with `AtlasFFI.atlas_param_free`.
  - Rational vectors use `Lattice.ratvec = {den:int, nums:int list}` and are
    normalized where needed.
*)

structure FPP_localDirac = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec
  type mat = Lattice.mat

  (* ---------------------------------------------------------------------- *)
  (* Configuration flags (mirrors top-level `.at` globals; not yet used).    *)
  (* ---------------------------------------------------------------------- *)

  val prefer_to_hts : bool ref = ref true
  val deform_flag : bool ref = ref false
  val revert_flag : bool ref = ref false
  val step_flag : bool ref = ref false
  val real_flag : bool ref = ref false
  val def_flag : bool ref = ref false
  val short_hts_flag : bool ref = ref false
  val one_two_reverse_flag : bool ref = ref false
  val test_interrupt_flag : bool ref = ref true
  val long_out_flag : bool ref = ref false
  val bl_interrupt_flag : bool ref = ref false
  val bl_step_flag : bool ref = ref false
  val bl_step_count : int ref = ref 2
  val bl_step_size : int ref = ref 5
  val unip_flag : bool ref = ref true
  val more_after_flag : bool ref = ref false
  val min_after_flag : bool ref = ref false

  (* ---------------------------------------------------------------------- *)
  (* Small ratvec helpers.                                                   *)
  (* ---------------------------------------------------------------------- *)

  fun ratvecNeg (u: ratvec) : ratvec = Lattice.ratvecScale (u, ~1, 1)
  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec = Lattice.ratvecSub (u, ratvecNeg v)

  (* ---------------------------------------------------------------------- *)
  (* `.at`-style parameter constructor from gamma (infinitesimal character). *)
  (* ---------------------------------------------------------------------- *)

  (*
    In many `.at` FPP scripts, the “geometric point” in the FPP is the
    infinitesimal character `gamma`. For fixed `(x,lambda)` with involution
    `theta(x)`, the associated `nu` is:

        nu = gamma - (I+theta)*lambda/2

    This is the same construction used in `atlas-scripts-sml/F4_FPP_points_compute.sml`.
  *)
  fun parameter_x_lambda_gamma (g: group, x: int, lambda: ratvec, gamma: ratvec) : param =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val nu = Lattice.ratvecSub (gamma, thetaPlusHalf)
    in
      Representations.parameter (g, x, lambda, nu)
    end

  (* ---------------------------------------------------------------------- *)
  (* Local vertex data induced by the affine involution.                      *)
  (* ---------------------------------------------------------------------- *)

  type perm = int array

  type localFD =
    { Lvd: VertexData.t
    , perm: perm                 (* length = |vd|, values in [~1..|vd|-1] *)
    , pairs: (int * int) list    (* representatives (i<j) of transposed pairs *)
    , mapAct: int array          (* length = |vd|, values in [~1..|Lvd|-1] *)
    }

  (*
    Compute the affine involution on the *global* FPP vertex set `vd` induced by
    `(x,lambda)`:

      a(v) = -theta*v + (I+theta)*lambda

    and build the derived local vertex table consisting of fixed vertices and
    midpoints of paired vertices.

    This is the SML analogue of the conceptual `Perm`/`mapAct` construction in
    the header comments of `FPP_localDirac.at`.
  *)
  fun localFD_Lvd_simple (g: group, x: int, lambda: ratvec, vd: VertexData.t) : localFD =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlus = Lattice.matVecMulRatvec onePlus lambda

      val n = VertexData.size vd
      val perm = Array.array (n, ~1)

      fun affineImage (v: ratvec) : ratvec =
        ratvecAdd (ratvecNeg (Lattice.matVecMulRatvec theta v), thetaPlus)

      fun fill i =
        let
          val v = Array.sub (#verts vd, i)
          val w = Lattice.ratvecNormalize (affineImage v)
        in
          case VertexData.lookup (vd, w) of
            NONE => Array.update (perm, i, ~1)
          | SOME j => Array.update (perm, i, j)
        end

      val () = List.app fill (List.tabulate (n, fn i => i))

      (* Extract fixed indices and transposed pairs (i<j). *)
      fun scan i (fixed, pairs) =
        if i >= n then
          (List.rev fixed, List.rev pairs)
        else
          let
            val j = Array.sub (perm, i)
          in
            if j = ~1 then
              scan (i + 1) (fixed, pairs)
            else if j = i then
              scan (i + 1) (i :: fixed, pairs)
            else if j > i andalso Array.sub (perm, j) = i then
              scan (i + 1) (fixed, (i, j) :: pairs)
            else
              scan (i + 1) (fixed, pairs)
          end

      val (fixed, pairs) = scan 0 ([], [])

      fun midpoint (i, j) =
        Lattice.ratvecScale (ratvecAdd (Array.sub (#verts vd, i), Array.sub (#verts vd, j)), 1, 2)

      val localVerts =
        (List.map (fn i => Array.sub (#verts vd, i)) fixed)
        @ (List.map midpoint pairs)

      val Lvd = VertexData.fromList localVerts
      val mapAct = Array.array (n, ~1)

      fun fillMap (idxs: int list, start: int) : unit =
        let
          fun loop ([], _) = ()
            | loop (i :: rest, k) =
                (Array.update (mapAct, i, k);
                 loop (rest, k + 1))
        in
          loop (idxs, start)
        end

      val () = fillMap (fixed, 0)

      fun fillPairs (ps: (int * int) list, start: int) : unit =
        let
          fun loop ([], _) = ()
            | loop ((i, j) :: rest, k) =
                (Array.update (mapAct, i, k);
                 Array.update (mapAct, j, k);
                 loop (rest, k + 1))
        in
          loop (ps, start)
        end

      val () = fillPairs (pairs, length fixed)
    in
      {Lvd = Lvd, perm = perm, pairs = pairs, mapAct = mapAct}
    end
end
