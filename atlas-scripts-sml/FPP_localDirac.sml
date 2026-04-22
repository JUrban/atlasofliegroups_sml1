use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/FPPFaceKey.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/VertexData.sml";
use "atlas-scripts-sml/basic.sml";

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
  type face_key = int list

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

  (* Stable key `[den, nums...]` after normalization (for fast equality tests). *)
  fun ratvecKey (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

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
  (* Gamma slices for a fixed (x,lambda).                                    *)
  (* ---------------------------------------------------------------------- *)

  (*
    In the folded-FPP workflow, barycenters `gamma` are grouped by the affine
    constraint:

      (I+theta(x))*gamma = (I+theta(x))*lambda

    which is the same “key match” used in `F4_FPP_points_compute`.
  *)
  fun gammas_for_x_lambda (g: group, x: int, lambda: ratvec) : ratvec list =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val k = ratvecKey (Lattice.matVecMulRatvec onePlus lambda)
      val barycenters = FPP_barycenters_fold.barycenters_all g

      fun keep gamma =
        ratvecKey (Lattice.matVecMulRatvec onePlus gamma) = k
    in
      List.filter keep barycenters
    end

  (*
    Enumerate final standard parameters at a fixed `(x,lambda)` by running over
    the compatible barycenters `gamma` and constructing:

      p = normalise(parameter(G,x,lambda, nu = gamma - (I+theta)*lambda/2))

    Returns newly allocated parameters; caller must free them.
  *)
  fun params_for_x_lambda (g: group, x: int, lambda: ratvec) : param list =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)

      fun mk gamma =
        let
          val nu = Lattice.ratvecSub (gamma, thetaPlusHalf)
          val p0 = Representations.parameter (g, x, lambda, nu)
          val p1 = AtlasFFI.atlas_param_normalise p0
          val () = AtlasFFI.atlas_param_free p0
          val () =
            if p1 = Foreign.Memory.null then
              raise Fail ("params_for_x_lambda: normalise failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
        in
          if AtlasFFI.atlas_param_is_standard p1 = 1 andalso AtlasFFI.atlas_param_is_final p1 = 1 then
            SOME p1
          else
            (AtlasFFI.atlas_param_free p1; NONE)
        end
    in
      List.mapPartial mk (gammas_for_x_lambda (g, x, lambda))
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

  (* `.at`-style compact encoding of the involution action on vertices:
     fixed indices and transposed pairs (representatives with `i<j`). *)
  type perm2 = int list * (int * int) list

  fun perm2_of_localFD ({perm, pairs, ...}: localFD) : perm2 =
    let
      val n = Array.length perm
      fun loop i acc =
        if i >= n then
          List.rev acc
        else if Array.sub (perm, i) = i then
          loop (i + 1) (i :: acc)
        else
          loop (i + 1) acc
      val fixed = loop 0 []
    in
      (fixed, pairs)
    end

  (* Membership test for tiny face keys (length <= 5). *)
  fun memberInt (x: int, xs: int list) : bool = List.exists (fn y => y = x) xs

  (* Face stability under the partially-defined involution `perm`. *)
  fun faceStableUnderPerm (perm: perm, face: face_key) : bool =
    List.all
      (fn i =>
        let
          val j = Array.sub (perm, i)
        in
          j <> ~1 andalso memberInt (j, face)
        end)
      face

  (* Map a global face key to a local face key using `mapAct`, collapsing
     paired vertices by sorting+deduping indices. *)
  fun mapFaceKey (mapAct: int array, face: face_key) : face_key option =
    let
      fun mappedIndex i =
        let
          val k = Array.sub (mapAct, i)
        in
          if k < 0 then NONE else SOME k
        end
    in
      if List.exists (fn i => Array.sub (mapAct, i) < 0) face then
        NONE
      else
        SOME (Basic.sort_u (op <=) (List.mapPartial mappedIndex face))
    end

  type local_face =
    { gamma: ratvec
    , global_face: face_key
    , local_face: face_key
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

  (* Convenience wrapper: compute `(Lvd, Perm2, mapAct)` using the canonical
     folded-FPP vertex table for `g`. *)
  fun localFD_Lvd2_simple (g: group, x: int, lambda: ratvec) : (VertexData.t * perm2 * int array) =
    let
      val faceCtx = FPPFaceKey.create g
      val vd = #vd faceCtx
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val perm2 = perm2_of_localFD fd
    in
      (#Lvd fd, perm2, #mapAct fd)
    end

  (*
    Enumerate all *stable* local faces meeting the `(x,lambda)` slice, as a list
    of records that remember both:
      - `global_face`: vertex indices in the global folded-FPP vertex table
      - `local_face`: vertex indices in the derived local vertex table `Lvd`

    This is a preparatory step toward porting the `.at` algorithms that build
    unitary face graphs dimension-by-dimension.
  *)
  fun local_faces_for_x_lambda (g: group, x: int, lambda: ratvec) : local_face list =
    let
      val faceCtx = FPPFaceKey.create g
      val vd = #vd faceCtx
      val {perm, mapAct, ...} = localFD_Lvd_simple (g, x, lambda, vd)

      fun one gamma =
        (case FPPFaceKey.faceKeyOfGamma (faceCtx, gamma) of
           NONE => NONE
         | SOME gf =>
             if not (faceStableUnderPerm (perm, gf)) then
               NONE
             else
               (case mapFaceKey (mapAct, gf) of
                  NONE => NONE
                | SOME lf => SOME {gamma = gamma, global_face = gf, local_face = lf}))
    in
      List.mapPartial one (gammas_for_x_lambda (g, x, lambda))
    end

  (*
    Enumerate final parameters associated to the barycenters of *local* faces.

    Atlas `.at` correspondence
    - In `FPP_localDirac.at`, after computing local faces (as vertex-index lists
      in `Lvd`), the scripts form
        `p = parameter(x,lambda, face_bary(Lvd, faceVerts))`
      and then finalize and add all final terms to a unitary hash.

    This is a baseline SML analogue: it performs the parameter construction and
    calls `ParamFinals.finals` to obtain final parameters.

    Notes
    - This does not (yet) implement any of the unitarity pruning logic; it only
      enumerates the candidates.
    - For testing/performance, `*_limit` variants allow restricting the number
      of faces processed.
  *)

  fun params_for_local_faces_limit (g: group, x: int, lambda: ratvec, maxFaces: int) : param list =
    let
      val faceCtx = FPPFaceKey.create g
      val vd = #vd faceCtx
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val facesAll = local_faces_for_x_lambda (g, x, lambda)
      val faces = if maxFaces < 0 then facesAll else List.take (facesAll, Int.min (maxFaces, length facesAll))

      fun addFinal ((q, mult), acc) =
        if mult = 0 then
          (AtlasFFI.atlas_param_free q; acc)
        else
          AllParameters.addUniqueByEquivalent (q, acc)

      fun addFromFace ({local_face, ...}: local_face, acc: param list) : param list =
        let
          val gammaLocal = VertexData.face_bary (#Lvd fd, local_face)
          val p0 = parameter_x_lambda_gamma (g, x, lambda, gammaLocal)
          val p1 = AtlasFFI.atlas_param_normalise p0
          val () = AtlasFFI.atlas_param_free p0
          val () =
            if p1 = Foreign.Memory.null then
              raise Fail ("params_for_local_faces_limit: normalise failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val finals = ParamFinals.finals p1
          val () = AtlasFFI.atlas_param_free p1
        in
          List.foldl addFinal acc finals
        end
    in
      List.foldl addFromFace [] faces
    end

  fun params_for_local_faces (g: group, x: int, lambda: ratvec) : param list =
    params_for_local_faces_limit (g, x, lambda, ~1)

  (* Filter a list of owned parameters by exact Atlas unitarity, freeing the
     rejected ones. *)
  fun keep_unitary_and_free_rest (ps: param list) : param list =
    let
      fun step (p, acc) =
        if AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1 then
          p :: acc
        else
          (AtlasFFI.atlas_param_free p; acc)
    in
      List.rev (List.foldl step [] ps)
    end

  (* Baseline “local Dirac” output: unitary final parameters coming from local
     face barycenters, with optional face-count limit for testing. *)
  fun unitary_params_for_local_faces_limit (g: group, x: int, lambda: ratvec, maxFaces: int) : param list =
    keep_unitary_and_free_rest (params_for_local_faces_limit (g, x, lambda, maxFaces))

  fun unitary_params_for_local_faces (g: group, x: int, lambda: ratvec) : param list =
    unitary_params_for_local_faces_limit (g, x, lambda, ~1)

  (* Insert unitary parameters into a `ParamHash` (which clones on insertion),
     freeing the input handles. Returns the number of new insertions. *)
  fun add_unitary_params_to_hash (uhash: ParamHash.t, ps: param list) : int =
    let
      val sizeBefore = ParamHash.size uhash
      fun one p =
        (ignore (ParamHash.match uhash p);
         AtlasFFI.atlas_param_free p)
      val () = List.app one ps
      val sizeAfter = ParamHash.size uhash
    in
      sizeAfter - sizeBefore
    end

  (* Compute unitary local-face parameters and add them to `uhash`. *)
  fun add_unitary_from_local_faces_limit
    (g: group, x: int, lambda: ratvec, maxFaces: int, uhash: ParamHash.t) : int =
    add_unitary_params_to_hash (uhash, unitary_params_for_local_faces_limit (g, x, lambda, maxFaces))

  fun add_unitary_from_local_faces (g: group, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces_limit (g, x, lambda, ~1, uhash)

  (*
    Simple entry points (baseline)

    These mirror the *shape* of the `.at` entry points like `local_test_GEO`,
    but currently use only the baseline enumeration:
      local faces -> barycenters -> finalize -> exact `is_unitary`.

    They exist primarily as scaffolding for the ongoing port; callers should
    expect these to be less efficient than the `.at` originals.
  *)

  fun local_test_GEO_simple_limit (g: group, x: int, lambda: ratvec, maxFaces: int) : param list =
    unitary_params_for_local_faces_limit (g, x, lambda, maxFaces)

  fun local_test_GEO_simple (g: group, x: int, lambda: ratvec) : param list =
    unitary_params_for_local_faces (g, x, lambda)

  fun local_test_GEO_simple_into_hash_limit
    (g: group, x: int, lambda: ratvec, maxFaces: int, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces_limit (g, x, lambda, maxFaces, uhash)

  fun local_test_GEO_simple_into_hash (g: group, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces (g, x, lambda, uhash)
end
