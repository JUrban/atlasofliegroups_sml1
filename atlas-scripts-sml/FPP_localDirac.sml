use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/FPPFaceKey.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/F4_FPP_barycenters.sml";
use "atlas-scripts-sml/F4_FPP_vertices.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/BigUnitaryCache.sml";
use "atlas-scripts-sml/FPP_unipotents.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/VertexData.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/hash.sml";
use "atlas-scripts-sml/unity.sml";
use "atlas-scripts-sml/unity_fpp.sml";
use "atlas-scripts-sml/to_ht.sml";
use "atlas-scripts-sml/FaceClasses.sml";

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

  Current status (important limitations remain)
  - This module now contains an *exact* “GEO-hash2 style” pipeline for building
    stable local faces dimension-by-dimension and filtering them by Atlas’
    unitarity predicate, with caching to avoid repeated expensive checks.
  - It still lacks the `.at` script’s full suite of *height-stepped* `to_ht`
    pruning and the more delicate local-Dirac/bottom-layer step logic; those
    are expected to be the next major functional additions.

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
  type face_verts_khash_table = int list list list
  type graph_data = FaceClasses.graph_data

  (* ---------------------------------------------------------------------- *)
  (* Small cache for ToHT pruning (impure height of c-form).                 *)
  (* ---------------------------------------------------------------------- *)

  structure ImpureHeightCache = struct
    type t =
      { tbl: ParamHash.t
      , vals: int array ref
      }

    fun create bucketCount : t =
      { tbl = ParamHash.create bucketCount
      , vals = ref (Array.array (Int.max (16, bucketCount), ~3))
      }

    fun ensureCapacity ({vals, ...}: t) (need: int) =
      let
        val a = !vals
        val cap = Array.length a
      in
        if need <= cap then
          ()
        else
          let
            val newCap = Int.max (need, cap * 2)
            val b = Array.array (newCap, ~3)
            fun copy i =
              if i = cap then () else (Array.update (b, i, Array.sub (a, i)); copy (i + 1))
          in
            copy 0;
            vals := b
          end
      end

    fun freeAll (t: t) =
      (ParamHash.freeAll (#tbl t);
       #vals t := Array.array (Array.length (!(#vals t)), ~3))

    fun lookup ({tbl, vals}: t) (p: param) : int option =
      let
        val j = ParamHash.lookup tbl p
      in
        if j < 0 then NONE else SOME (Array.sub (!vals, j))
      end

    (* Insert a computed value if absent; returns the stored value. *)
    fun getOrInsert ({tbl, vals}: t) (p: param) (compute: unit -> int) : int =
      case lookup {tbl = tbl, vals = vals} p of
        SOME v => v
      | NONE =>
          let
            val sizeBefore = ParamHash.size tbl
            val j = ParamHash.match tbl p
            val sizeAfter = ParamHash.size tbl
            val v = compute ()
            val () =
              if sizeAfter = sizeBefore + 1 then
                (ensureCapacity {tbl = tbl, vals = vals} (j + 1);
                 Array.update (!vals, j, v))
              else
                ()
          in
            v
          end
  end

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

  (* Key for an already-normalized rational vector. *)
  fun ratvecKeyNormalized (u: ratvec) : int list =
    #den u :: #nums u

  (* ---------------------------------------------------------------------- *)
  (* Root-datum helpers used by pmax/pmin and height schedules.              *)
  (* ---------------------------------------------------------------------- *)

  (* Sum integer vectors (assumes nonempty, equal length). *)
  fun sumVecs (vs: int list list) : int list =
    (case vs of
       [] => []
     | v0 :: rest =>
         let
           val n = length v0
           val () = if List.all (fn v => length v = n) rest then () else raise Fail "sumVecs: ragged"
           fun add2 (xs, ys) = ListPair.mapEq (op +) (xs, ys)
         in
           List.foldl add2 v0 rest
         end)

  (* `2rho_check` as an integer coweight: sum of positive coroots. *)
  fun two_rho_check (g: group) : int list =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          raise Fail ("two_rho_check: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val tworc = sumVecs (RootDatum.posCorootsCols rd)
      val () = AtlasFFI.atlas_rootdatum_free rd
    in
      tworc
    end

  (*
    Group-level context

    Many `FPP_localDirac.at` computations are repeated for many `(x,lambda)`
    pairs inside a fixed group `g`. In particular, building the folded-FPP
    vertex table (and its `FPPFaceKey` inverse mapping) and enumerating all
    folded-FPP face barycenters are expensive. We therefore expose an explicit
    context that callers can reuse.
  *)

  type gamma_bucket =
    { onePlus: mat
    , keys: int list Hash.t
    , gammaIdxsByKey: int list array
    }

  type ctx =
    { g: group
    , faceCtx: FPPFaceKey.t
    , barycenters: ratvec list
    , barycentersA: ratvec array
    , gammaFaceTable: (int list * face_key) array
    , twoRhoCheck: int list option ref
    , gammaBucketsByX: gamma_bucket option array
    }

  fun create_ctx (g: group) : ctx =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
      val isSplit = AtlasFFI.atlas_group_is_split g = 1

      fun parseLeadingInt (s: string) : int option =
        case String.tokens Char.isSpace s of
          [] => NONE
        | tok :: _ => Int.fromString tok

      val nPosRoots =
        (case parseLeadingInt (AtlasFFI.atlas_group_posroots_text g) of
           SOME n => n
         | NONE => ~1)

      fun looksLikeF4s () : bool =
        isSplit andalso rank = 4 andalso kgbSize = 229 andalso nPosRoots = 24

      fun loadBarycenters (path: string) : ratvec list option =
        (case path of
           "atlas-scripts-sml/data/F4_FPP_barycenters.txt" =>
             (SOME (F4_FPP_barycenters.loadRatvecs ()) handle _ => NONE)
         | _ => NONE)

      fun loadVertices (path: string) : ratvec list option =
        (case path of
           "atlas-scripts-sml/data/F4_FPP_vertices.txt" =>
             (SOME (F4_FPP_vertices.loadRatvecs ()) handle _ => NONE)
         | _ => NONE)

      fun loadGammaFaceTable (path: string) : (int list * face_key) array option =
        let
          val ins = TextIO.openIn path
          fun parseLine line =
            let
              val toks = String.tokens (fn c => Char.isSpace c orelse c = #"|") line
              fun toInt tok =
                case Int.fromString tok of
                  SOME n => n
                | NONE => raise Fail ("bad int token: " ^ tok)
            in
              case toks of
                den :: n1 :: n2 :: n3 :: n4 :: k :: rest =>
                  let
                    val key = [toInt den, toInt n1, toInt n2, toInt n3, toInt n4]
                    val kk = toInt k
                    val () = if kk >= 1 andalso kk <= 5 then () else raise Fail "bad face key arity"
                    val () = if length rest = kk then () else raise Fail "bad face key arity/rest"
                    val fk = List.map toInt rest
                  in
                    SOME (key, fk)
                  end
              | _ => NONE
            end
          fun loop acc =
            case TextIO.inputLine ins of
              NONE => List.rev acc
            | SOME line =>
                (case parseLine line of
                   NONE => loop acc
                 | SOME e => loop (e :: acc))
          val entries = (loop [] handle e => (TextIO.closeIn ins; raise e))
          val () = TextIO.closeIn ins
        in
          SOME (Array.fromList entries)
        end
        handle _ => NONE

      fun computeGammaFaceTable (faceCtx: FPPFaceKey.t, barycenters: ratvec list) : (int list * face_key) array =
        let
          fun toEntry gamma =
            (case FPPFaceKey.faceKeyOfGamma (faceCtx, gamma) of
               NONE => raise Fail "FPP_localDirac.create_ctx: missing face key for barycenter"
             | SOME fk => (ratvecKey gamma, fk))
          val entries = List.map toEntry barycenters
          val entriesSorted = Basic.sort_by (fn (k, _) => k, Sort.rlex_leq) entries
        in
          Array.fromList entriesSorted
        end

      val (barycenters, gammaFaceTableOpt) =
        if looksLikeF4s () then
          let
            val bary =
              (case loadBarycenters "atlas-scripts-sml/data/F4_FPP_barycenters.txt" of
                 SOME xs => xs
               | NONE => FPP_barycenters_fold.barycenters_all g)
            val tabOpt = loadGammaFaceTable "atlas-scripts-sml/data/f4s_gamma_face_table.txt"
          in
            (bary, tabOpt)
          end
        else
          (FPP_barycenters_fold.barycenters_all g, NONE)

      val faceCtx =
        if looksLikeF4s () andalso Option.isSome gammaFaceTableOpt then
          (case loadVertices "atlas-scripts-sml/data/F4_FPP_vertices.txt" of
             SOME verts => FPPFaceKey.createVerticesOnlyFromList verts
           | NONE => FPPFaceKey.createVerticesOnly g)
        else
          FPPFaceKey.create g

      val gammaFaceTable =
        case gammaFaceTableOpt of
          SOME tab => tab
        | NONE => computeGammaFaceTable (faceCtx, barycenters)

    in
      { g = g
      , faceCtx = faceCtx
      , barycenters = barycenters
      , barycentersA = Array.fromList barycenters
      , gammaFaceTable = gammaFaceTable
      , twoRhoCheck = ref NONE
      , gammaBucketsByX = Array.array (kgbSize, NONE)
      }
    end

  (* ---------------------------------------------------------------------- *)
  (* Cached gamma buckets for fixed x (theta).                               *)
  (* ---------------------------------------------------------------------- *)

  fun gamma_bucket_for_x_ctx (c: ctx, x: int) : gamma_bucket =
    case Array.sub (#gammaBucketsByX c, x) of
      SOME b => b
    | NONE =>
        let
          val g = #g c
          val rank = AtlasFFI.atlas_group_rank g
          val theta =
            AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
          val onePlus = Lattice.matAdd (Lattice.identity rank, theta)

          val baryA = #barycentersA c
          val n = Array.length baryA

          val keysH = Hash.make_hash_reserve ({hash_code = Hash.hash_code_vec, eq = (op =) }, n)
          val gammaIdxsByKey : int list array ref = ref (Array.array (Int.max (16, n div 4), []))

          fun ensureCap need =
            let
              val a = !gammaIdxsByKey
              val cap = Array.length a
            in
              if need <= cap then
                ()
              else
                let
                  val newCap = Int.max (need, cap * 2)
                  val b = Array.array (newCap, ([]: int list))
                  fun copy i =
                    if i = cap then () else (Array.update (b, i, Array.sub (a, i)); copy (i + 1))
                in
                  copy 0;
                  gammaIdxsByKey := b
                end
            end

          fun addGamma i =
            if i = n then
              ()
            else
              let
                val gamma = Array.sub (baryA, i)
                val key = ratvecKeyNormalized (Lattice.matVecMulRatvec onePlus gamma)
                val sizeBefore = #size keysH ()
                val j = #match keysH key
                val sizeAfter = #size keysH ()
                val () = if sizeAfter = sizeBefore + 1 then ensureCap (j + 1) else ()
                val a = !gammaIdxsByKey
              in
                Array.update (a, j, i :: Array.sub (a, j));
                addGamma (i + 1)
              end

          val () = addGamma 0

          val b =
            { onePlus = onePlus
            , keys = keysH
            , gammaIdxsByKey = !gammaIdxsByKey
            }
          val () = Array.update (#gammaBucketsByX c, x, SOME b)
        in
          b
        end

  fun global_face_of_gamma_ctx (c: ctx, gamma: ratvec) : face_key =
    let
      val key = ratvecKey gamma
      val tab = #gammaFaceTable c
      val n = Array.length tab
      fun keyAt i = #1 (Array.sub (tab, i))
      fun faceAt i = #2 (Array.sub (tab, i))
      fun keyEq (a: int list, b: int list) : bool = Sort.rlex_leq (a, b) andalso Sort.rlex_leq (b, a)
      val i = Basic.binary_search_first (fn j => Sort.rlex_leq (key, keyAt j), 0, n)
    in
      if i < n andalso keyEq (keyAt i, key) then faceAt i else raise Fail "global_face_of_gamma_ctx: missing key"
    end

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
  (* When true, attempt `Unity`/`to_ht`-based early-disproof in cached unitary checks. *)
  val to_ht_prune_flag : bool ref = ref false
  (* When true, seed graph classes from a known-unitary `ParamHash` when provided. *)
  val seed_known_unitaries_flag : bool ref = ref true
  (* When true, attempt a `.at`-style seed: a class is known unitary if all final
     terms of its representative face barycenter standard module are present in
     the known unitary hash. This is more accurate but can be expensive. *)
  val seed_known_unitaries_by_finals_flag : bool ref = ref false
  (* When true, compute a per-dimension height schedule from `pmax` using
     LKTs-based `next_heights`; otherwise use a cheap arithmetic schedule. *)
  val ht_schedule_from_pmax_flag : bool ref = ref false
  (* Default arithmetic schedule step (when `ht_schedule_from_pmax_flag=false`). *)
  val ht_schedule_step : int ref = ref 5

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
  fun gammas_for_x_lambda_ctx (c: ctx, x: int, lambda: ratvec) : ratvec list =
    let
      val b = gamma_bucket_for_x_ctx (c, x)
      val k = ratvecKeyNormalized (Lattice.matVecMulRatvec (#onePlus b) lambda)
      val j = Hash.lookup (#keys b) k
      val idxs = if j < 0 then [] else Array.sub (#gammaIdxsByKey b, j)
      val baryA = #barycentersA c
    in
      List.map (fn i => Array.sub (baryA, i)) idxs
    end

  fun gammas_for_x_lambda (g: group, x: int, lambda: ratvec) : ratvec list =
    gammas_for_x_lambda_ctx (create_ctx g, x, lambda)

  (*
    Enumerate final standard parameters at a fixed `(x,lambda)` by running over
    the compatible barycenters `gamma` and constructing:

      p = normalise(parameter(G,x,lambda, nu = gamma - (I+theta)*lambda/2))

    Returns newly allocated parameters; caller must free them.
  *)
  fun params_for_x_lambda_ctx (c: ctx, x: int, lambda: ratvec) : param list =
    let
      val g = #g c
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
      List.mapPartial mk (gammas_for_x_lambda_ctx (c, x, lambda))
    end

  fun params_for_x_lambda (g: group, x: int, lambda: ratvec) : param list =
    params_for_x_lambda_ctx (create_ctx g, x, lambda)

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
  fun localFD_Lvd2_simple_ctx (c: ctx, x: int, lambda: ratvec) : (VertexData.t * perm2 * int array) =
    let
      val vd = #vd (#faceCtx c)
      val g = #g c
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val perm2 = perm2_of_localFD fd
    in
      (#Lvd fd, perm2, #mapAct fd)
    end

  fun localFD_Lvd2_simple (g: group, x: int, lambda: ratvec) : (VertexData.t * perm2 * int array) =
    localFD_Lvd2_simple_ctx (create_ctx g, x, lambda)

  (* ---------------------------------------------------------------------- *)
  (* `.at` helpers: pmax / pmin                                              *)
  (* ---------------------------------------------------------------------- *)

  fun twoRhoCheck_ctx (c: ctx) : int list =
    case !(#twoRhoCheck c) of
      SOME v => v
    | NONE =>
        let
          val v = two_rho_check (#g c)
          val () = #twoRhoCheck c := SOME v
        in
          v
        end

  (* Pair an integer coweight with a rational weight, returning (num,den) without normalization. *)
  fun pairingNumDen (coweight: int list, w: ratvec) : IntInf.int * int =
    let
      val nums = #nums w
      val den = #den w
      val () = if length coweight = length nums then () else raise Fail "pairingNumDen: length mismatch"
      val s =
        List.foldl (op +) (0:IntInf.int)
          (ListPair.mapEq (fn (a, b) => IntInf.fromInt a * IntInf.fromInt b) (coweight, nums))
    in
      (s, den)
    end

  fun fracGT ((aNum, aDen): IntInf.int * int, (bNum, bDen): IntInf.int * int) : bool =
    aNum * IntInf.fromInt bDen > bNum * IntInf.fromInt aDen

  fun fracLT ((aNum, aDen): IntInf.int * int, (bNum, bDen): IntInf.int * int) : bool =
    aNum * IntInf.fromInt bDen < bNum * IntInf.fromInt aDen

  (* `.at` `pmax(x,lambda,Lvd)`: pick the vertex `nu` in `Lvd.list` maximizing `<2rho_check,nu>`. *)
  fun pmax_twoRhoCheck (twoRhoCheck: int list) (g: group, x: int, lambda: ratvec, Lvd: VertexData.t) : param =
    let
      val vs = VertexData.toList Lvd
      val () = if null vs then raise Fail "pmax: empty local vertex list" else ()

      fun best (v, (bestV, bestScore)) =
        let
          val sc = pairingNumDen (twoRhoCheck, v)
        in
          if fracGT (sc, bestScore) then (v, sc) else (bestV, bestScore)
        end

      val v0 = hd vs
      val init = (v0, pairingNumDen (twoRhoCheck, v0))
      val (vBest, _) = List.foldl best init (tl vs)
    in
      Representations.parameter (g, x, lambda, vBest)
    end

  (* `.at` `pmin(x,lambda,Lvd)`: pick the vertex `nu` in `Lvd.list` minimizing `<2rho_check,nu>`. *)
  fun pmin_twoRhoCheck (twoRhoCheck: int list) (g: group, x: int, lambda: ratvec, Lvd: VertexData.t) : param =
    let
      val vs = VertexData.toList Lvd
      val () = if null vs then raise Fail "pmin: empty local vertex list" else ()

      fun best (v, (bestV, bestScore)) =
        let
          val sc = pairingNumDen (twoRhoCheck, v)
        in
          if fracLT (sc, bestScore) then (v, sc) else (bestV, bestScore)
        end

      val v0 = hd vs
      val init = (v0, pairingNumDen (twoRhoCheck, v0))
      val (vBest, _) = List.foldl best init (tl vs)
    in
      Representations.parameter (g, x, lambda, vBest)
    end

  (* ---------------------------------------------------------------------- *)
  (* Face-graph class data (`localGraphK` family; partial port).             *)
  (* ---------------------------------------------------------------------- *)

  (*
    Build SCC class data for a `[[FaceVertsKHash]]`-style table.

    This mirrors `.at`:
      `localGraphK([[FaceVertsKHash]] LFvertsK)`

    Returns:
    - `gd`: SCC classes + condensed adjacency
    - `iucl`: per-class downward-closure lists (class ids reachable “downwards”)
    - `classListByFace`: per-dimension lists mapping face index -> class id
  *)
  fun localGraphK_FDKH (fd: face_verts_khash_table) : graph_data * int list array * int list list =
    let
      val gd = FaceClasses.up_data_FDKH fd
      val classListByFace = FaceClasses.class_lists_FDKH (fd, gd)
      val iucl = FaceClasses.full_down_classes gd
    in
      (gd, iucl, classListByFace)
    end

  (*
    Variant where reverse edges are added only when K-type polynomials agree
    up to a truncation height `level`, using `polHash` indices stored in `fd`.

    This mirrors `.at`:
      `localGraphK([[FaceVertsKHash]] FDKH, KTypePol_hash pol_hash, int level)`

    Parameters
    - `polHash`: owning `KTypePolHash` used for face K-character indices
    - `level`: truncation height (>= 0 enables truncation mode)
    - `redShift`: 0 when no `red_count` coord is present; 1 if present
  *)
  fun localGraphK_FDKH_kpol
    (fd: face_verts_khash_table, polHash: KTypePolHash.t, level: int, redShift: int)
    : graph_data * int list array * int list list =
    let
      val gd = FaceClasses.up_data_FDKH_kpol (fd, polHash, level, redShift)
      val classListByFace = FaceClasses.class_lists_FDKH (fd, gd)
      val iucl = FaceClasses.full_down_classes gd
    in
      (gd, iucl, classListByFace)
    end

  fun pmax (g: group, x: int, lambda: ratvec, Lvd: VertexData.t) : param =
    pmax_twoRhoCheck (two_rho_check g) (g, x, lambda, Lvd)

  fun pmin (g: group, x: int, lambda: ratvec, Lvd: VertexData.t) : param =
    pmin_twoRhoCheck (two_rho_check g) (g, x, lambda, Lvd)

  (* Compute a per-dimension truncation-height schedule for `(x,lambda)` that
     can be used for `ToHT` pruning. Returns a list of length `rank(g)+1`.

     Notes
     - This is a porting scaffold: the `.at` scripts compute tailored height
       schedules using several heuristics (`short_hts`, `next_heights`, etc.).
     - The schedule affects performance only (it is used for safe early
       disproof); exact unitarity is still checked at the end. *)
  fun slice_hts_for_x_lambda_ctx (c: ctx, x: int, lambda: ratvec) : int list =
    let
      val g = #g c
      val r = AtlasFFI.atlas_group_rank g
      val need = r + 1

      fun extendToNeed (xs: int list) : int list =
        if length xs >= need then
          List.take (xs, need)
        else
          (case List.rev xs of
             [] =>
               let
                 val step = Int.max (1, !ht_schedule_step)
               in
                 List.tabulate (need, fn i => (i + 1) * step)
               end
           | last :: _ =>
               xs @ List.tabulate (need - length xs, fn j => last + j + 1))
    in
      if !ht_schedule_from_pmax_flag then
        let
          val vd = #vd (#faceCtx c)
          val fd = localFD_Lvd_simple (g, x, lambda, vd)
          val p0 = pmax_twoRhoCheck (twoRhoCheck_ctx c) (g, x, lambda, #Lvd fd)
          val p1 = AtlasFFI.atlas_param_normalise p0
          val () = AtlasFFI.atlas_param_free p0
          val () =
            if p1 = Foreign.Memory.null then
              raise Fail ("slice_hts_for_x_lambda: normalise failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val hs = (UnityFPP.next_heights_lkts (p1, need) handle _ => Unity.next_heights (p1, need))
          val () = AtlasFFI.atlas_param_free p1
        in
          extendToNeed hs
        end
      else
        let
          val step = Int.max (1, !ht_schedule_step)
          val hs = List.tabulate (need, fn i => (i + 1) * step)
        in
          extendToNeed hs
        end
    end

  (*
    Enumerate all *stable* local faces meeting the `(x,lambda)` slice, as a list
    of records that remember both:
      - `global_face`: vertex indices in the global folded-FPP vertex table
      - `local_face`: vertex indices in the derived local vertex table `Lvd`

    This is a preparatory step toward porting the `.at` algorithms that build
    unitary face graphs dimension-by-dimension.
  *)
  fun local_faces_for_x_lambda_ctx (c: ctx, x: int, lambda: ratvec) : local_face list =
    let
      val g = #g c
      val faceCtx = #faceCtx c
      val vd = #vd faceCtx
      val {perm, mapAct, ...} = localFD_Lvd_simple (g, x, lambda, vd)

      fun one gamma =
        let
          val gf = global_face_of_gamma_ctx (c, gamma)
        in
          if not (faceStableUnderPerm (perm, gf)) then
            NONE
          else
            (case mapFaceKey (mapAct, gf) of
               NONE => NONE
             | SOME lf => SOME {gamma = gamma, global_face = gf, local_face = lf})
        end
    in
      List.mapPartial one (gammas_for_x_lambda_ctx (c, x, lambda))
    end

  fun local_faces_for_x_lambda (g: group, x: int, lambda: ratvec) : local_face list =
    local_faces_for_x_lambda_ctx (create_ctx g, x, lambda)

  (* Group local faces by their local dimension (length-1), returning an array
     of length `rank(g)+1`. *)
  fun local_faces_for_x_lambda_by_dim_ctx (c: ctx, x: int, lambda: ratvec) : face_key list array =
    let
      val g = #g c
      val r = AtlasFFI.atlas_group_rank g
      val a = Array.array (r + 1, ([]: face_key list))
      fun add ({local_face, ...}: local_face) =
        let
          val d = length local_face - 1
        in
          if d < 0 orelse d > r then
            ()
          else
            Array.update (a, d, local_face :: Array.sub (a, d))
        end
      val () = List.app add (local_faces_for_x_lambda_ctx (c, x, lambda))
    in
      a
    end

  fun local_faces_for_x_lambda_by_dim (g: group, x: int, lambda: ratvec) : face_key list array =
    local_faces_for_x_lambda_by_dim_ctx (create_ctx g, x, lambda)

  (* ---------------------------------------------------------------------- *)
  (* `[[FaceVertsKHash]]` face tables (K-character hash indices).            *)
  (* ---------------------------------------------------------------------- *)

  (*
    Build a `[[FaceVertsKHash]]`-style face table from a per-dimension list of
    *local* face keys, computing:
    - K-character index via `atlas_param_full_deform` stored in `polHash`
    - number of Langlands quotients via `ParamFinals.finalsCount`

    This is a pragmatic stepping stone toward porting the `.at` K-graph-based
    `local_testK_hash*` machinery; it is not yet performance-tuned.

    Parameters
    - `facesByDim`: local faces by dimension (indices refer to `Lvd`)
    - `maxPerDim`: if >= 0, truncate each dimension list to this many faces

    Representation
    - Each entry for a d-face is `[v0,...,vd, kcharIdx, langlandsCount]`.
  *)
  fun face_table_khash_from_facesByDim_limit
    (g: group, x: int, lambda: ratvec, thetaPlusHalf: ratvec, Lvd: VertexData.t,
     facesByDim: face_key list array, maxPerDim: int, polHash: KTypePolHash.t)
    : face_verts_khash_table =
    let
      fun takeLim xs =
        if maxPerDim < 0 then xs else List.take (xs, Int.min (maxPerDim, length xs))

      fun faceKcharAndLQ (face: face_key) : int * int =
        let
          val gammaLocal = VertexData.face_bary (Lvd, face)
          val nu = Lattice.ratvecSub (gammaLocal, thetaPlusHalf)
          val p0 = Representations.parameter (g, x, lambda, nu)
          val p1 = AtlasFFI.atlas_param_normalise p0
          val () = AtlasFFI.atlas_param_free p0
          val () =
            if p1 = Foreign.Memory.null then
              raise Fail ("face_table_khash: normalise failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()

          val langCount = ParamFinals.finalsCount p1

          val pol = AtlasFFI.atlas_param_full_deform p1
          val () =
            if pol = Foreign.Memory.null then
              raise Fail ("face_table_khash: full_deform failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val idx = KTypePolHash.match polHash pol
          val () = KTypePol.free pol
          val () = AtlasFFI.atlas_param_free p1
        in
          (idx, langCount)
        end

      fun rowForDim d =
        let
          val faces = takeLim (Array.sub (facesByDim, d))
          fun entry face =
            let
              val (idx, lq) = faceKcharAndLQ face
            in
              face @ [idx, lq]
            end
        in
          List.map entry faces
        end

      val dims = Array.length facesByDim
    in
      List.tabulate (dims, rowForDim)
    end

  (*
    Convenience wrapper: compute local faces for `(x,lambda)` and build a
    `[[FaceVertsKHash]]`-style table with K-character hash indices.
  *)
  fun local_face_table_khash_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxPerDim: int, polHash: KTypePolHash.t)
    : face_verts_khash_table =
    let
      val g = #g c
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val {Lvd, ...} = localFD_Lvd_simple (g, x, lambda, vd)
      val facesByDim = local_faces_for_x_lambda_by_dim_ctx (c, x, lambda)
    in
      face_table_khash_from_facesByDim_limit (g, x, lambda, thetaPlusHalf, Lvd, facesByDim, maxPerDim, polHash)
    end

  fun local_face_table_khash_limit
    (g: group, x: int, lambda: ratvec, maxPerDim: int, polHash: KTypePolHash.t)
    : face_verts_khash_table =
    local_face_table_khash_limit_ctx (create_ctx g, x, lambda, maxPerDim, polHash)

  (* Codimension-1 subfaces of a (simplex) face key, obtained by deleting each vertex once. *)
  fun codim1_subfaces (face: face_key) : face_key list =
    let
      val n = length face
      fun dropAt i = List.take (face, i) @ List.drop (face, i + 1)
    in
      List.tabulate (n, dropAt)
    end

  type unitary_cache = BigUnitaryCache.t

  (* Exact unitarity test for a local face: check all final terms at the face barycenter. *)
  fun face_is_unitary_exact_thetaPlusHalf
    (g: group, x: int, lambda: ratvec, thetaPlusHalf: ratvec, Lvd: VertexData.t, face: face_key) : bool =
    let
      val gammaLocal = VertexData.face_bary (Lvd, face)
      val nu = Lattice.ratvecSub (gammaLocal, thetaPlusHalf)
      val p0 = Representations.parameter (g, x, lambda, nu)
      val p1 = AtlasFFI.atlas_param_normalise p0
      val () = AtlasFFI.atlas_param_free p0
      val () =
        if p1 = Foreign.Memory.null then
          raise Fail ("face_is_unitary_exact: normalise failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      if AtlasFFI.atlas_param_is_final p1 = 1 then
        let
          val ok = AtlasFFI.atlas_param_is_hermitian p1 = 1 andalso AtlasFFI.atlas_param_is_unitary p1 = 1
          val () = AtlasFFI.atlas_param_free p1
        in
          ok
        end
      else
        let
          val finals = ParamFinals.finals p1
          val () = AtlasFFI.atlas_param_free p1
          fun okTerm (q, mult) =
            if mult = 0 then
              (AtlasFFI.atlas_param_free q; true)
            else
              let
                val ok = AtlasFFI.atlas_param_is_hermitian q = 1 andalso AtlasFFI.atlas_param_is_unitary q = 1
                val () = AtlasFFI.atlas_param_free q
              in
                ok
              end
        in
          List.all okTerm finals
        end
    end

  fun face_is_unitary_exact (g: group, x: int, lambda: ratvec, Lvd: VertexData.t, face: face_key) : bool =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
    in
      face_is_unitary_exact_thetaPlusHalf (g, x, lambda, thetaPlusHalf, Lvd, face)
    end

  (*
    Baseline face filtering (dimension-by-dimension)

    - Start with all local faces (grouped by dimension).
    - Keep a face in dimension `d` if:
        (1) it passes `face_is_unitary_exact` at its barycenter, and
        (2) all codim-1 subfaces are kept in dimension `d-1`.

    This mirrors the “faces from unitary subfaces” pattern used in the `.at`
    GEO-hash algorithms, but uses exact unitarity rather than `to_ht` pruning.
  *)
  fun unitary_local_faces_by_dim_exact_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : face_key list array =
    let
      val g = #g c
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val Lvd = #Lvd fd

      val facesByDim0 = local_faces_for_x_lambda_by_dim_ctx (c, x, lambda)
      val r = Array.length facesByDim0 - 1

      fun takeLim xs =
        if maxFacesPerDim < 0 then xs else List.take (xs, Int.min (maxFacesPerDim, length xs))

      val facesByDim = Array.tabulate (r + 1, fn d => takeLim (Array.sub (facesByDim0, d)))

      val kept = Array.array (r + 1, ([]: face_key list))

      fun keepDim0 () =
        let
          val vs = Array.sub (facesByDim, 0)
          val ks =
            List.filter (fn face => face_is_unitary_exact_thetaPlusHalf (g, x, lambda, thetaPlusHalf, Lvd, face)) vs
        in
          Array.update (kept, 0, ks)
        end

      fun faceSetOf (xs: face_key list) : face_key Hash.t =
        Hash.make_hash_data ({hash_code = Hash.hash_code_vec, eq = (op =) }, xs)

      fun keepDim d =
        let
          val prev = Array.sub (kept, d - 1)
          val prevSet = faceSetOf prev
          fun subfacesOk face =
            List.all (fn sf => Hash.lookup prevSet sf >= 0) (codim1_subfaces face)
          fun ok face =
            subfacesOk face andalso face_is_unitary_exact_thetaPlusHalf (g, x, lambda, thetaPlusHalf, Lvd, face)
          val fs = Array.sub (facesByDim, d)
          val ks = List.filter ok fs
        in
          Array.update (kept, d, ks)
        end

      val () = keepDim0 ()
      val () = List.app keepDim (List.tabulate (r, fn i => i + 1))
    in
      kept
    end

  fun unitary_local_faces_by_dim_exact_ctx (c: ctx, x: int, lambda: ratvec) : face_key list array =
    unitary_local_faces_by_dim_exact_limit_ctx (c, x, lambda, ~1)

  (* Cached variant: uses `BigUnitaryCache` for the final-parameter unitary checks. *)
  fun face_is_unitary_exact_thetaPlusHalf_cached
    (cache: unitary_cache)
    (allowPrune: bool)
    (g: group, x: int, lambda: ratvec, thetaPlusHalf: ratvec, Lvd: VertexData.t, face: face_key) : bool =
    let
      val gammaLocal = VertexData.face_bary (Lvd, face)
      val nu = Lattice.ratvecSub (gammaLocal, thetaPlusHalf)
      val p0 = Representations.parameter (g, x, lambda, nu)
      val p1 = AtlasFFI.atlas_param_normalise p0
      val () = AtlasFFI.atlas_param_free p0
      val () =
        if p1 = Foreign.Memory.null then
          raise Fail ("face_is_unitary_exact_cached: normalise failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      if AtlasFFI.atlas_param_is_final p1 = 1 then
        let
          val ok =
            if AtlasFFI.atlas_param_is_hermitian p1 <> 1 then
              false
            else if allowPrune then
              BigUnitaryCache.check_unitary_prune_equal_rank_steps cache (g, !bl_step_count, !bl_step_size) p1
            else
              BigUnitaryCache.check_unitary cache p1
          val () = AtlasFFI.atlas_param_free p1
        in
          ok
        end
      else
        let
          val finals = ParamFinals.finals p1
          val () = AtlasFFI.atlas_param_free p1
          fun okTerm (q, mult) =
            if mult = 0 then
              (AtlasFFI.atlas_param_free q; true)
            else if AtlasFFI.atlas_param_is_hermitian q <> 1 then
              (AtlasFFI.atlas_param_free q; false)
            else
              let
                val ok =
                  if allowPrune then
                    BigUnitaryCache.check_unitary_prune_equal_rank_steps cache (g, !bl_step_count, !bl_step_size) q
                  else
                    BigUnitaryCache.check_unitary cache q
                val () = AtlasFFI.atlas_param_free q
              in
                ok
              end
        in
          List.all okTerm finals
        end
    end

  fun unitary_local_faces_by_dim_exact_limit_ctx_cached
    (cache: unitary_cache)
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : face_key list array =
    let
      val g = #g c
      val allowPrune = !to_ht_prune_flag andalso Representations.is_equal_rank g
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val Lvd = #Lvd fd

      val facesByDim0 = local_faces_for_x_lambda_by_dim_ctx (c, x, lambda)
      val r = Array.length facesByDim0 - 1

      fun takeLim xs =
        if maxFacesPerDim < 0 then xs else List.take (xs, Int.min (maxFacesPerDim, length xs))

      val facesByDim = Array.tabulate (r + 1, fn d => takeLim (Array.sub (facesByDim0, d)))
      val kept = Array.array (r + 1, ([]: face_key list))

      fun faceSetOf (xs: face_key list) : face_key Hash.t =
        Hash.make_hash_data ({hash_code = Hash.hash_code_vec, eq = (op =) }, xs)

      fun keepDim0 () =
        let
          val vs = Array.sub (facesByDim, 0)
          val ks =
            List.filter
              (fn face => face_is_unitary_exact_thetaPlusHalf_cached cache allowPrune (g, x, lambda, thetaPlusHalf, Lvd, face))
              vs
        in
          Array.update (kept, 0, ks)
        end

      fun keepDim d =
        let
          val prev = Array.sub (kept, d - 1)
          val prevSet = faceSetOf prev
          fun subfacesOk face =
            List.all (fn sf => Hash.lookup prevSet sf >= 0) (codim1_subfaces face)
          fun ok face =
            subfacesOk face
            andalso face_is_unitary_exact_thetaPlusHalf_cached cache allowPrune (g, x, lambda, thetaPlusHalf, Lvd, face)
          val fs = Array.sub (facesByDim, d)
          val ks = List.filter ok fs
        in
          Array.update (kept, d, ks)
        end

      val () = keepDim0 ()
      val () = List.app keepDim (List.tabulate (r, fn i => i + 1))
    in
      kept
    end

  (* ---------------------------------------------------------------------- *)
  (* Minimal `local_testK_hash` analogue (exact unitarity + K-character index) *)
  (* ---------------------------------------------------------------------- *)

  (*
    A deliberately simplified, functional analogue of the `.at`
    `local_testK_hash*` family:
    - uses the exact cached unitary predicate on barycenter parameters
    - enforces closure (a face kept only if its codim-1 subfaces are kept)
    - then attaches K-character hash indices / #Langlands quotients

    It returns a `[[FaceVertsKHash]]`-style table containing only the kept faces.
  *)
  fun local_testK_hash_simple_limit_ctx_cached
    (cache: unitary_cache)
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t)
    : face_verts_khash_table =
    let
      val g = #g c
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val {Lvd, ...} = localFD_Lvd_simple (g, x, lambda, vd)
      val keptFacesByDim = unitary_local_faces_by_dim_exact_limit_ctx_cached cache (c, x, lambda, maxFacesPerDim)
    in
      face_table_khash_from_facesByDim_limit (g, x, lambda, thetaPlusHalf, Lvd, keptFacesByDim, ~1, polHash)
    end

  fun local_testK_hash_simple_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t)
    : face_verts_khash_table =
    let
      val cache = BigUnitaryCache.create 4096
      val res = local_testK_hash_simple_limit_ctx_cached cache (c, x, lambda, maxFacesPerDim, polHash)
      val () = BigUnitaryCache.freeAll cache
    in
      res
    end

  fun local_testK_hash_simple_limit
    (g: group, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t)
    : face_verts_khash_table =
    local_testK_hash_simple_limit_ctx (create_ctx g, x, lambda, maxFacesPerDim, polHash)

  (* ---------------------------------------------------------------------- *)
  (* Seeding helpers: known unitary classes from a `ParamHash`.              *)
  (* ---------------------------------------------------------------------- *)

  (* Parameters in `uhash` that match the given `(x,lambda)` (by normalized texts). *)
  fun params_in_hash_x_lambda (uhash: ParamHash.t, x: int, lambda: ratvec) : param list =
    let
      val lambdaKey = ratvecKey lambda
      fun ok p =
        AtlasFFI.atlas_param_x p = x
        andalso
        let
          val lamP = Lattice.ratvecNormalize (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_lambda_text p))
        in
          ratvecKeyNormalized lamP = lambdaKey
        end
    in
      List.filter ok (ParamHash.list uhash)
    end

  (* Build a lookup `gammaKey -> classId` for a `[[FaceVertsKHash]]` table with local barycenters. *)
  fun gammaKey_to_classId
    (Lvd: VertexData.t, fd: face_verts_khash_table, classListByFace: int list list)
    : int list -> int option =
    let
      val totalFaces = FaceClasses.size_fd fd
      val h =
        Hash.make_hash_reserve ({hash_code = Hash.hash_code_vec, eq = (op =) }, totalFaces)
      val clsA = Array.array (Int.max (1, totalFaces), ~1)

      fun insertEntry (d: int, j: int, entry: int list, cid: int) =
        let
          val verts = List.take (entry, d + 1)
          val gamma = VertexData.face_bary (Lvd, verts)
          val key = ratvecKey gamma
          val idx = #match h key
          val () = Array.update (clsA, idx, cid)
        in
          ()
        end

      fun loopD d =
        if d >= length fd then
          ()
        else
          let
            val row = List.nth (fd, d)
            val clRow = List.nth (classListByFace, d)
            fun loopJ (j, []) = ()
              | loopJ (j, e :: rest) =
                  (insertEntry (d, j, e, List.nth (clRow, j)); loopJ (j + 1, rest))
          in
            loopJ (0, row);
            loopD (d + 1)
          end

      val () = loopD 0
    in
      fn key =>
        let
          val idx = Hash.lookup h key
        in
          if idx < 0 then NONE
          else
            let
              val cid = Array.sub (clsA, idx)
            in
              if cid < 0 then NONE else SOME cid
            end
        end
    end

  (* Class ids corresponding to known unitary parameters (if their local barycenter face is present in `fd`). *)
  fun known_unitary_class_ids
    (uhash: ParamHash.t, g: group, x: int, lambda: ratvec, thetaPlusHalf: ratvec,
     Lvd: VertexData.t, fd: face_verts_khash_table, classListByFace: int list list)
    : int list =
    let
      val ps = params_in_hash_x_lambda (uhash, x, lambda)
      val lookup = gammaKey_to_classId (Lvd, fd, classListByFace)

      fun one (p, acc) =
        let
          val nu = Lattice.ratvecNormalize (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_nu_text p))
          val gammaLocal = ratvecAdd (nu, thetaPlusHalf)
          val key = ratvecKey gammaLocal
        in
          case lookup key of
            NONE => acc
          | SOME cid => cid :: acc
        end

      val cids = List.foldl one [] ps
    in
      Sort.sort_u (op <=) cids
    end

  (* `.at`-style seed predicate: for the standard module attached to a face barycenter,
     check that every nonzero-multiplicity final term is already in `knownUhash`. *)
  fun face_finals_all_in_hash
    (knownUhash: ParamHash.t)
    (g: group, x: int, lambda: ratvec, thetaPlusHalf: ratvec, Lvd: VertexData.t, face: face_key)
    : bool =
    let
      val gammaLocal = VertexData.face_bary (Lvd, face)
      val nu = Lattice.ratvecSub (gammaLocal, thetaPlusHalf)
      val p0 = Representations.parameter (g, x, lambda, nu)
      val p1 = AtlasFFI.atlas_param_normalise p0
      val () = AtlasFFI.atlas_param_free p0
      val () =
        if p1 = Foreign.Memory.null then
          raise Fail ("face_finals_all_in_hash: normalise failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()

      fun okFinal q =
        let
          val ok = ParamHash.contains knownUhash q
          val () = AtlasFFI.atlas_param_free q
        in
          ok
        end
    in
      if AtlasFFI.atlas_param_is_final p1 = 1 then
        okFinal p1
      else
        let
          val finals = ParamFinals.finals p1
          val () = AtlasFFI.atlas_param_free p1
          fun okTerm (q, mult) =
            if mult = 0 then (AtlasFFI.atlas_param_free q; true) else okFinal q
        in
          List.all okTerm finals
        end
    end

  (*
    Graph-based variant closer in spirit to `.at` `local_testK_hash`:
    - build a `[[FaceVertsKHash]]` table for *all* stable local faces (bounded by `maxFacesPerDim`)
    - build the SCC class graph (`localGraphK_FDKH` or the truncation variant)
    - test one representative face per class (exact unitary predicate)
    - propagate unitary downward and nonunitary upward in the class DAG
    - keep only faces belonging to unitary classes

    Notes
    - This is still much simpler than the full `.at` code: it does not yet
      incorporate bottom-layer/Dirac tests or special-case “wiggle” logic.
    - It is intended as a functional staging point for further ports.

    Parameters
    - `level`: if < 0 use full tail-equality for reverse edges; if >= 0 use
      KTypePol truncation to this height when deciding reverse edges.
  *)
  fun local_testK_hash_graph_exact_limit_ctx_cached_known
    (cache: unitary_cache)
    (knownUhashOpt: ParamHash.t option)
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int)
    : face_verts_khash_table =
    let
      val g = #g c
      val allowPrune = !to_ht_prune_flag andalso Representations.is_equal_rank g
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val {Lvd, ...} = localFD_Lvd_simple (g, x, lambda, vd)

      val fd = local_face_table_khash_limit_ctx (c, x, lambda, maxFacesPerDim, polHash)

      val (gd, down, classListByFace) =
        if level < 0 then
          localGraphK_FDKH fd
        else
          localGraphK_FDKH_kpol (fd, polHash, level, 0)

      val up = FaceClasses.full_up_classes gd
      val {classes, ...} = gd
      val k = length classes

      datatype st = U | N | Q
      val status = Array.array (k, Q)

      fun setU j =
        case Array.sub (status, j) of
          N => raise Fail "local_testK_hash_graph_exact: conflicting class statuses (U vs N)"
        | _ => Array.update (status, j, U)

      fun setN j =
        case Array.sub (status, j) of
          U => raise Fail "local_testK_hash_graph_exact: conflicting class statuses (N vs U)"
        | _ => Array.update (status, j, N)

      fun markDown (cid: int) =
        List.app setU (Array.sub (down, cid))

      fun markUp (cid: int) =
        List.app setN (Array.sub (up, cid))

      fun faceKeyOfNode (node: int) : face_key =
        let
          val (d, j) = FaceClasses.coords_f fd node
          val entry = List.nth (List.nth (fd, d), j)
        in
          List.take (entry, d + 1)
        end

      fun testClass (cid: int) : bool =
        let
          val nodes = List.nth (classes, cid)
          val node = case nodes of [] => raise Fail "local_testK_hash_graph_exact: empty SCC class" | n :: _ => n
          val face = faceKeyOfNode node
        in
          face_is_unitary_exact_thetaPlusHalf_cached cache allowPrune (g, x, lambda, thetaPlusHalf, Lvd, face)
        end

      fun loop cid =
        if cid = k then
          ()
        else
          (case Array.sub (status, cid) of
             Q =>
               let
                 val seeded =
                   if !seed_known_unitaries_by_finals_flag then
                     (case knownUhashOpt of
                        NONE => false
                      | SOME uhash =>
                          ((let
                              val nodes = List.nth (classes, cid)
                              val node =
                                case nodes of
                                  [] => raise Fail "local_testK_hash_graph_exact: empty SCC class"
                                | n :: _ => n
                              val face = faceKeyOfNode node
                              val ok = face_finals_all_in_hash uhash (g, x, lambda, thetaPlusHalf, Lvd, face)
                            in
                              if ok then (markDown cid; true) else false
                            end) handle _ => false))
                   else
                     false
               in
                 if seeded then
                   loop (cid + 1)
                 else if testClass cid then
                   (markDown cid; loop (cid + 1))
                 else
                   (markUp cid; loop (cid + 1))
               end
           | _ => loop (cid + 1))

      (* Optional seeding: if a class contains a face whose barycenter parameter is in a
         known unitary `ParamHash`, mark it unitary and propagate downward. *)
      fun seedKnown (uhash: ParamHash.t) =
        let
          val cids = known_unitary_class_ids (uhash, g, x, lambda, thetaPlusHalf, Lvd, fd, classListByFace)
        in
          List.app markDown cids
        end

      val () =
        if !seed_known_unitaries_flag then
          (case knownUhashOpt of NONE => () | SOME uhash => seedKnown uhash)
        else
          ()
      val () = loop 0

      fun keepFace (d: int, j: int) : bool =
        let
          val cid = List.nth (List.nth (classListByFace, d), j)
        in
          Array.sub (status, cid) = U
        end

      fun filterDim d =
        let
          val row = List.nth (fd, d)
          fun pick (j, entry) = if keepFace (d, j) then SOME entry else NONE
        in
          List.mapPartial pick (ListPair.zipEq (List.tabulate (length row, fn i => i), row))
        end
    in
      List.tabulate (length fd, filterDim)
    end

  fun local_testK_hash_graph_exact_limit_ctx_cached
    (cache: unitary_cache)
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int)
    : face_verts_khash_table =
    local_testK_hash_graph_exact_limit_ctx_cached_known cache NONE (c, x, lambda, maxFacesPerDim, polHash, level)

  fun local_testK_hash_graph_exact_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int)
    : face_verts_khash_table =
    let
      val cache = BigUnitaryCache.create 4096
      val res = local_testK_hash_graph_exact_limit_ctx_cached cache (c, x, lambda, maxFacesPerDim, polHash, level)
      val () = BigUnitaryCache.freeAll cache
    in
      res
    end

  fun local_testK_hash_graph_exact_limit_ctx_known
    (knownUhash: ParamHash.t)
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int)
    : face_verts_khash_table =
    let
      val cache = BigUnitaryCache.create 4096
      val res = local_testK_hash_graph_exact_limit_ctx_cached_known cache (SOME knownUhash) (c, x, lambda, maxFacesPerDim, polHash, level)
      val () = BigUnitaryCache.freeAll cache
    in
      res
    end

  fun local_testK_hash_graph_exact_limit
    (g: group, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int)
    : face_verts_khash_table =
    local_testK_hash_graph_exact_limit_ctx (create_ctx g, x, lambda, maxFacesPerDim, polHash, level)

  fun local_testK_hash_graph_exact_limit_known
    (knownUhash: ParamHash.t)
    (g: group, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int)
    : face_verts_khash_table =
    local_testK_hash_graph_exact_limit_ctx_known knownUhash (create_ctx g, x, lambda, maxFacesPerDim, polHash, level)

  (* ---------------------------------------------------------------------- *)
  (* To-height pruning variant (safe early disproof, then exact check later) *)
  (* ---------------------------------------------------------------------- *)

  (* Safe early disproof for a local face at height bound `ht`.
     Returns `true` on exceptions (conservative). *)
  fun face_is_unitary_to_ht_thetaPlusHalf_cached
    (cache: ImpureHeightCache.t)
    (g: group, x: int, lambda: ratvec, thetaPlusHalf: ratvec, Lvd: VertexData.t, face: face_key, ht: int) : bool =
    let
      val gammaLocal = VertexData.face_bary (Lvd, face)
      val nu = Lattice.ratvecSub (gammaLocal, thetaPlusHalf)
      val p0 = Representations.parameter (g, x, lambda, nu)
      val p1 = AtlasFFI.atlas_param_normalise p0
      val () = AtlasFFI.atlas_param_free p0
      val () =
        if p1 = Foreign.Memory.null then
          raise Fail ("face_is_unitary_to_ht: normalise failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()

      val rank = AtlasFFI.atlas_group_rank g

      fun cFormImpureHeight (q: param) : int =
        let
          val cf = AtlasFFI.atlas_param_c_form_irreducible q
          val () =
            if cf = Foreign.Memory.null then
              raise Fail ("face_is_unitary_to_ht: c_form_irreducible failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val d = KTypePol.impureHeight (cf, rank)
          val () = KTypePol.free cf
        in
          d
        end

      fun okFinal q =
        AtlasFFI.atlas_param_is_hermitian q = 1
        andalso
        let
          val d = ImpureHeightCache.getOrInsert cache q (fn () => cFormImpureHeight q)
        in
          d = ~1 orelse d > ht
        end
    in
      if AtlasFFI.atlas_param_is_final p1 = 1 then
        let
          val ok = okFinal p1 handle _ => true
          val () = AtlasFFI.atlas_param_free p1
        in
          ok
        end
      else
        let
          val finals = ParamFinals.finals p1
          val () = AtlasFFI.atlas_param_free p1
          fun okTerm (q, mult) =
            if mult = 0 then
              (AtlasFFI.atlas_param_free q; true)
            else
              let
                val ok = (okFinal q handle _ => true)
                val () = AtlasFFI.atlas_param_free q
              in
                ok
              end
        in
          List.all okTerm finals
        end
    end

  fun face_is_unitary_to_ht_thetaPlusHalf
    (g: group, x: int, lambda: ratvec, thetaPlusHalf: ratvec, Lvd: VertexData.t, face: face_key, ht: int) : bool =
    let
      val cache = ImpureHeightCache.create 1024
      val ok = (face_is_unitary_to_ht_thetaPlusHalf_cached cache (g, x, lambda, thetaPlusHalf, Lvd, face, ht)
                handle _ => true)
      val () = ImpureHeightCache.freeAll cache
    in
      ok
    end

  (*
    Two-stage graph-based test:
    (1) Safe early-disproof via `ToHT` (c-form purity) at height `ht`, marking
        classes proven nonunitary and propagating upward.
    (2) Exact unitary test for the remaining classes, with unitary/nonunitary
        propagation as in `local_testK_hash_graph_exact_*`.

    This is closer to the `.at` intent: spend cheap time eliminating obviously
    nonunitary classes, then do the expensive exact unitary checks on the rest.
  *)
  fun local_testK_hash_graph_to_ht_then_exact_limit_ctx_known
    (knownUhashOpt: ParamHash.t option)
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int, ht: int)
    : face_verts_khash_table =
    let
      val g = #g c
      val allowPrune = !to_ht_prune_flag andalso Representations.is_equal_rank g
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val {Lvd, ...} = localFD_Lvd_simple (g, x, lambda, vd)

      val fd = local_face_table_khash_limit_ctx (c, x, lambda, maxFacesPerDim, polHash)

      val (gd, down, classListByFace) =
        if level < 0 then
          localGraphK_FDKH fd
        else
          localGraphK_FDKH_kpol (fd, polHash, level, 0)

      val up = FaceClasses.full_up_classes gd
      val {classes, ...} = gd
      val k = length classes

      datatype st = U | N | Q
      val status = Array.array (k, Q)

      fun setU j =
        case Array.sub (status, j) of
          N => raise Fail "local_testK_hash_graph_to_ht_then_exact: conflicting class statuses (U vs N)"
        | _ => Array.update (status, j, U)

      fun setN j =
        case Array.sub (status, j) of
          U => raise Fail "local_testK_hash_graph_to_ht_then_exact: conflicting class statuses (N vs U)"
        | _ => Array.update (status, j, N)

      fun markDown (cid: int) =
        List.app setU (Array.sub (down, cid))

      fun markUp (cid: int) =
        List.app setN (Array.sub (up, cid))

      fun faceKeyOfNode (node: int) : face_key =
        let
          val (d, j) = FaceClasses.coords_f fd node
          val entry = List.nth (List.nth (fd, d), j)
        in
          List.take (entry, d + 1)
        end

      fun testToHt (impCache: ImpureHeightCache.t) (cid: int) : bool =
        let
          val nodes = List.nth (classes, cid)
          val node =
            case nodes of
              [] => raise Fail "local_testK_hash_graph_to_ht_then_exact: empty SCC class"
            | n :: _ => n
          val face = faceKeyOfNode node
        in
          face_is_unitary_to_ht_thetaPlusHalf_cached impCache (g, x, lambda, thetaPlusHalf, Lvd, face, ht)
        end

      fun testExact (uCache: unitary_cache) (cid: int) : bool =
        let
          val nodes = List.nth (classes, cid)
          val node =
            case nodes of
              [] => raise Fail "local_testK_hash_graph_to_ht_then_exact: empty SCC class"
            | n :: _ => n
          val face = faceKeyOfNode node
        in
          face_is_unitary_exact_thetaPlusHalf_cached uCache allowPrune (g, x, lambda, thetaPlusHalf, Lvd, face)
        end

      val impCache = ImpureHeightCache.create 2048
      val uCache = BigUnitaryCache.create 4096

      fun passToHt cid =
        if cid = k then
          ()
        else
          (case Array.sub (status, cid) of
             Q =>
               let
                 val seeded =
                   if !seed_known_unitaries_by_finals_flag then
                     (case knownUhashOpt of
                        NONE => false
                      | SOME uhash =>
                          ((let
                              val nodes = List.nth (classes, cid)
                              val node =
                                case nodes of
                                  [] => raise Fail "local_testK_hash_graph_to_ht_then_exact: empty SCC class"
                                | n :: _ => n
                              val face = faceKeyOfNode node
                              val ok = face_finals_all_in_hash uhash (g, x, lambda, thetaPlusHalf, Lvd, face)
                            in
                              if ok then (markDown cid; true) else false
                            end) handle _ => false))
                   else
                     false
               in
                 if seeded then
                   passToHt (cid + 1)
                 else if testToHt impCache cid then
                   passToHt (cid + 1)
                 else
                   (markUp cid; passToHt (cid + 1))
               end
           | _ => passToHt (cid + 1))

      fun passExact cid =
        if cid = k then
          ()
        else
          (case Array.sub (status, cid) of
             Q =>
               if testExact uCache cid then
                 (markDown cid; passExact (cid + 1))
               else
                 (markUp cid; passExact (cid + 1))
           | _ => passExact (cid + 1))

      fun seedKnown (uhash: ParamHash.t) =
        let
          val cids = known_unitary_class_ids (uhash, g, x, lambda, thetaPlusHalf, Lvd, fd, classListByFace)
        in
          List.app markDown cids
        end

      val () =
        if !seed_known_unitaries_flag then
          (case knownUhashOpt of NONE => () | SOME uhash => seedKnown uhash)
        else
          ()
      val () = (passToHt 0; passExact 0)
      val () = (ImpureHeightCache.freeAll impCache; BigUnitaryCache.freeAll uCache)

      fun keepFace (d: int, j: int) : bool =
        let
          val cid = List.nth (List.nth (classListByFace, d), j)
        in
          Array.sub (status, cid) = U
        end

      fun filterDim d =
        let
          val row = List.nth (fd, d)
          fun pick (j, entry) = if keepFace (d, j) then SOME entry else NONE
        in
          List.mapPartial pick (ListPair.zipEq (List.tabulate (length row, fn i => i), row))
        end
    in
      List.tabulate (length fd, filterDim)
    end

  fun local_testK_hash_graph_to_ht_then_exact_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int, ht: int)
    : face_verts_khash_table =
    local_testK_hash_graph_to_ht_then_exact_limit_ctx_known NONE (c, x, lambda, maxFacesPerDim, polHash, level, ht)

  fun local_testK_hash_graph_to_ht_then_exact_limit
    (g: group, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int, ht: int)
    : face_verts_khash_table =
    local_testK_hash_graph_to_ht_then_exact_limit_ctx (create_ctx g, x, lambda, maxFacesPerDim, polHash, level, ht)

  fun local_testK_hash_graph_to_ht_then_exact_limit_known
    (knownUhash: ParamHash.t)
    (g: group, x: int, lambda: ratvec, maxFacesPerDim: int, polHash: KTypePolHash.t, level: int, ht: int)
    : face_verts_khash_table =
    local_testK_hash_graph_to_ht_then_exact_limit_ctx_known (SOME knownUhash)
      (create_ctx g, x, lambda, maxFacesPerDim, polHash, level, ht)

  (* Dimension-by-dimension face filtering using `ToHT` at per-dimension bounds.
     This is intended as a pruning pre-pass; callers should still do an exact
     `is_unitary` verification on the resulting barycenter parameters. *)
  fun unitary_local_faces_by_dim_to_ht_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : face_key list array =
    let
      val g = #g c
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val Lvd = #Lvd fd

      val hts = slice_hts_for_x_lambda_ctx (c, x, lambda)
      fun htForDim d = List.nth (hts, d) handle _ => ~1

      val facesByDim0 = local_faces_for_x_lambda_by_dim_ctx (c, x, lambda)
      val r = Array.length facesByDim0 - 1

      fun takeLim xs =
        if maxFacesPerDim < 0 then xs else List.take (xs, Int.min (maxFacesPerDim, length xs))

      val facesByDim = Array.tabulate (r + 1, fn d => takeLim (Array.sub (facesByDim0, d)))

      val kept = Array.array (r + 1, ([]: face_key list))
      val cache = ImpureHeightCache.create 4096

      fun faceSetOf (xs: face_key list) : face_key Hash.t =
        Hash.make_hash_data ({hash_code = Hash.hash_code_vec, eq = (op =) }, xs)

      fun keepDim0 () =
        let
          val vs = Array.sub (facesByDim, 0)
          val ht0 = htForDim 0
          val ks =
            List.filter
              (fn face => face_is_unitary_to_ht_thetaPlusHalf_cached cache (g, x, lambda, thetaPlusHalf, Lvd, face, ht0))
              vs
        in
          Array.update (kept, 0, ks)
        end

      fun keepDim d =
        let
          val prev = Array.sub (kept, d - 1)
          val prevSet = faceSetOf prev
          fun subfacesOk face =
            List.all (fn sf => Hash.lookup prevSet sf >= 0) (codim1_subfaces face)
          val htd = htForDim d
          fun ok face =
            subfacesOk face
            andalso face_is_unitary_to_ht_thetaPlusHalf_cached cache (g, x, lambda, thetaPlusHalf, Lvd, face, htd)
          val fs = Array.sub (facesByDim, d)
          val ks = List.filter ok fs
        in
          Array.update (kept, d, ks)
        end

      val () = keepDim0 ()
      val () = List.app keepDim (List.tabulate (r, fn i => i + 1))
      val () = ImpureHeightCache.freeAll cache
    in
      kept
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

  fun params_for_local_faces_limit_ctx (c: ctx, x: int, lambda: ratvec, maxFaces: int) : param list =
    let
      val g = #g c
      val vd = #vd (#faceCtx c)
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val facesAll = local_faces_for_x_lambda_ctx (c, x, lambda)
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
        in
          if AtlasFFI.atlas_param_is_final p1 = 1 then
            AllParameters.addUniqueByEquivalent (p1, acc)
          else
            let
              val finals = ParamFinals.finals p1
              val () = AtlasFFI.atlas_param_free p1
            in
              List.foldl addFinal acc finals
            end
        end
    in
      List.foldl addFromFace [] faces
    end

  fun params_for_local_faces_limit (g: group, x: int, lambda: ratvec, maxFaces: int) : param list =
    params_for_local_faces_limit_ctx (create_ctx g, x, lambda, maxFaces)

  fun params_for_local_faces (g: group, x: int, lambda: ratvec) : param list =
    params_for_local_faces_limit (g, x, lambda, ~1)

  (* Enumerate final parameters from barycenters of a given list of local faces. *)
  fun params_for_given_local_faces_thetaPlusHalf
    (g: group, x: int, lambda: ratvec, thetaPlusHalf: ratvec, Lvd: VertexData.t, faces: face_key list) : param list =
    let
      fun addFinal ((q, mult), acc) =
        if mult = 0 then
          (AtlasFFI.atlas_param_free q; acc)
        else
          AllParameters.addUniqueByEquivalent (q, acc)

      fun addFromFace (face: face_key, acc: param list) : param list =
        let
          val gammaLocal = VertexData.face_bary (Lvd, face)
          val nu = Lattice.ratvecSub (gammaLocal, thetaPlusHalf)
          val p0 = Representations.parameter (g, x, lambda, nu)
          val p1 = AtlasFFI.atlas_param_normalise p0
          val () = AtlasFFI.atlas_param_free p0
          val () =
            if p1 = Foreign.Memory.null then
              raise Fail ("params_for_given_local_faces: normalise failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
        in
          if AtlasFFI.atlas_param_is_final p1 = 1 then
            AllParameters.addUniqueByEquivalent (p1, acc)
          else
            let
              val finals = ParamFinals.finals p1
              val () = AtlasFFI.atlas_param_free p1
            in
              List.foldl addFinal acc finals
            end
        end
    in
      List.foldl addFromFace [] faces
    end

  (* Exact unitary parameters from barycenters of faces kept by the ToHT pass.
     Uses `BigUnitaryCache` for the final exact `is_unitary` checks. *)
  fun unitary_params_from_unitary_faces_by_dim_to_ht_exact_limit_ctx_cached
    (cache: unitary_cache)
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    let
      val g = #g c
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val Lvd = #Lvd fd

      val keptFacesByDim = unitary_local_faces_by_dim_to_ht_limit_ctx (c, x, lambda, maxFacesPerDim)
      val faces = List.concat (Array.foldr (op ::) [] keptFacesByDim)
      val ps0 = params_for_given_local_faces_thetaPlusHalf (g, x, lambda, thetaPlusHalf, Lvd, faces)

      fun keepUnitaryCached (ps: param list) : param list =
        let
          fun step (p, acc) =
            if AtlasFFI.atlas_param_is_hermitian p = 1 andalso BigUnitaryCache.check_unitary cache p then
              p :: acc
            else
              (AtlasFFI.atlas_param_free p; acc)
        in
          List.rev (List.foldl step [] ps)
        end
    in
      keepUnitaryCached ps0
    end

  (* Baseline “GEO-hash2” style output: use face-closure filtering to select
     unitary faces, then return unitary final parameters at their barycenters. *)
  fun unitary_params_from_unitary_faces_by_dim_exact_limit_ctx_cached
    (cache: unitary_cache)
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    let
      val g = #g c
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlusHalf = Lattice.ratvecScale (Lattice.matVecMulRatvec onePlus lambda, 1, 2)
      val vd = #vd (#faceCtx c)
      val fd = localFD_Lvd_simple (g, x, lambda, vd)
      val Lvd = #Lvd fd

      val keptFacesByDim = unitary_local_faces_by_dim_exact_limit_ctx_cached cache (c, x, lambda, maxFacesPerDim)
      val faces = List.concat (Array.foldr (op ::) [] keptFacesByDim)
      val ps0 = params_for_given_local_faces_thetaPlusHalf (g, x, lambda, thetaPlusHalf, Lvd, faces)
      fun keepUnitary (ps: param list) : param list =
        let
          fun step (p, acc) =
            if AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1 then
              p :: acc
            else
              (AtlasFFI.atlas_param_free p; acc)
        in
          List.rev (List.foldl step [] ps)
        end
    in
      keepUnitary ps0
    end

  fun unitary_params_from_unitary_faces_by_dim_exact_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    let
      val g = #g c
      val cache = BigUnitaryCache.create 1024
      val () =
        if !unip_flag then
          FPP_unipotents.insert_unipotents_to_paramhash (g, BigUnitaryCache.uhash cache)
        else
          ()
      val ps =
        unitary_params_from_unitary_faces_by_dim_exact_limit_ctx_cached cache (c, x, lambda, maxFacesPerDim)
      val () = BigUnitaryCache.freeAll cache
    in
      ps
    end

  (* Exact unitary parameters from the ToHT-pruned face sets. *)
  fun unitary_params_from_unitary_faces_by_dim_to_ht_exact_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    let
      val g = #g c
      val cache = BigUnitaryCache.create 1024
      val () =
        if !unip_flag then
          FPP_unipotents.insert_unipotents_to_paramhash (g, BigUnitaryCache.uhash cache)
        else
          ()
      val ps =
        unitary_params_from_unitary_faces_by_dim_to_ht_exact_limit_ctx_cached cache (c, x, lambda, maxFacesPerDim)
      val () = BigUnitaryCache.freeAll cache
    in
      ps
    end

  (* `.at`-style naming: a baseline analogue of `local_test_GEO_hash2` that
     uses closure-filtered unitary faces, with exact Atlas unitary checks. *)
  fun local_test_GEO_hash2_exact_limit_ctx (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    unitary_params_from_unitary_faces_by_dim_exact_limit_ctx (c, x, lambda, maxFacesPerDim)

  fun local_test_GEO_hash2_exact_ctx (c: ctx, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash2_exact_limit_ctx (c, x, lambda, ~1)

  fun local_test_GEO_hash2_exact_limit (g: group, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    local_test_GEO_hash2_exact_limit_ctx (create_ctx g, x, lambda, maxFacesPerDim)

  fun local_test_GEO_hash2_exact (g: group, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash2_exact_limit (g, x, lambda, ~1)

  fun local_test_GEO_hash2_exact_into_hash_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, uhash: ParamHash.t) : int =
    let
      val ps = local_test_GEO_hash2_exact_limit_ctx (c, x, lambda, maxFacesPerDim)
      val sizeBefore = ParamHash.size uhash
      fun one p =
        (ignore (ParamHash.match uhash p);
         AtlasFFI.atlas_param_free p)
      val () = List.app one ps
      val sizeAfter = ParamHash.size uhash
    in
      sizeAfter - sizeBefore
    end

  fun local_test_GEO_hash2_exact_into_hash_ctx (c: ctx, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    local_test_GEO_hash2_exact_into_hash_limit_ctx (c, x, lambda, ~1, uhash)

  (*
    Compatibility wrappers (names from `FPP_localDirac.at`)

    The `.at` code distinguishes several variants:
      - `local_test_GEO_hash2` / `local_test_GEO_hash` / `local_test_GEO_hash_dumb`
      - `local_test_GEO`

    For now, we provide *compiling* SML entry points with the same base names,
    selecting between:
      - the exact-baseline `*_hash2_exact*` routines above, and
      - the ToHT-pruning pipeline (`unitary_local_faces_by_dim_to_ht_*`)
    depending on the flag `prefer_to_hts`.

    This mirrors the `.at` scripts’ intent: use ToHT as a safe early-disproof
    pass for speed, but still verify exact unitarity before returning results.
  *)

  fun local_test_GEO_hash2_limit_ctx (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    if !prefer_to_hts then
      unitary_params_from_unitary_faces_by_dim_to_ht_exact_limit_ctx (c, x, lambda, maxFacesPerDim)
    else
      local_test_GEO_hash2_exact_limit_ctx (c, x, lambda, maxFacesPerDim)

  fun local_test_GEO_hash2_ctx (c: ctx, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash2_limit_ctx (c, x, lambda, ~1)

  fun local_test_GEO_hash2_limit (g: group, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    local_test_GEO_hash2_limit_ctx (create_ctx g, x, lambda, maxFacesPerDim)

  fun local_test_GEO_hash2 (g: group, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash2_limit (g, x, lambda, ~1)

  fun local_test_GEO_hash2_into_hash_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, uhash: ParamHash.t) : int =
    let
      val ps = local_test_GEO_hash2_limit_ctx (c, x, lambda, maxFacesPerDim)
      val sizeBefore = ParamHash.size uhash
      fun one p =
        (ignore (ParamHash.match uhash p);
         AtlasFFI.atlas_param_free p)
      val () = List.app one ps
      val sizeAfter = ParamHash.size uhash
    in
      sizeAfter - sizeBefore
    end

  fun local_test_GEO_hash2_into_hash_ctx (c: ctx, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    local_test_GEO_hash2_into_hash_limit_ctx (c, x, lambda, ~1, uhash)

  fun local_test_GEO_hash2_into_hash_limit
    (g: group, x: int, lambda: ratvec, maxFacesPerDim: int, uhash: ParamHash.t) : int =
    local_test_GEO_hash2_into_hash_limit_ctx (create_ctx g, x, lambda, maxFacesPerDim, uhash)

  fun local_test_GEO_hash2_into_hash (g: group, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    local_test_GEO_hash2_into_hash_limit (g, x, lambda, ~1, uhash)

  fun local_test_GEO_hash_limit_ctx (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    local_test_GEO_hash2_limit_ctx (c, x, lambda, maxFacesPerDim)

  fun local_test_GEO_hash_ctx (c: ctx, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash2_ctx (c, x, lambda)

  fun local_test_GEO_hash_limit (g: group, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    local_test_GEO_hash2_limit (g, x, lambda, maxFacesPerDim)

  fun local_test_GEO_hash (g: group, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash2 (g, x, lambda)

  fun local_test_GEO_limit (g: group, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    local_test_GEO_hash2_limit (g, x, lambda, maxFacesPerDim)

  fun local_test_GEO (g: group, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash2 (g, x, lambda)

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

  fun unitary_params_for_local_faces_limit_ctx (c: ctx, x: int, lambda: ratvec, maxFaces: int) : param list =
    keep_unitary_and_free_rest (params_for_local_faces_limit_ctx (c, x, lambda, maxFaces))

  fun unitary_params_for_local_faces (g: group, x: int, lambda: ratvec) : param list =
    unitary_params_for_local_faces_limit (g, x, lambda, ~1)

  fun unitary_params_for_local_faces_ctx (c: ctx, x: int, lambda: ratvec) : param list =
    unitary_params_for_local_faces_limit_ctx (c, x, lambda, ~1)

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

  fun add_unitary_from_local_faces_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFaces: int, uhash: ParamHash.t) : int =
    add_unitary_params_to_hash (uhash, unitary_params_for_local_faces_limit_ctx (c, x, lambda, maxFaces))

  fun add_unitary_from_local_faces (g: group, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces_limit (g, x, lambda, ~1, uhash)

  fun add_unitary_from_local_faces_ctx (c: ctx, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces_limit_ctx (c, x, lambda, ~1, uhash)

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

  fun local_test_GEO_simple_limit_ctx (c: ctx, x: int, lambda: ratvec, maxFaces: int) : param list =
    unitary_params_for_local_faces_limit_ctx (c, x, lambda, maxFaces)

  fun local_test_GEO_simple (g: group, x: int, lambda: ratvec) : param list =
    unitary_params_for_local_faces (g, x, lambda)

  fun local_test_GEO_simple_ctx (c: ctx, x: int, lambda: ratvec) : param list =
    unitary_params_for_local_faces_ctx (c, x, lambda)

  fun local_test_GEO_simple_into_hash_limit
    (g: group, x: int, lambda: ratvec, maxFaces: int, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces_limit (g, x, lambda, maxFaces, uhash)

  fun local_test_GEO_simple_into_hash_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFaces: int, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces_limit_ctx (c, x, lambda, maxFaces, uhash)

  fun local_test_GEO_simple_into_hash (g: group, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces (g, x, lambda, uhash)

  fun local_test_GEO_simple_into_hash_ctx (c: ctx, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    add_unitary_from_local_faces_ctx (c, x, lambda, uhash)

  (* ---------------------------------------------------------------------- *)
  (* Additional `.at`-name compatibility: `*_hash_dumb*`                      *)
  (* ---------------------------------------------------------------------- *)

  (* The `.at` implementation’s `local_test_GEO_hash_dumb` is a “fallback”
     variant that avoids the more elaborate face-graph / ToHT pruning logic.
     In the SML port, the closest analogue is the baseline enumeration
     provided by `local_test_GEO_simple*`. *)

  fun local_test_GEO_hash_dumb_limit_ctx (c: ctx, x: int, lambda: ratvec, maxFaces: int) : param list =
    local_test_GEO_simple_limit_ctx (c, x, lambda, maxFaces)

  fun local_test_GEO_hash_dumb_ctx (c: ctx, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash_dumb_limit_ctx (c, x, lambda, ~1)

  fun local_test_GEO_hash_dumb_limit (g: group, x: int, lambda: ratvec, maxFaces: int) : param list =
    local_test_GEO_hash_dumb_limit_ctx (create_ctx g, x, lambda, maxFaces)

  fun local_test_GEO_hash_dumb (g: group, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash_dumb_limit (g, x, lambda, ~1)

  fun local_test_GEO_hash_dumb_into_hash_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFaces: int, uhash: ParamHash.t) : int =
    local_test_GEO_simple_into_hash_limit_ctx (c, x, lambda, maxFaces, uhash)

  fun local_test_GEO_hash_dumb_into_hash_ctx (c: ctx, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    local_test_GEO_hash_dumb_into_hash_limit_ctx (c, x, lambda, ~1, uhash)

  fun local_test_GEO_hash_dumb_into_hash_limit
    (g: group, x: int, lambda: ratvec, maxFaces: int, uhash: ParamHash.t) : int =
    local_test_GEO_hash_dumb_into_hash_limit_ctx (create_ctx g, x, lambda, maxFaces, uhash)

  fun local_test_GEO_hash_dumb_into_hash (g: group, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    local_test_GEO_hash_dumb_into_hash_limit (g, x, lambda, ~1, uhash)

  (* `.at`-style helper used when a ToHT-driven schedule is unavailable or
     deemed unhelpful: try the hash2 path if enabled, otherwise fall back to
     the dumb baseline. *)
  fun local_test_GEO_hash_else_dumb_limit_ctx (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    if !prefer_to_hts then
      local_test_GEO_hash2_limit_ctx (c, x, lambda, maxFacesPerDim)
    else
      local_test_GEO_hash_dumb_limit_ctx (c, x, lambda, maxFacesPerDim)

  fun local_test_GEO_hash_else_dumb_ctx (c: ctx, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash_else_dumb_limit_ctx (c, x, lambda, ~1)

  fun local_test_GEO_hash_else_dumb_limit (g: group, x: int, lambda: ratvec, maxFacesPerDim: int) : param list =
    local_test_GEO_hash_else_dumb_limit_ctx (create_ctx g, x, lambda, maxFacesPerDim)

  fun local_test_GEO_hash_else_dumb (g: group, x: int, lambda: ratvec) : param list =
    local_test_GEO_hash_else_dumb_limit (g, x, lambda, ~1)

  fun local_test_GEO_hash_else_dumb_into_hash_limit_ctx
    (c: ctx, x: int, lambda: ratvec, maxFacesPerDim: int, uhash: ParamHash.t) : int =
    if !prefer_to_hts then
      local_test_GEO_hash2_into_hash_limit_ctx (c, x, lambda, maxFacesPerDim, uhash)
    else
      local_test_GEO_hash_dumb_into_hash_limit_ctx (c, x, lambda, maxFacesPerDim, uhash)

  fun local_test_GEO_hash_else_dumb_into_hash_ctx (c: ctx, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    local_test_GEO_hash_else_dumb_into_hash_limit_ctx (c, x, lambda, ~1, uhash)

  fun local_test_GEO_hash_else_dumb_into_hash_limit
    (g: group, x: int, lambda: ratvec, maxFacesPerDim: int, uhash: ParamHash.t) : int =
    local_test_GEO_hash_else_dumb_into_hash_limit_ctx (create_ctx g, x, lambda, maxFacesPerDim, uhash)

  fun local_test_GEO_hash_else_dumb_into_hash (g: group, x: int, lambda: ratvec, uhash: ParamHash.t) : int =
    local_test_GEO_hash_else_dumb_into_hash_limit (g, x, lambda, ~1, uhash)
end
