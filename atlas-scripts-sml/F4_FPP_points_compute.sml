use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/FPP_lambdas_fold.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/FPPFlags.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/FPPFaceKey.sml";
use "atlas-scripts-sml/VertexData.sml";
use "atlas-scripts-sml/unity.sml";
use "atlas-scripts-sml/F4_FPP_barycenters.sml";
use "atlas-scripts-sml/F4_FPP_lambdas.sml";

(*
  File: atlas-scripts-sml/F4_FPP_points_compute.sml

  Purpose
  - Compute and filter the Atlas parameter set used in the F4 FPP/unitarity
    verification workflow (`script_to_verify_F4_FPP_unitary_dual.sml`).

  What this module produces
  - A `ParamHash.t` containing (deduplicated-by-equivalence) final parameters
    built from the folded-FPP barycenters (gamma values) and the precomputed
    lambda table, across all KGB elements of `F4_s`.

  High-level algorithm
  - For each KGB element `x`:
      - read `theta(x)` and build `(I+theta)`
      - for each candidate `lambda` attached to `x`:
          - select those `gamma` barycenters compatible with the affine
            involution constraint (implemented by key matching)
          - build `nu` from `gamma` and `(I+theta)*lambda/2`
          - construct and normalize the parameter
          - keep it iff it is standard, final, hermitian, and (optionally)
            unitary (controlled by `FPPFlags.Dirac_flag`)
  - Insert into `ParamHash` (which owns canonical stored handles).

  Memory/ownership
  - Parameters built during the scan are freed immediately after hashing/matching.
  - The caller owns the returned `ParamHash.t` from `computeAll`/`computeThetaStableFaces`
    and must free it via `ParamHash.freeAll`.
*)
structure F4_FPP_points_compute = struct
  type ratvec = Lattice.ratvec

  (* Convert SML `~` negatives to C-style `-` negatives as expected by Atlas parsers. *)
  fun intToCText (n: int) : string =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  (* Serialize an integer list in the Atlas C++ whitespace-separated format. *)
  fun intsToCText (xs: int list) : string =
    String.concatWith " " (List.map intToCText xs)

  (* Normalize a rational vector and turn it into a stable key `[den, nums...]`. *)
  fun ratvecKey (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  (* Normalize a rational vector and serialize it as `(numsText, den)` for FFI calls. *)
  fun ratvecToTextParts (u: ratvec) : string * int =
    let
      val u = Lattice.ratvecNormalize u
    in
      (intsToCText (#nums u), #den u)
    end

  (* Add two rational vectors. *)
  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec =
    Lattice.ratvecSub (u, Lattice.ratvecScale (v, ~1, 1))

  (* Populate `out` with all parameters produced by the folded-FPP barycenter and
     lambda tables, filtered by standard/final/hermitian and (optionally) unitary. *)
  fun computeAllIntoParamHash (g: AtlasFFI.group, out: ParamHash.t) : unit =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val kgbSize = AtlasFFI.atlas_group_kgb_size g

      fun parseLeadingInt (s: string) : int option =
        case String.tokens Char.isSpace s of
          [] => NONE
        | tok :: _ => Int.fromString tok

      val isSplit = AtlasFFI.atlas_group_is_split g = 1
      val nPosRoots =
        (case parseLeadingInt (AtlasFFI.atlas_group_posroots_text g) of
           SOME n => n
         | NONE => ~1)

      fun looksLikeF4s () : bool =
        isSplit andalso rank = 4 andalso kgbSize = 229 andalso nPosRoots = 24

      val barycenters =
        if looksLikeF4s () then
          (F4_FPP_barycenters.loadRatvecs () handle _ => FPP_barycenters_fold.barycenters_all g)
        else
          FPP_barycenters_fold.barycenters_all g

      val lambdasByX =
        if looksLikeF4s () then
          (F4_FPP_lambdas.loadRatvecs kgbSize handle _ => FPP_lambdas_fold.FPP_lambdas_table g)
        else
          FPP_lambdas_fold.FPP_lambdas_table g

      (* Equal-rank predicate: true iff some KGB element has involution `-I`. *)
      val equalRank =
        let
          fun loop i =
            if i >= kgbSize then
              false
            else if AtlasFFI.atlas_group_kgb_involution_is_minus_identity (g, i) = 1 then
              true
            else
              loop (i + 1)
        in
          loop 0
        end

      fun parseTheta x =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))

      fun mkParamAndMaybeAdd (x: int, lambda: ratvec, thetaPlusHalf: ratvec, gamma: ratvec) : unit =
        let
          val nu = Lattice.ratvecSub (gamma, thetaPlusHalf)
          val (lambdaNumsText, lambdaDen) = ratvecToTextParts lambda
          val (nuNumsText, nuDen) = ratvecToTextParts nu

          val p0 = AtlasFFI.atlas_param_new_from_lambda_nu_text (g, x, lambdaNumsText, lambdaDen, nuNumsText, nuDen)
        in
          if p0 = Foreign.Memory.null then
            raise Fail ("F4_FPP_points_compute: param construction failed: " ^ AtlasFFI.atlas_last_error ())
          else
            let
              val p1 = AtlasFFI.atlas_param_normalise p0
              val () = AtlasFFI.atlas_param_free p0
            in
              if p1 = Foreign.Memory.null then
                raise Fail ("F4_FPP_points_compute: normalise failed: " ^ AtlasFFI.atlas_last_error ())
              else
                let
                  val okBase =
                    AtlasFFI.atlas_param_is_standard p1 = 1 andalso AtlasFFI.atlas_param_is_final p1 = 1
                    andalso AtlasFFI.atlas_param_is_hermitian p1 = 1

                  (* If we have already stored an equivalent parameter, it is
                     already known to satisfy the same filters, so skip any
                     further (potentially expensive) checks. *)
                  val already = okBase andalso ParamHash.lookup out p1 >= 0

                  val okUnitary =
                    if not okBase orelse already orelse not (!FPPFlags.Dirac_flag) then
                      true
                    else if !FPPFlags.to_ht_prune_flag andalso equalRank then
                      Unity.is_unitary_test_prune_equal_rank_steps
                        (g, p1, !FPPFlags.to_ht_prune_steps, !FPPFlags.to_ht_prune_step_size)
                    else if not equalRank then
                      AtlasFFI.atlas_param_is_unitary p1 = 1
                    else
                      AtlasFFI.atlas_param_is_unitary p1 = 1

                  val () =
                    if okBase andalso okUnitary andalso not already then
                      ignore (ParamHash.match out p1)
                    else
                      ()

                  val () = AtlasFFI.atlas_param_free p1
                in
                  ()
                end
            end
        end

      fun loopX x =
        let
          val theta = parseTheta x
          val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
          val baryKeys = List.map (fn gam => ratvecKey (Lattice.matVecMulRatvec onePlus gam)) barycenters

          fun loopLambda (lambda: ratvec) =
            let
              val thetaPlus = Lattice.matVecMulRatvec onePlus lambda
              val thetaPlusHalf = Lattice.ratvecScale (thetaPlus, 1, 2)
              val k = ratvecKey thetaPlus

              fun scan ([], []) = ()
                | scan (gam :: gs, k2 :: ks) =
                    (if k2 = k then mkParamAndMaybeAdd (x, lambda, thetaPlusHalf, gam) else ();
                     scan (gs, ks))
                | scan _ = raise Fail "F4_FPP_points_compute: internal error (bary/key mismatch)"
            in
              scan (barycenters, baryKeys)
            end

          val lambdas = Array.sub (lambdasByX, x)
          val () =
            if !FPPFlags.final_verbose andalso x mod 25 = 0 then
              TextIO.print
                ("F4_FPP_points_compute: x=" ^ Int.toString x ^ "/" ^ Int.toString (kgbSize - 1) ^ " lambdas="
                 ^ Int.toString (length lambdas) ^ " outSize=" ^ Int.toString (ParamHash.size out) ^ "\n")
            else
              ()
        in
          List.app loopLambda lambdas
        end
    in
      List.app loopX (List.tabulate (kgbSize, fn i => i))
    end

  (* Variant inspired by `.at`'s theta-stable face logic:
     restrict gammas to barycenters of faces whose vertex set is stable under the affine involution
       v |-> -theta*v + (I+theta)*lambda
     (stability is checked on the FPP vertex-index set via `VertexData.lookup`).
     Intended as a stepping stone toward the exact 1864 "unitary facet" set for F4_s. *)
  (* Experimental variant: restrict to barycenters of affine-theta-stable faces.
     Not currently used by the main verifier, but kept as a reference path. *)
  fun computeThetaStableFacesIntoParamHash (g: AtlasFFI.group, out: ParamHash.t) : unit =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val kgbSize = AtlasFFI.atlas_group_kgb_size g

      val faceCtx = FPPFaceKey.create g
      val verts = #verts faceCtx
      val vd = #vd faceCtx
      val nVerts = Array.length verts

      val allGammas = FPP_barycenters_fold.barycenters_all g

      val faces : (int list * ratvec) array =
        Array.fromList
          (List.map
             (fn gamma =>
               case FPPFaceKey.faceKeyOfGamma (faceCtx, gamma) of
                 NONE => raise Fail "computeThetaStableFaces: missing faceKey for barycenter"
               | SOME fk => (fk, gamma))
             allGammas)

      val nFaces = Array.length faces

      val lambdasByX = FPP_lambdas_fold.FPP_lambdas_table g

      fun parseTheta x =
        AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))

      fun mkParamAndMaybeAdd (x: int, lambda: ratvec, thetaPlusHalf: ratvec, gamma: ratvec) : unit =
        let
          val nu = Lattice.ratvecSub (gamma, thetaPlusHalf)
          val (lambdaNumsText, lambdaDen) = ratvecToTextParts lambda
          val (nuNumsText, nuDen) = ratvecToTextParts nu

          val p0 = AtlasFFI.atlas_param_new_from_lambda_nu_text (g, x, lambdaNumsText, lambdaDen, nuNumsText, nuDen)
        in
          if p0 = Foreign.Memory.null then
            raise Fail
              ("F4_FPP_points_compute.computeThetaStableFaces: param construction failed: " ^ AtlasFFI.atlas_last_error ())
          else
            let
              val p1 = AtlasFFI.atlas_param_normalise p0
              val () = AtlasFFI.atlas_param_free p0
            in
              if p1 = Foreign.Memory.null then
                raise Fail
                  ("F4_FPP_points_compute.computeThetaStableFaces: normalise failed: " ^ AtlasFFI.atlas_last_error ())
              else
                let
                  val ok =
                    AtlasFFI.atlas_param_is_standard p1 = 1 andalso AtlasFFI.atlas_param_is_final p1 = 1
                    andalso AtlasFFI.atlas_param_is_hermitian p1 = 1
                    andalso (not (!FPPFlags.Dirac_flag) orelse AtlasFFI.atlas_param_is_unitary p1 = 1)

                  val () = if ok then ignore (ParamHash.match out p1) else ()
                  val () = AtlasFFI.atlas_param_free p1
                in
                  ()
                end
            end
        end

      fun faceThetaStable (img: int option array, fk: int list) : bool =
        let
          fun inFace j = List.exists (fn t => t = j) fk
          fun ok i =
            (case Array.sub (img, i) of
               NONE => false
             | SOME j =>
                 inFace j andalso
                 (case Array.sub (img, j) of
                    SOME i2 => i2 = i
                  | NONE => false))
        in
          List.all ok fk
        end

      fun loopX x =
        let
          val theta = parseTheta x
          val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
          val lambdas = Array.sub (lambdasByX, x)

          fun loopLambda (lambda: ratvec) =
            let
              val thetaPlus = Lattice.matVecMulRatvec onePlus lambda
              val thetaPlusHalf = Lattice.ratvecScale (thetaPlus, 1, 2)

              val img = Array.array (nVerts, NONE : int option)

              fun computeImg i =
                let
                  val v = Array.sub (verts, i)
                  val thetaV = Lattice.matVecMulRatvec theta v
                  val w = ratvecAdd (Lattice.ratvecScale (thetaV, ~1, 1), thetaPlus)
                in
                  Array.update (img, i, VertexData.lookup (vd, w))
                end

              val () = List.app computeImg (List.tabulate (nVerts, fn i => i))

              fun scanFace idx =
                if idx = nFaces then
                  ()
                else
                  let
                    val (fk, gamma) = Array.sub (faces, idx)
                  in
                    if faceThetaStable (img, fk) then
                      mkParamAndMaybeAdd (x, lambda, thetaPlusHalf, gamma)
                    else
                      ();
                    scanFace (idx + 1)
                  end

              val () = scanFace 0
            in
              ()
            end

          val () =
            if !FPPFlags.final_verbose andalso x mod 25 = 0 then
              TextIO.print
                ("F4_FPP_points_compute(thetaStableFaces): x="
                 ^ Int.toString x
                 ^ "/"
                 ^ Int.toString (kgbSize - 1)
                 ^ " lambdas="
                 ^ Int.toString (length lambdas)
                 ^ " outSize="
                 ^ Int.toString (ParamHash.size out)
                 ^ "\n")
            else
              ()
        in
          List.app loopLambda lambdas
        end
    in
      List.app loopX (List.tabulate (kgbSize, fn i => i))
    end

  (* Allocate a fresh hash and fill it using `computeThetaStableFacesIntoParamHash`. *)
  fun computeThetaStableFaces (g: AtlasFFI.group) : ParamHash.t =
    let
      val out = ParamHash.create 65536
      val () = computeThetaStableFacesIntoParamHash (g, out)
    in
      out
    end

  (* Allocate a fresh hash and fill it using `computeAllIntoParamHash`. *)
  fun computeAll (g: AtlasFFI.group) : ParamHash.t =
    let
      val out = ParamHash.create 65536
      val () = computeAllIntoParamHash (g, out)
    in
      out
    end

  (* Convenience aliases used by higher-level scripts. *)
  val computeIntoParamHash = computeAllIntoParamHash
  val compute = computeAll
end
