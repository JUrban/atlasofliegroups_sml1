use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/FPP_vertices_fold.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/FPP_lambdas_fold.sml

  Purpose
  - Partial SML port of `atlas-scripts/FPP_lambdas.at` tailored to split groups
    without “flippable-edge” contributions (notably `F4_s`).
  - Produces the lambda tables used to construct the F4 FPP parameter set.

  High-level algorithm
  - For each KGB element `x` with involution `theta(x)`:
      1) enumerate integral candidates for `(1+theta)*lambda` coming from FPP vertices
      2) solve the defining lattice equations to obtain `lambda0` candidates
      3) add all 2-torsion twists (`LambdaDifferential0.allTheta theta`)
      4) filter by asking Atlas whether `(x,lambda,nu=0)` yields a nonzero final
         (via `ParamFinals.finals`)

  Output
  - `FPP_lambdas_table` returns an array indexed by `x`, whose entries are the
    deduplicated list of admissible `lambda` values for that `x`.
*)
structure FPP_lambdas_fold = struct
  type mat = Lattice.mat
  type vec = int list
  type ratvec = Lattice.ratvec

  (* Stable key `[den, nums...]` after normalization. *)
  fun ratvecKey (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  (* Sort and unique rational vectors under the key ordering. *)
  fun no_reps_ratvec (us: ratvec list) : ratvec list =
    Basic.sort_u_by (ratvecKey, Sort.rlex_leq) us

  (* Add integer vectors componentwise. *)
  fun vecAdd (a: vec, b: vec) : vec = ListPair.mapEq (op +) (a, b)

  (* Negate an integer vector. *)
  fun vecNeg v : vec = List.map (fn x => ~x) v

  (* Parse a rational weight in Atlas text form. *)
  fun parseRatWeight (s: string) : ratvec = AllParameters.parseRatWeightText s

  (* Add an integral vector to a rational vector (same dimension). *)
  fun ratvecAddIntVec (u: ratvec, v: vec) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
      val den = #den u
      val nums = #nums u
      val () = if length nums = length v then () else raise Fail "ratvecAddIntVec: length mismatch"
      val nums' = ListPair.mapEq (fn (a, b) => a + den * b) (nums, v)
    in
      Lattice.ratvecNormalize {den = den, nums = nums'}
    end

  (* Add each vector in `vs` to `u`. *)
  fun ratvecAddIntVecs (u: ratvec, vs: vec list) : ratvec list =
    List.map (fn v => ratvecAddIntVec (u, v)) vs

  (* Solve `a*x=b`, raising if no solution exists. *)
  fun requiredVecSolve (a: mat, b: vec) : vec =
    case Lattice.solve (a, b) of
      NONE => raise Fail "requiredVecSolve: no solution"
    | SOME x => x

  (* Compute `I+theta`. *)
  fun th1_of_theta (theta: mat) : mat =
    let
      val (n, m) = Lattice.matShape theta
      val () = if n = m then () else raise Fail "th1_of_theta: non-square"
    in
      Lattice.matAdd (Lattice.identity n, theta)
    end

  (* Text for a length-`n` zero vector. *)
  fun zerosText n : string =
    String.concatWith " " (List.tabulate (n, fn _ => "0"))

  (* Serialize an int list in the Atlas C++ parser format (C-style negatives). *)
  fun intsToCText xs =
    let
      fun intToCText n =
        let
          val s = Int.toString n
        in
          if String.size s > 0 andalso String.sub (s, 0) = #"~" then
            "-" ^ String.extract (s, 1, NONE)
          else
            s
        end
    in
      String.concatWith " " (List.map intToCText xs)
    end

  (* Cache key for a theta matrix. *)
  fun thetaKey (theta: mat) : string = Lattice.matToText theta

  (* Compute the candidate lam+theta*lam values (integral vectors) from vertices only. *)
  (* Enumerate integral candidates for `(1+theta)*lambda` arising from FPP vertices. *)
  fun lamthlams_from_vertices (verts: ratvec list, rho: ratvec, theta: mat) : vec list =
    let
      val th1 = th1_of_theta theta
      val shiftRat = Lattice.matVecMulRatvec th1 rho
      val shiftRat = Lattice.ratvecNormalize shiftRat
      val shift =
        if #den shiftRat = 1 then vecNeg (#nums shiftRat)
        else raise Fail "lamthlams_from_vertices: expected (1+theta)*rho integral"

      fun okVertex v =
        let
          val w = Lattice.ratvecNormalize (Lattice.matVecMulRatvec th1 v)
        in
          if #den w <> 1 then NONE
          else
            let
              val rhs = vecAdd (#nums w, shift)
            in
              case Lattice.solve (th1, rhs) of
                NONE => NONE
              | SOME _ => SOME (#nums w)
            end
        end
    in
      Basic.sort_u (Sort.rlex_leq) (List.mapPartial okVertex verts)
    end

  (* Recover `lambda0` values from the computed `(1+theta)*lambda` candidates. *)
  fun lambda0s_from_lamthlams (rho: ratvec, theta: mat, lamthlams: vec list) : ratvec list =
    let
      val th1 = th1_of_theta theta
      val shiftRat = Lattice.ratvecNormalize (Lattice.matVecMulRatvec th1 rho)
      val shift =
        if #den shiftRat = 1 then vecNeg (#nums shiftRat)
        else raise Fail "lambda0s_from_lamthlams: expected (1+theta)*rho integral"

      fun one lamthlam =
        let
          val rhs = vecAdd (lamthlam, shift)
          val lr = requiredVecSolve (th1, rhs)
        in
          ratvecAddIntVec (rho, lr)
        end
    in
      List.map one lamthlams
    end

  (* Compute FPP_lambdas(x) as a list of ratvecs (lambda values), for a fixed KGB index x. *)
  (* Compute `FPP_lambdas(x)` without caching across different `x`. *)
  fun FPP_lambdas_x (g: AtlasFFI.group, x: int) : ratvec list =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta = AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val rho = parseRatWeight (AtlasFFI.atlas_group_rho_text g)
      val verts = FPP_vertices_fold.vertices g (* uncached *)

      val lamthlams = lamthlams_from_vertices (verts, rho, theta)
      val lambda0s = lambda0s_from_lamthlams (rho, theta, lamthlams)
      val twists = LambdaDifferential0.allTheta theta
      val lambdas = List.concat (List.map (fn l0 => ratvecAddIntVecs (l0, twists)) lambda0s)

      fun tryParam (lam: ratvec) : ratvec option =
        let
          val lam = Lattice.ratvecNormalize lam
          val p =
            AtlasFFI.atlas_param_new_from_lambda_nu_text
              ( g
              , x
              , intsToCText (#nums lam)
              , #den lam
              , zerosText rank
              , 1
              )
        in
          if p = Foreign.Memory.null then
            NONE
          else
            let
              val lamText = AtlasFFI.atlas_param_lambda_text p
              val finals = ParamFinals.finals p
              val ok = List.exists (fn (_, mult) => mult <> 0) finals
              val () = ParamFinals.freeTerms finals
              val () = AtlasFFI.atlas_param_free p
            in
              if ok then SOME (parseRatWeight lamText) else NONE
            end
        end
    in
      no_reps_ratvec (List.mapPartial tryParam lambdas)
    end

  type theta_info =
    { theta: mat
    , lambda0s: ratvec list
    , twists: vec list
    }

  (* Compute the theta-dependent pieces used in the lambda construction. *)
  fun compute_theta_info (verts: ratvec list, rho: ratvec, theta: mat) : theta_info =
    let
      val lamthlams = lamthlams_from_vertices (verts, rho, theta)
      val lambda0s = lambda0s_from_lamthlams (rho, theta, lamthlams)
      val twists = LambdaDifferential0.allTheta theta
    in
      {theta = theta, lambda0s = lambda0s, twists = twists}
    end

  (* Lookup-or-compute theta info in a simple association-list cache. *)
  fun get_theta_info (cache: (string * theta_info) list ref, verts: ratvec list, rho: ratvec, theta: mat) : theta_info =
    let
      val k = thetaKey theta
      fun find [] = NONE
        | find ((k', v) :: rest) = if k' = k then SOME v else find rest
    in
      case find (!cache) of
        SOME v => v
      | NONE =>
          let
            val v = compute_theta_info (verts, rho, theta)
            val () = cache := (k, v) :: (!cache)
          in
            v
          end
    end

  (* Cached version of `FPP_lambdas_x` that reuses vertices and theta-derived info. *)
  fun FPP_lambdas_x_cached (g: AtlasFFI.group, x: int, verts: ratvec list, rho: ratvec, cache: (string * theta_info) list ref) : ratvec list =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta = AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val info = get_theta_info (cache, verts, rho, theta)
      val lambdas = List.concat (List.map (fn l0 => ratvecAddIntVecs (l0, #twists info)) (#lambda0s info))

      fun tryParam (lam: ratvec) : ratvec option =
        let
          val lam = Lattice.ratvecNormalize lam
          val p =
            AtlasFFI.atlas_param_new_from_lambda_nu_text
              ( g
              , x
              , intsToCText (#nums lam)
              , #den lam
              , zerosText rank
              , 1
              )
        in
          if p = Foreign.Memory.null then
            NONE
          else
            let
              val lamText = AtlasFFI.atlas_param_lambda_text p
              val finals = ParamFinals.finals p
              val ok = List.exists (fn (_, mult) => mult <> 0) finals
              val () = ParamFinals.freeTerms finals
              val () = AtlasFFI.atlas_param_free p
            in
              if ok then SOME (parseRatWeight lamText) else NONE
            end
        end
    in
      no_reps_ratvec (List.mapPartial tryParam lambdas)
    end

  (* Compute all `FPP_lambdas(x)` for a group, caching vertices and theta-derived data. *)
  (* Compute the full `x -> lambdas` table for `g`. *)
  fun FPP_lambdas_table (g: AtlasFFI.group) : ratvec list array =
    let
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
      val rho = parseRatWeight (AtlasFFI.atlas_group_rho_text g)
      val verts = FPP_vertices_fold.vertices g
      val cache = ref ([]: (string * theta_info) list)
    in
      Array.tabulate (kgbSize, fn x => FPP_lambdas_x_cached (g, x, verts, rho, cache))
    end
end
