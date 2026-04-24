(*
  File: atlas-scripts-sml/SimplerVerifyF4FPP.sml

  Purpose
  - Implementation module for the SML translation of
    `atlas-scripts/simpler_script_to_verify_F4_FPP_unitary_dual.at`.
  - The corresponding entrypoint script is:
      `atlas-scripts-sml/simpler_script_to_verify_F4_FPP_unitary_dual.sml`
    which is disabled by default to prevent accidental multi-hour runs.

  What the original `.at` script does (high-level)
  - Build the F4_s “known unitary” parameter hash (expected size 1864).
  - Then brute-force enumerate (x, lambda, gamma) where:
      - `x` ranges over all KGB elements,
      - `lambda` ranges over `FPP_lambdas(x)`,
      - `gamma` ranges over all folded-FPP barycenters (all face dimensions).
  - For each triple, construct a parameter and test:
      if `is_unitary(first_param(finalize(parameter(x,lambda,gamma))))`
      and that final term is missing from the known hash, print a warning.

  Notes on translation choices
  - In the `.at` script, the variable name `nu` is used for a barycenter
    (infinitesimal character). In the SML port we match the real construction
    used elsewhere in this repository (see `F4_FPP_points_compute`):
      given a barycenter `gamma`, compute
        nu = gamma - (I + theta(x)) * lambda / 2
      and then call `Representations.parameter(g,x,lambda,nu)`.
*)

use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPPFlags.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/FPP_lambdas_fold.sml";
use "atlas-scripts-sml/F4_FPP_barycenters.sml";
use "atlas-scripts-sml/F4_FPP_lambdas.sml";
use "atlas-scripts-sml/F4_FPP_points_compute.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/F4FPPParamConstruct.sml";

structure SimplerVerifyF4FPP = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec
  type triple = {x: int, lambda: ratvec, gamma: ratvec}

  (* Heuristic: detect the standard F4_s instance to use fixtures. *)
  fun looksLikeF4s (g: group) : bool =
    AtlasFFI.atlas_group_is_split g = 1
    andalso AtlasFFI.atlas_group_rank g = 4
    andalso AtlasFFI.atlas_group_kgb_size g = 229

  fun loadBarycenters (g: group) : ratvec list =
    if looksLikeF4s g then
      (F4_FPP_barycenters.loadRatvecs () handle _ => FPP_barycenters_fold.barycenters_all g)
    else
      FPP_barycenters_fold.barycenters_all g

  fun loadLambdasByX (g: group) : ratvec list array =
    let
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
    in
      if looksLikeF4s g then
        (F4_FPP_lambdas.loadRatvecs kgbSize handle _ => FPP_lambdas_fold.FPP_lambdas_table g)
      else
        FPP_lambdas_fold.FPP_lambdas_table g
    end

  val param_of_x_lambda_gamma = F4FPPParamConstruct.param_of_x_lambda_gamma
  val first_final_term = F4FPPParamConstruct.first_final_term_consume

  (* Iterate the slow domain D_slow without materializing it as a list. *)
  fun for_domain (g: group, f: triple -> unit) : unit =
    let
      val gammas = loadBarycenters g
      val lambdasByX = loadLambdasByX g
      val kgbSize = Array.length lambdasByX
      fun loopX x =
        if x = kgbSize then
          ()
        else
          let
            val lambdas = Array.sub (lambdasByX, x)
            fun loopL [] = ()
              | loopL (lambda :: rest) =
                  (List.app (fn gamma => f {x = x, lambda = lambda, gamma = gamma}) gammas; loopL rest)
          in
            loopL lambdas;
            loopX (x + 1)
          end
    in
      loopX 0
    end

  (* Check one triple from the slow domain. Returns `true` iff it witnesses a
     “missing” unitary final term (i.e. a counterexample to completeness). *)
  fun triple_is_missing (g: group, uhash: ParamHash.t, t: triple) : bool =
    let
      val {x, lambda, gamma} = t
      val p0 = param_of_x_lambda_gamma (g, x, lambda, gamma)
    in
      case first_final_term p0 of
        NONE => false
      | SOME pi =>
          let
            val isU = AtlasFFI.atlas_param_is_unitary pi = 1
            val missing = ParamHash.lookup uhash pi < 0
            val () = AtlasFFI.atlas_param_free pi
          in
            isU andalso missing
          end
    end

  (* Iterate the domain and count counterexamples. Optionally print a warning
     line for each counterexample (matching the original `.at` script). *)
  fun check_domain (g: group, uhash: ParamHash.t, verbose: bool) : int =
    let
      val misses = ref 0
      fun step t =
        if triple_is_missing (g, uhash, t) then
          (misses := !misses + 1;
           if verbose then TextIO.print "IT'S ALL WRONG!!!\n" else ())
        else
          ()
      val () = for_domain (g, step)
    in
      !misses
    end

  fun build_known_unitary_hash (g: group) : ParamHash.t =
    let
      val uhash = ParamHash.create 8192
      val () = FPPFlags.Dirac_flag := true
      val () = F4_FPP_points_compute.computeAllIntoParamHash (g, uhash)
      val () =
        if ParamHash.size uhash = 1864 then
          ()
        else
          raise Fail ("expected 1864 params, got " ^ Int.toString (ParamHash.size uhash))
    in
      uhash
    end

  fun runSlow () : unit =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
      val uhash = build_known_unitary_hash g
    in
      ignore (check_domain (g, uhash, true));
      ParamHash.freeAll uhash;
      AtlasFFI.atlas_group_free g
    end

  (* Fast partial check: only sample a bounded number of x/lambda/gamma values. *)
  fun runFast (maxX: int, maxLambdasPerX: int, maxGammas: int) : unit =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
      val uhash = build_known_unitary_hash g

      val gammasAll = loadBarycenters g
      val gammas = List.take (gammasAll, Int.max (0, Int.min (maxGammas, length gammasAll)))
      val lambdasByX = loadLambdasByX g
      val kgbSize = Array.length lambdasByX
      val xLimit = Int.max (0, Int.min (maxX, kgbSize))

      fun loopX x =
        if x = xLimit then
          ()
        else
          let
            val lambdas0 = Array.sub (lambdasByX, x)
            val lambdas = List.take (lambdas0, Int.max (0, Int.min (maxLambdasPerX, length lambdas0)))
            fun loopL [] = ()
              | loopL (lambda :: rest) =
                  ( List.app
                      (fn gamma =>
                         if triple_is_missing (g, uhash, {x = x, lambda = lambda, gamma = gamma}) then
                           TextIO.print "IT'S ALL WRONG!!!\n"
                         else
                           ())
                      gammas;
                    loopL rest)
          in
            loopL lambdas;
            loopX (x + 1)
          end
    in
      loopX 0;
      ParamHash.freeAll uhash;
      AtlasFFI.atlas_group_free g
    end
end
