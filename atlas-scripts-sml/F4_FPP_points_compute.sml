use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/FPP_lambdas_fold.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/FPPFlags.sml";
use "atlas-scripts-sml/Lattice.sml";

structure F4_FPP_points_compute = struct
  type ratvec = Lattice.ratvec

  fun intToCText (n: int) : string =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  fun intsToCText (xs: int list) : string =
    String.concatWith " " (List.map intToCText xs)

  fun ratvecKey (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  fun ratvecToTextParts (u: ratvec) : string * int =
    let
      val u = Lattice.ratvecNormalize u
    in
      (intsToCText (#nums u), #den u)
    end

  fun computeAllIntoParamHash (g: AtlasFFI.group, out: ParamHash.t) : unit =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val kgbSize = AtlasFFI.atlas_group_kgb_size g

      val barycenters = FPP_barycenters_fold.barycenters_all g
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
                  val ok =
                    AtlasFFI.atlas_param_is_standard p1 = 1 andalso AtlasFFI.atlas_param_is_final p1 = 1
                    andalso AtlasFFI.atlas_param_is_hermitian p1 = 1
                    andalso (not (!FPPFlags.Dirac_flag) orelse AtlasFFI.atlas_param_is_unitary_c_form p1 = 1)

                  val () =
                    if ok then
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

  fun computeAll (g: AtlasFFI.group) : ParamHash.t =
    let
      val out = ParamHash.create 65536
      val () = computeAllIntoParamHash (g, out)
    in
      out
    end

  (* Convenience aliases. *)
  val computeIntoParamHash = computeAllIntoParamHash
  val compute = computeAll
end
