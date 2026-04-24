use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/ParamFinals.sml";

(*
  File: atlas-scripts-sml/F4FPPParamConstruct.sml

  Purpose
  - Shared helpers for the F4 FPP verification programs:
      - build `nu` from `(x,lambda,gamma)` via the standard formula
          nu = gamma - (I + theta(x)) * lambda / 2
      - construct Atlas parameters from `(x,lambda,nu)` and normalize them
      - obtain the “first final term” (`first_param(finalize(p))` in `.at`)

  Why this exists
  - `VerifyF4FPP` (fast) and `SimplerVerifyF4FPP` (slow) should agree on the
    parameter construction and normalization semantics; separating the shared
    pieces helps make that explicit, and is a prerequisite for the staged
    formalization outlined in `VERIFY_ESTIMATE.md`.

  Ownership
  - Functions returning `AtlasFFI.param` return *owned* handles; callers must
    free them with `AtlasFFI.atlas_param_free`.
  - Functions that consume a handle will state so explicitly.
*)

structure F4FPPParamConstruct = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec
  type mat = Lattice.mat

  fun theta_of_x (g: group, x: int) : mat =
    AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))

  fun thetaPlusHalf_of_theta_lambda (rank: int, theta: mat, lambda: ratvec) : ratvec =
    let
      val onePlus = Lattice.matAdd (Lattice.identity rank, theta)
      val thetaPlus = Lattice.matVecMulRatvec onePlus lambda
    in
      Lattice.ratvecScale (thetaPlus, 1, 2)
    end

  fun nu_of_thetaPlusHalf_gamma (thetaPlusHalf: ratvec, gamma: ratvec) : ratvec =
    Lattice.ratvecSub (gamma, thetaPlusHalf)

  fun nu_of_x_lambda_gamma (g: group, x: int, lambda: ratvec, gamma: ratvec) : ratvec =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta = theta_of_x (g, x)
      val thetaPlusHalf = thetaPlusHalf_of_theta_lambda (rank, theta, lambda)
    in
      nu_of_thetaPlusHalf_gamma (thetaPlusHalf, gamma)
    end

  (* Construct an unnormalized parameter from `(x,lambda,gamma)` using the
     standard `nu` formula. *)
  fun param_of_x_lambda_gamma (g: group, x: int, lambda: ratvec, gamma: ratvec) : param =
    Representations.parameter (g, x, lambda, nu_of_x_lambda_gamma (g, x, lambda, gamma))

  (* Normalize a parameter, consuming the input handle. *)
  fun normalise_consume (p0: param) : param =
    let
      val p1 = AtlasFFI.atlas_param_normalise p0
      val () = AtlasFFI.atlas_param_free p0
    in
      if p1 = Foreign.Memory.null then
        raise Fail ("F4FPPParamConstruct.normalise_consume: normalise failed: " ^ AtlasFFI.atlas_last_error ())
      else
        p1
    end

  (* Normalize `p0` and pick the first final term, consuming `p0`.

     Returns `NONE` if finalization yields no final terms with positive
     multiplicity. *)
  fun first_final_term_consume (p0: param) : param option =
    let
      val p1 = normalise_consume p0
    in
      if AtlasFFI.atlas_param_is_final p1 = 1 then
        SOME p1
      else
        let
          val finals = ParamFinals.finals p1
          val () = AtlasFFI.atlas_param_free p1
          fun pick [] = NONE
            | pick ((q, mult) :: rest) =
                if mult = 0 then (AtlasFFI.atlas_param_free q; pick rest)
                else
                  (List.app (fn (r, _) => AtlasFFI.atlas_param_free r) rest; SOME q)
        in
          pick finals
        end
    end
end

