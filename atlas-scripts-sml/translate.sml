use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/parameters.sml";
use "atlas-scripts-sml/ParamPol.sml";

(*
  File: atlas-scripts-sml/translate.sml

  Purpose
  - Standard ML port (incremental) of `atlas-scripts/translate.at`.
  - Implements the core “translation of parameters” utilities needed by
    downstream scripts:
      - `translate_param_by` on parameters (and on `ParamPol`)
      - `T_param` translation to a target infinitesimal character

  Atlas correspondence
  - The `.at` primitive `param(x,lambda_rho,gamma)` constructs a (possibly
    non-final / non-standard) parameter by directly specifying:
      - `x : KGBElt`
      - `lambda_rho : X^*` (integral)
      - `gamma : ratvec` (infinitesimal character)
    This is *not* the same as `parameter(x,lambda,nu)`.

    In the C++ library, this corresponds to `Rep_context::sr_gamma`.
    The SML port calls this via:
      `AtlasFFI.atlas_param_new_from_lambda_rho_gamma_text`.

  Ownership
  - Functions returning `AtlasFFI.param` allocate a fresh parameter handle;
    callers must free it with `AtlasFFI.atlas_param_free`.
  - Functions returning `ParamPol.t` allocate a fresh polynomial that owns its
    parameters; callers must free it with `ParamPol.free`.

  Notes / limitations
  - This is an incremental port: it does not yet cover the “standard module”
    translation operators (`T_std`, `T_irr`, `Psi_irr`, ...). Add those as soon
    as another script requires them.
*)

structure Translate = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec

  fun fail where' msg = raise Fail ("Translate." ^ where' ^ ": " ^ msg)

  fun ratvecToText (u: ratvec) : string =
    let
      val u = Lattice.ratvecNormalize u
    in
      Int.toString (#den u) ^ " " ^ String.concatWith " " (List.map Int.toString (#nums u))
    end

  fun requireNonNullParam where' (p: param) =
    if p = Foreign.Memory.null then
      fail where' ("null parameter: " ^ AtlasFFI.atlas_last_error ())
    else
      ()

  fun ratvecAddIntShift (u: ratvec, shift: int list) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
      val den = #den u
      val nums = #nums u
      val () =
        if length nums = length shift then ()
        else fail "ratvecAddIntShift" "length mismatch"
      val nums2 = ListPair.mapEq (fn (a, s) => a + s * den) (nums, shift)
    in
      Lattice.ratvecNormalize {den = den, nums = nums2}
    end

  fun vecAdd (xs: int list, ys: int list) : int list =
    (if length xs = length ys then ListPair.mapEq (op +) (xs, ys)
     else fail "vecAdd" "length mismatch")

  (* Decompose a parameter `p` into the `.at` tuple `(x,lambda_rho,gamma)` where
     `lambda_rho` is integral and `gamma` is the infinitesimal character. *)
  fun unpack (p: param) : group * int * int list * ratvec =
    let
      val () = requireNonNullParam "unpack" p
      val g = AtlasFFI.atlas_param_group_handle p
      val () =
        if g = Foreign.Memory.null then
          fail "unpack" ("param group handle failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val x = AtlasFFI.atlas_param_x p
      val lambda = Parameters.parseRatvecText (AtlasFFI.atlas_param_lambda_text p)
      val gamma = Parameters.parseRatvecText (AtlasFFI.atlas_param_gamma_text p)
      val rho = Parameters.parseRatvecText (AtlasFFI.atlas_group_rho_text g)
      val lambdaRhoRat = Lattice.ratvecSub (lambda, rho)
      val lambdaRho =
        (case Lattice.ratvecToIntegral lambdaRhoRat of
           SOME v => v
         | NONE => fail "unpack" "lambda-rho not integral")
    in
      (g, x, lambdaRho, gamma)
    end

  (* Atlas: `translate_param_by (Param p, vec shift)`. *)
  fun translate_param_by (p: param, shift: int list) : param =
    let
      val (g, x, lambdaRho, gamma) = unpack p
      val () =
        if length lambdaRho = length shift then ()
        else fail "translate_param_by" "shift length mismatch"
      val lambdaRho2 = vecAdd (lambdaRho, shift)
      val gamma2 = ratvecAddIntShift (gamma, shift)
      val p2 =
        AtlasFFI.atlas_param_new_from_lambda_rho_gamma_text
          ( g
          , x
          , AllParameters.intsToCText lambdaRho2
          , 1
          , AllParameters.intsToCText (#nums gamma2)
          , #den gamma2
          )
      val () = requireNonNullParam "translate_param_by" p2
    in
      p2
    end

  fun translate_param_by_pol (poly: ParamPol.t, shift: int list) : ParamPol.t =
    let
      val out = ParamPol.create ()
      fun addOne (c, p) =
        let
          val p2 = translate_param_by (p, shift)
        in
          ParamPol.addTermMove (out, c, p2)
        end
    in
      List.app addOne (ParamPol.terms poly);
      out
    end

  (* Atlas: `T_param (Param p, ratvec gamma_target)`. *)
  fun T_param (p: param, gammaTarget: ratvec) : param =
    let
      val (_, _, _, gamma) = unpack p
      val diff = Lattice.ratvecNormalize (Lattice.ratvecSub (gammaTarget, gamma))
      val () =
        if #den diff = 1 then ()
        else fail "T_param" ("cannot translate to target (non-integral shift): " ^ ratvecToText gammaTarget)
      val shift = #nums diff
    in
      translate_param_by (p, shift)
    end

  fun gamma_of_parampol (poly: ParamPol.t) : ratvec option =
    (case ParamPol.terms poly of
       [] => NONE
     | (_, p0) :: rest =>
         let
           val g0 = Parameters.parseRatvecText (AtlasFFI.atlas_param_gamma_text p0)
           fun same (_, p) =
             let
               val g = Parameters.parseRatvecText (AtlasFFI.atlas_param_gamma_text p)
             in
               g = g0
             end
         in
           if List.all same rest then SOME g0 else fail "gamma_of_parampol" "infinitesimal character not well-defined"
         end)

  fun T_param_pol (poly: ParamPol.t, gammaTarget: ratvec) : ParamPol.t =
    (case gamma_of_parampol poly of
       NONE => ParamPol.create ()
     | SOME gamma =>
         let
           val diff = Lattice.ratvecNormalize (Lattice.ratvecSub (gammaTarget, gamma))
           val () =
             if #den diff = 1 then ()
             else fail "T_param_pol" "cannot translate ParamPol (non-integral shift)"
         in
           translate_param_by_pol (poly, #nums diff)
         end)
end
