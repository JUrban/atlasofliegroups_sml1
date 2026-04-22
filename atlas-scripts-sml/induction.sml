use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/Coordinates.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/parabolics.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";

(* Partial SML analogue of `atlas-scripts/induction.at`.
   This file starts with the “good range” dominance checks and Levi construction
   needed by unitary-dual scripts. *)
structure Induction = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec
  type parabolic = Parabolics.parabolic

  fun ratvecNeg (u: ratvec) : ratvec =
    Lattice.ratvecScale (u, ~1, 1)

  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec =
    Lattice.ratvecSub (u, ratvecNeg v)

  fun parseRatWeightText (s: string) : ratvec =
    AllParameters.parseRatWeightText s

  fun is_dominant (g: group) (v: ratvec) : bool =
    let
      val cors = Coordinates.parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)
      val coords = Coordinates.coordsRatFromCoroots cors v
      val zero = {num = 0, den = 1}
    in
      List.all (fn x => Coordinates.leq (zero, x)) coords
    end

  fun Levi (g: group) ((S, x): parabolic) : group =
    let
      val S_text = String.concatWith " " (List.map Int.toString S)
      val L = AtlasFFI.atlas_group_new_levi_of_parabolic (g, S_text, x)
      val () =
        if L = Foreign.Memory.null then
          raise Fail ("Induction.Levi: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      L
    end

  (* `is_weakly_good(p_L,G)` from induction.at (requires L = real_form(p_L) is Levi in G). *)
  fun is_weakly_good (pL: param, G: group, L: group) : bool =
    let
      val rhoG = parseRatWeightText (AtlasFFI.atlas_group_rho_text G)
      val rhoL = parseRatWeightText (AtlasFFI.atlas_group_rho_text L)
      val gammaL = parseRatWeightText (AtlasFFI.atlas_param_gamma_text pL)
      val rho_u = Lattice.ratvecSub (rhoG, rhoL)
      val v = ratvecAdd (gammaL, rho_u)
    in
      is_dominant G v
    end

  fun tryLevi (g: group) ((S, x): parabolic) : group option =
    let
      val S_text = String.concatWith " " (List.map Int.toString S)
      val L = AtlasFFI.atlas_group_new_levi_of_parabolic (g, S_text, x)
    in
      if L = Foreign.Memory.null then NONE else SOME L
    end

  fun subsetString (S: int list) : string =
    "[" ^ String.concatWith "," (List.map Int.toString S) ^ "]"

  fun lieTypeString (lt: LieType.t) : string =
    let
      fun one (c, r) = str c ^ Int.toString r
    in
      String.concatWith " x " (List.map one lt)
    end

  fun groupTypeString (g: group) : string =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          raise Fail ("Induction.groupTypeString: rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val lt = RootDatum.lieType rd
      val () = RootDatum.free rd
    in
      lieTypeString lt
    end

  fun findKGBByInvolutionAndTorusFactorText (g: group) (thetaText: string, tfText: string) : int option =
    let
      val n = AtlasFFI.atlas_group_kgb_size g
      fun loop x =
        if x >= n then NONE
        else if AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x) = thetaText
                andalso AtlasFFI.atlas_kgb_torus_factor_text (g, x) = tfText then
          SOME x
        else
          loop (x + 1)
    in
      loop 0
    end

  fun powerSet (xs: int list) : int list list =
    let
      fun step (x, acc) = acc @ List.map (fn s => x :: s) acc
    in
      List.foldl step [[]] xs
    end

  fun theta_stable_parabolics_with (G: group) (x: int) : parabolic list =
    let
      val r = AtlasFFI.atlas_group_semisimple_rank G
      val simples = List.tabulate (r, fn i => i)
      fun keep S =
        let
          val y = Parabolics.maximal G (S, x)
          val P = (S, y)
        in
          if Parabolics.is_closed G P then
            (case tryLevi G P of
               NONE => NONE
             | SOME L => (AtlasFFI.atlas_group_free L; SOME P))
          else
            NONE
        end
    in
      List.mapPartial keep (powerSet simples)
    end

  fun induced_matches (p: param, G: group, xG: int) : bool =
    let
      val lambda = parseRatWeightText (AtlasFFI.atlas_param_lambda_text p)
      val nu = parseRatWeightText (AtlasFFI.atlas_param_nu_text p)

      val p0 =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          (G, xG, AllParameters.intsToCText (#nums lambda), #den lambda, AllParameters.intsToCText (#nums nu), #den nu)
      val () =
        if p0 = Foreign.Memory.null then
          raise Fail ("Induction.induced_matches: parameter construction failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val p1 = AtlasFFI.atlas_param_normalise p0
      val () = AtlasFFI.atlas_param_free p0
      val () =
        if p1 = Foreign.Memory.null then
          raise Fail ("Induction.induced_matches: normalise failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()

      val finals = ParamFinals.finals p1
      val () = AtlasFFI.atlas_param_free p1
    in
      let
        val ok = List.exists (fn (q, mult) => mult = 1 andalso AtlasFFI.atlas_param_equal (q, p) = 1) finals
        val () = ParamFinals.freeTerms finals
      in
        ok
      end
    end

  (* Atlas-style `is_good_range_induced_from(p)` but only returns the first witness (if any),
     formatted like the old C++ probe: "1|same|unitary|type=... rf=... S=[...]". *)
  fun good_range_induced_from_first_text (p: param, G: group) : string =
    let
      val x = AtlasFFI.atlas_param_x p
      val thetaText = AtlasFFI.atlas_group_kgb_involution_matrix_text (G, x)
      val tfText = AtlasFFI.atlas_kgb_torus_factor_text (G, x)
      val rhoG = parseRatWeightText (AtlasFFI.atlas_group_rho_text G)
      val lambdaP = parseRatWeightText (AtlasFFI.atlas_param_lambda_text p)
      val nuP = parseRatWeightText (AtlasFFI.atlas_param_nu_text p)

      fun tryOne ((S, y): parabolic) : string option =
        let
          val P = (S, y)
        in
          case tryLevi G P of
            NONE => NONE
          | SOME L =>
              (case findKGBByInvolutionAndTorusFactorText L (thetaText, tfText) of
                 NONE => (AtlasFFI.atlas_group_free L; NONE)
               | SOME xL =>
                   let
                     val rhoL = parseRatWeightText (AtlasFFI.atlas_group_rho_text L)
                     val lambdaL = ratvecAdd (Lattice.ratvecSub (lambdaP, rhoG), rhoL)
                     val pL0 =
                       AtlasFFI.atlas_param_new_from_lambda_nu_text
                         ( L
                         , xL
                         , AllParameters.intsToCText (#nums lambdaL)
                         , #den lambdaL
                         , AllParameters.intsToCText (#nums nuP)
                         , #den nuP
                         )
                     val () =
                       if pL0 = Foreign.Memory.null then
                         raise Fail ("Induction.good_range: p_L construction failed: " ^ AtlasFFI.atlas_last_error ())
                       else
                         ()
                     val pL1 = AtlasFFI.atlas_param_normalise pL0
                     val () = AtlasFFI.atlas_param_free pL0
                     val () =
                       if pL1 = Foreign.Memory.null then
                         raise Fail ("Induction.good_range: normalise failed: " ^ AtlasFFI.atlas_last_error ())
                       else
                         ()
                     val okFinal = AtlasFFI.atlas_param_is_final pL1 = 1
                     val okGood = if okFinal then is_weakly_good (pL1, G, L) else false
                     val same = if AtlasFFI.atlas_group_semisimple_rank L = AtlasFFI.atlas_group_semisimple_rank G then "1" else "0"
                   in
                     if not okGood then
                       (AtlasFFI.atlas_param_free pL1; AtlasFFI.atlas_group_free L; NONE)
                     else
                       let
                         val unitary = if AtlasFFI.atlas_param_is_unitary pL1 = 1 then "1" else "0"
                         val rf = AtlasFFI.atlas_group_real_form_number L
                         val desc = "type=" ^ groupTypeString L ^ " rf=" ^ Int.toString rf ^ " S=" ^ subsetString S

                         val xGopt =
                           findKGBByInvolutionAndTorusFactorText
                             G
                             ( AtlasFFI.atlas_group_kgb_involution_matrix_text (L, xL)
                             , AtlasFFI.atlas_kgb_torus_factor_text (L, xL)
                             )
                         val matches =
                           case xGopt of
                             NONE => false
                           | SOME xG => induced_matches (p, G, xG)
                       in
                         AtlasFFI.atlas_param_free pL1;
                         AtlasFFI.atlas_group_free L;
                         if matches then SOME ("1|" ^ same ^ "|" ^ unitary ^ "|" ^ desc) else NONE
                       end
                   end)
        end

      val tsp = theta_stable_parabolics_with G x
    in
      case List.mapPartial tryOne tsp of
        [] => "0|||"
      | txt :: _ => txt
    end
end
