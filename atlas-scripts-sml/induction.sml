use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/Coordinates.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/parabolics.sml";

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
end

