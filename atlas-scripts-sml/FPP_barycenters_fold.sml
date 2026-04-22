use "atlas-scripts-sml/cofolded.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/FPP_fundamental_alcove.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(* Folded-FPP barycenters, using C++ `weyl::FPP_orbit_numers` to enumerate affine orbits. *)
structure FPP_barycenters_fold = struct
  type ratvec = Lattice.ratvec

  fun ratvecKey (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  fun no_reps_ratvec (us: ratvec list) : ratvec list =
    Basic.sort_u_by (ratvecKey, Sort.rlex_leq) us

  fun orbit_points_same_denom (rd: RootDatum.t, gamma: ratvec) : ratvec list =
    let
      val gamma = Lattice.ratvecNormalize gamma
      val denom = #den gamma
      val numsMat = RootDatum.FPP_orbit_numers (rd, gamma) (* rows are numerators *)
      val (k, r) = IntMatrix.matShape numsMat
      fun mkRow row =
        let
          val () = if length row = r then () else raise Fail "orbit_points_same_denom: ragged"
        in
          Lattice.ratvecNormalize {den = denom, nums = row}
        end
    in
      List.map mkRow numsMat
    end

  fun barycenters_dim (g: AtlasFFI.group, dim: int) : ratvec list =
    let
      val (affd, m, j0) = Cofolded.cofolded g
      val () =
        if j0 = ~1 then ()
        else raise Fail "FPP_barycenters_fold: j0<>-1 case not yet implemented"

      val faces0 = FPP_fundamental_alcove.faces_fundamental (affd, dim)
      val bary0s = List.map FPP_fundamental_alcove.barycenter faces0
      val orb = List.concat (List.map (fn b => orbit_points_same_denom (affd, b)) bary0s)
      val mapped = List.map (fn u => Lattice.matVecMulRatvec m u) orb
      val () = RootDatum.free affd
    in
      no_reps_ratvec mapped
    end

  fun barycenters_all (g: AtlasFFI.group) : ratvec list =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val r = RootDatum.rank rd
      val () = RootDatum.free rd
      val byDim = List.tabulate (r + 1, fn d => barycenters_dim (g, d))
    in
      no_reps_ratvec (List.concat byDim)
    end
end

