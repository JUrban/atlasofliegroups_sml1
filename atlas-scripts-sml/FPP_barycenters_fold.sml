use "atlas-scripts-sml/cofolded.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/FPP_fundamental_alcove.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/FPP_barycenters_fold.sml

  Purpose
  - Compute folded-FPP barycenters used in the F4 verification pipeline.
  - Uses the Atlas C++ routine `weyl::FPP_orbit_numers` (exposed via FFI) to
    enumerate affine Weyl orbits efficiently, then folds via the cofolded datum.

  Output
  - Lists of `Lattice.ratvec` values representing `gamma` barycenters.
*)
structure FPP_barycenters_fold = struct
  type ratvec = Lattice.ratvec

  (* Stable key `[den, nums...]` for normalization/deduplication. *)
  fun ratvecKey (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  (* Sort and unique rational vectors under `ratvecKey`. *)
  fun no_reps_ratvec (us: ratvec list) : ratvec list =
    Basic.sort_u_by (ratvecKey, Sort.rlex_leq) us

  (* Enumerate the orbit points of `gamma` in the affine Weyl group, keeping the
     same denominator as `gamma`. *)
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

  (* Barycenters of faces of a fixed dimension, folded via the cofolded datum. *)
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

  (* All barycenters across all face dimensions. *)
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
