use "atlas-scripts-sml/cofolded.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/FPP_fundamental_alcove.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(* Folded-FPP vertices, using C++ `weyl::FPP_orbit_numers` to enumerate affine orbits. *)
structure FPP_vertices_fold = struct
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
      if k = 0 then [] else List.map mkRow numsMat
    end

  fun vertices (g: AtlasFFI.group) : ratvec list =
    let
      val (affd, m, j0) = Cofolded.cofolded g
      val () =
        if j0 = ~1 then ()
        else raise Fail "FPP_vertices_fold.vertices: j0<>-1 case not yet implemented"

      val fvs = FPP_fundamental_alcove.fundamental_vertices affd
      val orb = List.concat (List.map (fn v => orbit_points_same_denom (affd, v)) fvs)
      val mapped = List.map (fn u => Lattice.matVecMulRatvec m u) orb
      val () = RootDatum.free affd
    in
      no_reps_ratvec mapped
    end
end

