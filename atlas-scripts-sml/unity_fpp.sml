use "atlas-scripts-sml/unity.sml";
use "atlas-scripts-sml/FPP_faces_herm.sml";

(*
  File: atlas-scripts-sml/unity_fpp.sml

  Purpose
  - Lightweight “glue” between `Unity` and `FPP_faces_herm`.
  - Keeps `unity.sml` independent of the (large) FPP module dependency graph,
    while still providing the LKTs-based height scheduling utilities used by
    the `.at` FPP scripts.
*)

structure UnityFPP = struct
  type param = AtlasFFI.param

  (* LKTs-based height list, mirroring `next_heights(p,depth)` usage from the
     FPP scripts (`FPP_faces_herm.at`). *)
  fun next_heights_lkts (p: param, depth: int) : int list =
    FPP_faces_herm.next_heights (p, depth)

  (* Single “next height” above the lowest K-types, or `~1` if none found. *)
  fun next_height (p: param) : int =
    FPP_faces_herm.next_height p
end

