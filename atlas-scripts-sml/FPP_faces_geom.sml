use "atlas-scripts-sml/FPP_faces_geom_fold.sml";

(*
  File: atlas-scripts-sml/FPP_faces_geom.sml

  Purpose
  - Compatibility wrapper matching the `.at` filename `FPP_faces_geom.at`.
  - The substantive implementation currently lives in
    `atlas-scripts-sml/FPP_faces_geom_fold.sml` (folded-FPP geometry utilities).

  Notes
  - Keeping this shim allows future `.at`→`.sml` translations to `use
    "atlas-scripts-sml/FPP_faces_geom.sml"` without having to remember the
    `_fold` suffix.
*)

structure FPP_faces_geom = FPP_faces_geom_fold;

