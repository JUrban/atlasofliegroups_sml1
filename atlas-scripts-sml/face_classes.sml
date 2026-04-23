use "atlas-scripts-sml/FaceClasses.sml";

(*
  File: atlas-scripts-sml/face_classes.sml

  Purpose
  - Compatibility wrapper matching the `.at` filename `face_classes.at`.
  - The implementation lives in `atlas-scripts-sml/FaceClasses.sml` as
    `structure FaceClasses`.

  Notes
  - Some translations prefer to keep the `.at`-style module name; we provide
    a couple of aliases in addition to the canonical `FaceClasses`.
*)

structure Face_classes = FaceClasses
structure face_classes = FaceClasses

