use "atlas-scripts-sml/Coordinates.sml";
use "atlas-scripts-sml/coordinates_at.sml";

(*
  File: atlas-scripts-sml/coordinates.sml

  Purpose
  - Compatibility wrapper matching the `.at` filename `coordinates.at`.
  - The `coordinates.at` translation lives in `atlas-scripts-sml/coordinates_at.sml`
    as `structure CoordinatesAT` (change-of-basis routines).
  - This wrapper also loads `atlas-scripts-sml/Coordinates.sml` which provides
    additional coordinate helpers used by the FPP code (not a direct `.at` port).
*)
