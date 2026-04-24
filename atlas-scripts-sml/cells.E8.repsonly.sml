use "atlas-scripts-sml/IntListData.sml";

(*
  File: atlas-scripts-sml/cells.E8.repsonly.sml

  Purpose
  - SML translation of the pure-data script:
      `atlas-scripts/cells.E8.repsonly.at`
  - The `.at` file defines `cells`, a (large) list of Weyl cells encoded as
    lists of parameter numbers.

  Note on naming
  - The file name is kept to match the original `.at` base name, but the SML
    structure name is normalized (dots are not permitted in identifiers).
*)

structure Cells_E8_repsonly = struct
  val cells : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/cells.E8.repsonly.txt"
end

