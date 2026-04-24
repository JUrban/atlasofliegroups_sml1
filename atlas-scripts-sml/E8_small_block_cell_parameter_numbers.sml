use "atlas-scripts-sml/IntListData.sml";

(*
  File: atlas-scripts-sml/E8_small_block_cell_parameter_numbers.sml

  Purpose
  - SML translation of the pure-data script:
      `atlas-scripts/E8_small_block_cell_parameter_numbers.at`
  - Defines `cells_small`, the list of parameter-number lists for the E8
    “small block” Weyl cells (in the ordering used by the original script).

  Data source
  - `atlas-scripts-sml/data/E8_small_block_cell_parameter_numbers.txt`
*)

structure E8_small_block_cell_parameter_numbers = struct
  val cells_small : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/E8_small_block_cell_parameter_numbers.txt"
end

