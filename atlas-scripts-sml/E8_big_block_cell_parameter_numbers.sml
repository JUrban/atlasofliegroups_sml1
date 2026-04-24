use "atlas-scripts-sml/IntListData.sml";

(*
  File: atlas-scripts-sml/E8_big_block_cell_parameter_numbers.sml

  Purpose
  - SML translation of the pure-data script:
      `atlas-scripts/E8_big_block_cell_parameter_numbers.at`
  - The original `.at` file defines a large literal list `cells_big` giving,
    for each Weyl cell, the parameter numbers belonging to that cell in the
    E8 “big block” computations.

  Implementation strategy
  - The literal is stored as a text fixture:
      `atlas-scripts-sml/data/E8_big_block_cell_parameter_numbers.txt`
    with one cell per line (space-separated integers), and loaded on demand.
  - This avoids multi-megabyte SML source while keeping runtime independent
    of any `.at` script evaluation.
*)

structure E8_big_block_cell_parameter_numbers = struct
  val cells_big : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/E8_big_block_cell_parameter_numbers.txt"
end

