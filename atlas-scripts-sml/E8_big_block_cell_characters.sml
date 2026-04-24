use "atlas-scripts-sml/IntListData.sml";

(*
  File: atlas-scripts-sml/E8_big_block_cell_characters.sml

  Purpose
  - SML translation of the pure-data script:
      `atlas-scripts/E8_big_block_cell_characters.at`
  - The original `.at` file defines `cell_characters_big`, a list of integer
    vectors encoding Weyl cell characters in the ordering used by the E8 “big
    block” computations.

  Data source
  - `atlas-scripts-sml/data/E8_big_block_cell_characters.txt`
    (one character per line, space-separated integers)
*)

structure E8_big_block_cell_characters = struct
  val cell_characters_big : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/E8_big_block_cell_characters.txt"
end

