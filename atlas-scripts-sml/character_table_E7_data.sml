use "atlas-scripts-sml/IntListData.sml";

(*
  File: atlas-scripts-sml/character_table_E7_data.sml

  Purpose
  - SML translation of the *data-only* parts of:
      `atlas-scripts/character_table_E7_data.at`
  - The original `.at` file preloads Weyl conjugacy class data for E7
    (representative words, sizes, orders, and a power table) used by
    character-table code.

  What's included here
  - `class_words_E7 : int list list`
  - `class_orders_E7 : int list`
  - `class_sizes_E7 : int list`
  - `class_centralizer_orders_E7 : int list`
  - `class_powers_E7 : int list list`

  What's NOT included (yet)
  - Construction of actual `WeylElt` / class representative objects; the SML
    port currently represents Weyl elements via words or action matrices, and
    the full character-table stack has not been ported.

  Data source
  - Repo-local fixtures under `atlas-scripts-sml/data/` generated from the
    `.at` file:
      - `character_table_E7_class_words.txt`
      - `character_table_E7_class_orders.txt`
      - `character_table_E7_class_sizes.txt`
      - `character_table_E7_class_centralizer_orders.txt`
      - `character_table_E7_class_powers.txt`
*)

structure CharacterTable_E7_Data = struct
  val class_words_E7 : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/character_table_E7_class_words.txt"

  val class_orders_E7 : int list =
    IntListData.loadInts "atlas-scripts-sml/data/character_table_E7_class_orders.txt"

  val class_sizes_E7 : int list =
    IntListData.loadInts "atlas-scripts-sml/data/character_table_E7_class_sizes.txt"

  val class_centralizer_orders_E7 : int list =
    IntListData.loadInts "atlas-scripts-sml/data/character_table_E7_class_centralizer_orders.txt"

  val class_powers_E7 : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/character_table_E7_class_powers.txt"
end

