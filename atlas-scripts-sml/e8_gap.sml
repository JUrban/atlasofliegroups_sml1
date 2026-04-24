use "atlas-scripts-sml/IntListData.sml";
use "atlas-scripts-sml/StringListData.sml";

(*
  File: atlas-scripts-sml/e8_gap.sml

  Purpose
  - SML translation of `atlas-scripts/e8_gap.at`.
  - Provides the processed GAP character-table data for W(E8) used by
    `character_table_E8.at`.

  Scope
  - This port is data-focused: it exposes the precomputed tables and power maps
    as SML values loaded from repo-local fixtures under `atlas-scripts-sml/data/`.
  - It does not attempt to reproduce the `.at` helper `e8_gap_class_nr` (string
    label lookup) since we store the digitized power maps directly.

  Provided values
  - `e8_gap_table` (112x112): integer character table rows.
  - `e8_gap_orders` (112): element orders per class (GAP class order).
  - `e8_gap_centralizer_sizes` (112): centralizer sizes per class (GAP order).
  - `e8_gap_classes` (112): string labels per class (GAP order).
  - `gap_power_2`, `gap_power_3`, `gap_power_5`, `gap_power_7` (112):
    digitized power maps: `gap_power_k[j]` is the class index of `x^k` when
    `x` lies in class `j`.
  - `e8_gap_profile_cols` (112 columns, length 5 each):
    the profile vectors used to map between class orderings, matching the `.at`
    `e8_gap_profile` intent.
*)

structure E8_gap = struct
  val e8_gap_table : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/e8_gap_table.txt"

  val e8_gap_orders : int list =
    IntListData.loadInts "atlas-scripts-sml/data/e8_gap_orders.txt"

  val e8_gap_centralizer_sizes : int list =
    IntListData.loadInts "atlas-scripts-sml/data/e8_gap_centralizer_sizes.txt"

  val e8_gap_classes : string list =
    StringListData.loadLines "atlas-scripts-sml/data/e8_gap_classes.txt"

  val gap_power_2 : int list =
    IntListData.loadInts "atlas-scripts-sml/data/gap_power_2.txt"
  val gap_power_3 : int list =
    IntListData.loadInts "atlas-scripts-sml/data/gap_power_3.txt"
  val gap_power_5 : int list =
    IntListData.loadInts "atlas-scripts-sml/data/gap_power_5.txt"
  val gap_power_7 : int list =
    IntListData.loadInts "atlas-scripts-sml/data/gap_power_7.txt"

  val () =
    if length e8_gap_table = 112 andalso List.all (fn row => length row = 112) e8_gap_table then
      ()
    else
      raise Fail "E8_gap: unexpected e8_gap_table shape"

  val () =
    if length e8_gap_orders = 112
       andalso length e8_gap_centralizer_sizes = 112
       andalso length e8_gap_classes = 112
       andalso length gap_power_2 = 112
       andalso length gap_power_3 = 112
       andalso length gap_power_5 = 112
       andalso length gap_power_7 = 112 then
      ()
    else
      raise Fail "E8_gap: unexpected vector lengths"

  val order_W = 696729600
  val e8_gap_sgn_index = 1
  val e8_gap_reflection_index = 67

  val sign_char = List.nth (e8_gap_table, e8_gap_sgn_index)
  val reflection_char = List.nth (e8_gap_table, e8_gap_reflection_index)

  val class_sizes : int list =
    List.map (fn k => order_W div k) e8_gap_centralizer_sizes

  val reflection_at_cube : int list =
    List.tabulate (112, fn j => List.nth (reflection_char, List.nth (gap_power_3, j)))

  (* Column profiles: list of 112 vectors of length 5. *)
  val e8_gap_profile_cols : int list list =
    List.tabulate
      (112, fn j =>
         [ List.nth (e8_gap_orders, j)
         , List.nth (class_sizes, j)
         , List.nth (sign_char, j)
         , List.nth (reflection_char, j)
         , List.nth (reflection_at_cube, j)
         ])
end

