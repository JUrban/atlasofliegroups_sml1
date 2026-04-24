use "atlas-scripts-sml/e8_gap.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/class_tables.sml";

(*
  File: atlas-scripts-sml/character_table_E8.sml

  Purpose
  - Partial SML translation of `atlas-scripts/character_table_E8.at`.
  - This port is deliberately *data-oriented*: it exposes the precomputed
    GAP-order character table of W(E8) and the auxiliary “profile columns”
    used for reordering/matching, without requiring the full `.at` machinery
    (`WeylClassTable`, `class_tables`, `character_tables`).

  What the original `.at` script does (high-level)
  - Loads the processed GAP character table (`e8_gap.at`).
  - Given an Atlas `WeylClassTable` order, computes a permutation between the
    Atlas class ordering and the GAP ordering by comparing 5-component
    “profiles”:
      `[order, class_size, sign, reflection, reflection(x^3)]`.
  - Reorders the GAP table into Atlas order and wraps it into a `CharacterTable`.

  What this SML port provides
  - `e8_table` (112x112): the integer character table rows in GAP class order.
  - `e8_profile_cols` (112 columns, length 5): the profile vectors in GAP order.
  - `to_special_E8`: the “to special” index map copied from the `.at` file.

  Notes / limitations
  - This module does NOT implement `character_table_E8(WeylClassTable)` since
    we do not yet have the SML equivalent of `WeylClassTable` and class-table
    infrastructure; it is intended as a building block for that future port.
*)

structure CharacterTable_E8 = struct
  val e8_table : int list list = E8_gap.e8_gap_table
  val e8_profile_cols : int list list = E8_gap.e8_gap_profile_cols
  val e8_orders : int list = E8_gap.e8_gap_orders
  val e8_class_sizes : int list = E8_gap.class_sizes
  val e8_class_labels : string list = E8_gap.e8_gap_classes

  val () =
    if length e8_table = 112 andalso List.all (fn row => length row = 112) e8_table then
      ()
    else
      raise Fail "CharacterTable_E8: unexpected e8_table shape"

  val () =
    if length e8_profile_cols = 112 andalso List.all (fn v => length v = 5) e8_profile_cols then
      ()
    else
      raise Fail "CharacterTable_E8: unexpected e8_profile_cols shape"

  val to_special_E8_table : int list =
    [ 0, 1, 71, 72, 4, 5, 52, 14, 15, 71, 72, 52
    , 42, 43, 14, 15, 52, 28, 29, 42, 43, 21, 22, 23
    , 24, 52, 96, 97, 28, 29, 52, 63, 64, 52, 53, 54
    , 42, 43, 38, 92, 93, 52, 42, 43, 42, 43, 52, 96
    , 97, 52, 50, 51, 52, 53, 54, 52, 56, 57, 52, 110
    , 111, 104, 62, 63, 64, 65, 66, 67, 68, 92, 93, 71
    , 72, 14, 15, 52, 28, 29, 92, 93, 80, 81, 52, 50
    , 51, 92, 93, 52, 53, 54, 50, 51, 92, 93, 110, 111
    , 96, 97, 52, 99, 100, 63, 64, 52, 104, 62, 106, 107
    , 108, 109, 110, 111
    ]

  val () =
    if length to_special_E8_table = 112 then
      ()
    else
      raise Fail "CharacterTable_E8: unexpected to_special_E8_table length"

  fun to_special_E8 (i: int) : int =
    if i < 0 orelse i >= length to_special_E8_table then
      raise Fail "CharacterTable_E8.to_special_E8: index out of range"
    else
      List.nth (to_special_E8_table, i)

  (* Build a usable `CharacterTables.CharacterTable.t` in GAP class order. *)
  fun character_table_E8_gap () : CharacterTables.CharacterTable.t =
    let
      val wct = ClassTables.class_table_stub_from_orders_sizes (e8_orders, e8_class_sizes)
      val class_names =
        List.tabulate
          (112, fn i =>
             "E8_" ^ List.nth (e8_class_labels, i) ^ "_class_" ^ Int.toString i)
      val irreps = List.map (fn row => (row, "irrep_" ^ Int.toString (List.nth (row, 0)))) e8_table
      val degrees = List.map (fn row => List.nth (row, 0)) e8_table
    in
      CharacterTables.make (wct, class_names, irreps, degrees, to_special_E8)
    end
end
