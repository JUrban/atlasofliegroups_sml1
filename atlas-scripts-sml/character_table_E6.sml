use "atlas-scripts-sml/IntListData.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/class_tables.sml";

(*
  File: atlas-scripts-sml/character_table_E6.sml

  Purpose
  - Partial SML translation of `atlas-scripts/character_table_E6.at`.
  - This port currently focuses on the *precomputed Magma-order character data*
    used by the `.at` character-table builder, without attempting to re-create
    the full `CharacterTable` / `WeylClassTable` infrastructure in SML.

  What is provided
  - `e6_table`: the full list of irreducible characters of W(E6) in Magma class
    order, as integer vectors (rows).
  - `e6_profile_cols`: for each conjugacy class (in Magma order), the profile
    vector `[order, class_size, sign_value, reflection_value]`.
    This matches the data shape used by `Magma_positions_E6` in the `.at` file.
  - `to_special_E6`: the “to special” index map used by the `.at` file.

  Data source
  - `atlas-scripts-sml/data/character_table_E6_table.txt`
  - `atlas-scripts-sml/data/character_table_E6_orders_magma.txt`
  - `atlas-scripts-sml/data/character_table_E6_sizes_magma.txt`
*)

structure CharacterTable_E6 = struct
  val e6_table : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/character_table_E6_table.txt"

  val e6_orders_magma : int list =
    IntListData.loadInts "atlas-scripts-sml/data/character_table_E6_orders_magma.txt"

  val e6_sizes_magma : int list =
    IntListData.loadInts "atlas-scripts-sml/data/character_table_E6_sizes_magma.txt"

  val () =
    if length e6_table = 25 andalso List.all (fn row => length row = 25) e6_table then
      ()
    else
      raise Fail "CharacterTable_E6: unexpected e6_table shape"

  val () =
    if length e6_orders_magma = 25 andalso length e6_sizes_magma = 25 then
      ()
    else
      raise Fail "CharacterTable_E6: unexpected orders/sizes length"

  val sign_index = 1
  val reflection_index = 2

  val sign_char = List.nth (e6_table, sign_index)
  val reflection_char = List.nth (e6_table, reflection_index)

  (* Column profiles in the `.at` sense: list of 25 vectors of length 4. *)
  val e6_profile_cols : int list list =
    List.tabulate
      (25, fn j =>
         [ List.nth (e6_orders_magma, j)
         , List.nth (e6_sizes_magma, j)
         , List.nth (sign_char, j)
         , List.nth (reflection_char, j)
         ])

  val to_special_E6_table : int list =
    [ 0, 1, 2, 3
    , 21
    , 14, 14, 15, 15
    , 9, 10, 21
    , 12, 13
    , 14, 15
    , 16, 17, 21
    , 19, 20
    , 21
    , 22, 23
    , 21
    ]

  fun to_special_E6 (i: int) : int =
    if i < 0 orelse i >= length to_special_E6_table then
      raise Fail "CharacterTable_E6.to_special_E6: index out of range"
    else
      List.nth (to_special_E6_table, i)

  (* Build a usable `CharacterTables.CharacterTable.t` in Magma class order. *)
  fun character_table_E6_magma () : CharacterTables.CharacterTable.t =
    let
      val wct = ClassTables.class_table_stub_from_orders_sizes (e6_orders_magma, e6_sizes_magma)
      val class_names = List.tabulate (25, fn i => "E6_class_" ^ Int.toString i)
      val irreps = List.map (fn row => (row, "irrep_" ^ Int.toString (List.nth (row, 0)))) e6_table
      val degrees = List.map (fn row => List.nth (row, 0)) e6_table
    in
      CharacterTables.make (wct, class_names, irreps, degrees, to_special_E6)
    end
end
