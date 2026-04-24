use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/partitions.sml";
use "atlas-scripts-sml/combinatorics.sml";
use "atlas-scripts-sml/class_tables.sml";
use "atlas-scripts-sml/character_tables.sml";

(*
  File: atlas-scripts-sml/classical_character_tables.sml

  Purpose
  - Partial SML translation of `atlas-scripts/classical_character_tables.at`.
  - Implements character tables for the symmetric group S_n (type A Weyl
    groups), using the Murnaghan–Nakayama rule.

  Scope (current)
  - Provides `character_table_S(n)` producing a `CharacterTables.CharacterTable.t`
    whose conjugacy classes and irreps are both indexed by partitions of `n`.
  - This is enough to support type-A factors in `character_tables_reductive.sml`.
  - Types B/C/D and their bipartition combinatorics are not implemented yet.

  Ordering conventions
  - Class ordering: `Partitions.partitions n` (smallest largest-part first).
  - Irrep ordering: the reverse of that list (so it goes from trivial `[n]`
    toward sign `[1,1,...,1]`), matching the `.at` code comment.
*)

structure ClassicalCharacterTables = struct
  type partition = Partitions.partition
  type character_table = CharacterTables.CharacterTable.t

  fun toIntChecked (where', x: IntInf.int) : int =
    let
      val maxI = IntInf.fromInt (Option.valOf Int.maxInt)
      val minI = IntInf.fromInt (Option.valOf Int.minInt)
    in
      if x > maxI orelse x < minI then
        raise Fail ("ClassicalCharacterTables." ^ where' ^ ": overflow")
      else
        IntInf.toInt x
    end

  fun character_table_S (n: int) : character_table =
    if n < 0 then
      raise Fail "ClassicalCharacterTables.character_table_S: negative n"
    else
      let
        val parts = Partitions.partitions n
        val class_names = List.map Partitions.toString parts
        val class_sizes = List.map Combinatorics.cycle_class_size parts
        val class_orders = List.map Combinatorics.cycle_type_order parts
        val wct = ClassTables.class_table_stub_from_orders_sizes (class_orders, class_sizes)

        val irreps_parts = List.rev parts
        fun rowFor lam = List.map (fn cyc => Combinatorics.Murnaghan_Nakayama (lam, cyc)) parts
        val irreps = List.map (fn lam => (rowFor lam, Partitions.toString lam)) irreps_parts

        val degrees = List.map (fn lam => toIntChecked ("degree", Partitions.dim_rep lam)) irreps_parts
      in
        CharacterTables.make (wct, class_names, irreps, degrees, (fn i => i))
      end
end

