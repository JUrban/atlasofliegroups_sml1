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
  - Implements character tables for the hyperoctahedral groups H_n (types B/C
    Weyl groups), using the signed analogue of Murnaghan–Nakayama implemented
    in `Combinatorics.hyperoctahedral_character`.

  Scope (current)
  - Provides `character_table_S(n)` producing a `CharacterTables.CharacterTable.t`
    whose conjugacy classes and irreps are both indexed by partitions of `n`.
  - This is enough to support type-A factors in `character_tables_reductive.sml`.
  - Provides `character_table_B(n)` and `character_table_C(n)` producing a
    `CharacterTables.CharacterTable.t` whose conjugacy classes and irreps are
    both indexed by bipartitions of `n`.
  - Provides `character_table_D(n)` producing a `CharacterTables.CharacterTable.t`
    whose conjugacy classes and irreps are indexed by the refined type-D
    parameter sets from `Combinatorics`:
      - `Combinatorics.D_class` (unsplit and split classes),
      - `Combinatorics.D_irrep` (unsplit and split irreps).

  Ordering conventions
  - Class ordering: `Partitions.partitions n` (smallest largest-part first).
  - Irrep ordering: the reverse of that list (so it goes from trivial `[n]`
    toward sign `[1,1,...,1]`), matching the `.at` code comment.
*)

structure ClassicalCharacterTables = struct
  type partition = Partitions.partition
  type bipartition = Combinatorics.bipartition
  type signed_cycles = Combinatorics.signed_cycles
  type D_class = Combinatorics.D_class
  type D_irrep = Combinatorics.D_irrep
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

  fun bipartition_as_irrep_label ((lambda, mu): bipartition) : string =
    "{ " ^ Partitions.toString lambda ^ " +|- " ^ Partitions.toString mu ^ " }"

  (* Irreps of H_n are indexed by bipartitions of total size n.

     Ordering here matches `.at` `BC_irreps(n)`:
     - distribute total size as i=n..0 (larger left size first),
     - within each size, enumerate partitions in reversed `Partitions.partitions`
       order so `[k]` comes before `[1,1,...,1]`. *)
  fun BC_irreps (n: int) : bipartition list =
    Combinatorics.pairs_of_total_sum (n, (fn k => List.rev (Partitions.partitions k)))

  fun find_identity_class (class_orders: int list, class_sizes: int list) : int =
    let
      fun loop i =
        if i >= length class_orders then
          raise Fail "ClassicalCharacterTables: no identity class found"
        else if List.nth (class_orders, i) = 1 andalso List.nth (class_sizes, i) = 1 then
          i
        else
          loop (i + 1)
    in
      loop 0
    end

  fun character_table_B (n: int) : character_table =
    if n < 0 then
      raise Fail "ClassicalCharacterTables.character_table_B: negative n"
    else
      let
        val class_list = Combinatorics.partition_pairs n
        val class_cycles : signed_cycles list = List.map Combinatorics.to_cycles class_list
        val class_names = List.map Combinatorics.signed_cycles_toString class_cycles
        val class_sizes = List.map Combinatorics.signed_cycle_class_size class_cycles
        val class_orders = List.map Combinatorics.signed_cycle_type_order class_cycles
        val wct = ClassTables.class_table_stub_from_orders_sizes (class_orders, class_sizes)

        val irreps_list = BC_irreps n
        val () =
          if length irreps_list = length class_list then
            ()
          else
            raise Fail "ClassicalCharacterTables.character_table_B: expected square table (bipartition count mismatch)"

        fun rowFor rep = List.map (fn cyc => Combinatorics.hyperoctahedral_character (rep, cyc)) class_cycles
        val irreps = List.map (fn rep => (rowFor rep, bipartition_as_irrep_label rep)) irreps_list

        val idj = find_identity_class (class_orders, class_sizes)
        val degrees = List.map (fn (row, _) => List.nth (row, idj)) irreps
      in
        CharacterTables.make (wct, class_names, irreps, degrees, (fn i => i))
      end

  (* The Weyl groups of type B_n and C_n are isomorphic (both are H_n), so the
     character tables coincide; we keep a separate entry point for parity with
     the `.at` library. *)
  fun character_table_C (n: int) : character_table = character_table_B n

  fun character_table_D (n: int) : character_table =
    if n < 2 then
      raise Fail "ClassicalCharacterTables.character_table_D: n<2 not supported"
    else
      let
        val class_list : D_class list = Combinatorics.D_classes n
        val class_names = List.map Combinatorics.D_class_toString class_list
        val class_sizes = List.map Combinatorics.D_class_size class_list
        val class_orders = List.map Combinatorics.D_cycle_type_order class_list
        val wct = ClassTables.class_table_stub_from_orders_sizes (class_orders, class_sizes)

        val irrep_list : D_irrep list = Combinatorics.D_irreps n
        val () =
          if length irrep_list = length class_list then
            ()
          else
            raise Fail "ClassicalCharacterTables.character_table_D: expected square table"

        fun rowFor rep = List.map (fn c => Combinatorics.D_character (rep, c)) class_list
        val irreps = List.map (fn rep => (rowFor rep, Combinatorics.D_irrep_toString rep)) irrep_list

        val idj = find_identity_class (class_orders, class_sizes)
        val degrees = List.map (fn (row, _) => List.nth (row, idj)) irreps
      in
        CharacterTables.make (wct, class_names, irreps, degrees, (fn i => i))
      end
end
