use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/character_table_E6.sml";
use "atlas-scripts-sml/character_table_E7.sml";
use "atlas-scripts-sml/character_table_E8.sml";
use "atlas-scripts-sml/character_table_F.sml";
use "atlas-scripts-sml/character_table_G.sml";
use "atlas-scripts-sml/classical_character_tables.sml";

(*
  File: atlas-scripts-sml/character_tables_reductive.sml

  Purpose
  - Partial SML translation of `atlas-scripts/character_tables_reductive.at`.
  - The `.at` file provides Weyl-group character tables for *reductive* root
    data by decomposing into simple factors and combining tables.

  Scope of this port (current)
  - Implements only the “simple exceptional” dispatchers:
      - E6/E7/E8 via the precomputed Magma/GAP tables already ported.
      - F4 and G2 via the Kondo-order and fixed-order tables already ported.
  - Implements classical type A (via symmetric groups) and types B/C (via the
    hyperoctahedral character recursion).
  - Type D is not yet ported.

  Notes
  - For G2, the current implementation constructs the split group `G2_s` via
    `AtlasFFI.atlas_group_new_simple` rather than using the `RootDatum` input.
    This matches the intent for Weyl-group data, but will be refined once we
    have a proper `WeylClassTable` builder from `RootDatum` alone.
*)

structure CharacterTablesReductive = struct
  type character_table = CharacterTables.CharacterTable.t

  fun joinWith (sep: string) (xs: string list) : string =
    (case xs of
       [] => ""
     | [x] => x
     | _ => String.concatWith sep xs)

  fun lcm (a: int, b: int) : int =
    let
      fun gcd (x: int, y: int) : int =
        if y = 0 then Int.abs x else gcd (y, x mod y)
    in
      if a = 0 orelse b = 0 then 0 else Int.abs (a div gcd (a, b) * b)
    end

  fun product (xs: int list) : int = List.foldl (op * ) 1 xs

  (* Mixed radix encoding/decoding with bases `bs = [b0,b1,...]` where digits
     satisfy `0 <= di < bi`.

     Convention: index = d0 + b0*(d1 + b1*(d2 + ...)).
  *)
  fun mixedRadixEncode (bases: int list, digits: int list) : int =
    let
      fun loop ([], [], acc, _) = acc
        | loop (b :: bs, d :: ds, acc, mul) =
            if d < 0 orelse d >= b then
              raise Fail "CharacterTablesReductive.mixedRadixEncode: digit out of range"
            else
              loop (bs, ds, acc + mul * d, mul * b)
        | loop _ = raise Fail "CharacterTablesReductive.mixedRadixEncode: length mismatch"
    in
      loop (bases, digits, 0, 1)
    end

  fun mixedRadixDecode (bases: int list, idx0: int) : int list =
    let
      val () = if idx0 < 0 then raise Fail "CharacterTablesReductive.mixedRadixDecode: negative idx" else ()
      fun loop ([], _, acc) = List.rev acc
        | loop (b :: bs, idx, acc) =
            let
              val d = idx mod b
              val idx' = idx div b
            in
              loop (bs, idx', d :: acc)
            end
    in
      loop (bases, idx0, [])
    end

  fun allWords (bases: int list) : int list list =
    let
      val n = product bases
    in
      List.tabulate (n, fn i => mixedRadixDecode (bases, i))
    end

  (* Combine already-constructed factor tables into a product table.

     This is the SML analogue of the `.at` `combine(rd,factors)` logic, but
     operates directly on tables (and uses a stub class table for the product).
  *)
  fun combine_tables (factors: character_table list) : character_table =
    (case factors of
       [] => raise Fail "CharacterTablesReductive.combine_tables: empty"
     | [ct] => ct
     | _ =>
         let
           val ns = List.map CharacterTables.n_irreps factors
           val words = allWords ns
           val n = length words

           fun tensorAt (irDigits: int list, clsDigits: int list) : int =
             List.foldl
               (op * )
               1
               (ListPair.mapEq
                  (fn (ir, (ct, cls)) => List.nth (CharacterTables.character (ct, ir), cls))
                  (irDigits, ListPair.zipEq (factors, clsDigits)))

           val class_orders =
             List.map
               (fn clsDigits =>
                  List.foldl (fn ((ct, j), acc) => lcm (acc, List.nth (#class_orders (#class_table ct), j))) 1
                    (ListPair.zipEq (factors, clsDigits)))
               words

           val class_sizes =
             List.map
               (fn clsDigits =>
                  List.foldl (fn ((ct, j), acc) => acc * List.nth (#class_sizes (#class_table ct), j)) 1
                    (ListPair.zipEq (factors, clsDigits)))
               words

           val wct = ClassTables.class_table_stub_from_orders_sizes (class_orders, class_sizes)

           val class_names =
             List.map
               (fn clsDigits =>
                  joinWith "*" (ListPair.mapEq (fn (ct, j) => CharacterTables.class_label (ct, j)) (factors, clsDigits)))
               words

           val irreps : (CharacterTables.char_row * string) list =
             List.map
               (fn irDigits =>
                  let
                    val name =
                      joinWith "." (ListPair.mapEq (fn (ct, i) => CharacterTables.irreducible_label (ct, i)) (factors, irDigits))
                    val row =
                      List.map (fn clsDigits => tensorAt (irDigits, clsDigits)) words
                  in
                    (row, name)
                  end)
               words

           val degrees =
             List.map
               (fn irDigits =>
                  List.foldl (fn ((ct, i), acc) => acc * CharacterTables.degree (ct, i)) 1
                    (ListPair.zipEq (factors, irDigits)))
               words

           fun to_special idx =
             let
               val irDigits = mixedRadixDecode (ns, idx)
               val spDigits =
                 ListPair.mapEq (fn (ct, i) => CharacterTables.special (ct, i)) (factors, irDigits)
             in
               mixedRadixEncode (ns, spDigits)
             end
         in
           CharacterTables.make (wct, class_names, irreps, degrees, to_special)
         end)

  fun simple_character_table (lt: LieType.t) : character_table =
    (case lt of
       [(#"A", r)] => ClassicalCharacterTables.character_table_S (r + 1)
     | [(#"B", r)] => ClassicalCharacterTables.character_table_B r
     | [(#"C", r)] => ClassicalCharacterTables.character_table_C r
     | [(#"D", r)] => ClassicalCharacterTables.character_table_D r
     | [(#"E", 6)] => CharacterTable_E6.character_table_E6_magma ()
     | [(#"E", 7)] => CharacterTable_E7.character_table_E7_magma ()
     | [(#"E", 8)] => CharacterTable_E8.character_table_E8_gap ()
     | [(#"F", 4)] =>
         let
           val rd = RootDatum.newSimple (#"F", 4, false)
           val ct = CharacterTable_F.character_table_F4 rd
           val () = RootDatum.free rd
         in
           ct
         end
     | [(#"G", 2)] =>
         let
           val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
           val ct = CharacterTable_G.character_table_G2 g
           val () = AtlasFFI.atlas_group_free g
         in
           ct
         end
     | _ => raise Fail "CharacterTablesReductive.simple_character_table: not implemented for this Lie type")

  (* RootDatum-directed dispatcher for simple root data.

     This mirrors the `.at` `character_table_simple` logic for exceptional
     types, but currently rejects multi-factor root data. *)
  fun character_table (rd: RootDatum.t) : character_table =
    let
      val lt = RootDatum.lieType rd
    in
      combine_tables (List.map (fn sf => simple_character_table [sf]) lt)
    end
end
