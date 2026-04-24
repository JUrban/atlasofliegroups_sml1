use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/character_table_E6.sml";
use "atlas-scripts-sml/character_table_E7.sml";
use "atlas-scripts-sml/character_table_E8.sml";
use "atlas-scripts-sml/character_table_F.sml";
use "atlas-scripts-sml/character_table_G.sml";

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
  - Classical types A/B/C/D and the general `combine` logic are not yet ported.

  Notes
  - For G2, the current implementation constructs the split group `G2_s` via
    `AtlasFFI.atlas_group_new_simple` rather than using the `RootDatum` input.
    This matches the intent for Weyl-group data, but will be refined once we
    have a proper `WeylClassTable` builder from `RootDatum` alone.
*)

structure CharacterTablesReductive = struct
  type character_table = CharacterTables.CharacterTable.t

  fun simple_character_table (lt: LieType.t) : character_table =
    (case lt of
       [(#"E", 6)] => CharacterTable_E6.character_table_E6_magma ()
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
      case lt of
        [_] => simple_character_table lt
      | _ => raise Fail "CharacterTablesReductive.character_table: multi-factor not implemented"
    end
end

