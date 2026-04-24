(* Smoke test for `character_table_reps.sml`: induction from trivial subgroup gives regular character. *)

use "atlas-scripts-sml/character_table_reps.sml";
use "atlas-scripts-sml/character_table_G.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/class_tables.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun expect (name, cond) = if cond then () else raise Fail name

fun main () =
  let
    val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
    val ctG2 = CharacterTable_G.character_table_G2 g
    val wctG2 = #class_table ctG2

    val orderG = List.foldl (op +) 0 (#class_sizes wctG2)
    val () = expect ("|W(G2)|", orderG = 12)

    (* Trivial subgroup: 1 class, size 1, order 1, only element is identity. *)
    val wctTriv : WeylClassTable.t =
      { n_classes = 1
      , class_representatives = [ [] ]
      , class_sizes = [ 1 ]
      , class_orders = [ 1 ]
      , class_of = (fn _ => 0)
      , class_power = (fn (_, _) => 0)
      }

    val piTriv = [ 1 ]
    val induced = CharacterTableReps.induce_character_wct (wctTriv, wctG2, (fn w => w), piTriv)

    (* Regular character: |W| at identity class, 0 elsewhere. *)
    val () = expect ("regular at id", List.nth (induced, 0) = 12)
    val () = expect ("regular elsewhere", List.all (fn x => x = 0) (List.drop (induced, 1)))

    (* Decomposition of regular character equals degrees. *)
    val mults = CharacterTables.decompose (ctG2, induced)
    val degrees = List.map (fn (chi, _) => List.nth (chi, 0)) CharacterTable_G.irreps
    val () = expect ("regular decomposes to degrees", mults = degrees)

    val () = AtlasFFI.atlas_group_free g
  in
    print "test_induce_character_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

