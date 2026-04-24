(* Smoke test for type D Weyl-group character tables. *)

use "atlas-scripts-sml/classical_character_tables.sml";
use "atlas-scripts-sml/character_tables.sml";

fun expect (name, cond) = if cond then () else raise Fail name

fun main () =
  let
    (* D3 has Weyl group isomorphic to A3 (order 24). *)
    val ctD3 = ClassicalCharacterTables.character_table_D 3
    val () = expect ("D3 orthogonal", CharacterTables.check_orthogonality ctD3)
    val () = expect ("D3 order", CharacterTables.order_W ctD3 = 24)
    val () = expect ("D3 special idempotent", List.all (fn i => CharacterTables.special (ctD3, CharacterTables.special (ctD3, i)) = CharacterTables.special (ctD3, i)) (List.tabulate (CharacterTables.n_irreps ctD3, fn i => i)))

    (* A small nontrivial case: D4 has order 192. *)
    val ctD4 = ClassicalCharacterTables.character_table_D 4
    val () = expect ("D4 orthogonal", CharacterTables.check_orthogonality ctD4)
    val () = expect ("D4 order", CharacterTables.order_W ctD4 = 192)
    val () = expect ("D4 special idempotent", List.all (fn i => CharacterTables.special (ctD4, CharacterTables.special (ctD4, i)) = CharacterTables.special (ctD4, i)) (List.tabulate (CharacterTables.n_irreps ctD4, fn i => i)))
  in
    print "test_classical_character_table_D_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);
