(* Smoke test for hyperoctahedral (type B/C) character tables. *)

use "atlas-scripts-sml/classical_character_tables.sml";
use "atlas-scripts-sml/character_tables.sml";

fun expect (name, cond) = if cond then () else raise Fail name

fun main () =
  let
    val ctB2 = ClassicalCharacterTables.character_table_B 2
    val () = expect ("B2 orthogonal", CharacterTables.check_orthogonality ctB2)
    val () = expect ("B2 order", CharacterTables.order_W ctB2 = 8)

    val ctC3 = ClassicalCharacterTables.character_table_C 3
    val () = expect ("C3 orthogonal", CharacterTables.check_orthogonality ctC3)
    val () = expect ("C3 order", CharacterTables.order_W ctC3 = 48)
  in
    print "test_classical_character_table_BC_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

