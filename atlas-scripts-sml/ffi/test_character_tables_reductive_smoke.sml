(* Smoke test for `character_tables_reductive.sml` (exceptional-only dispatcher). *)

use "atlas-scripts-sml/character_tables_reductive.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";

fun expect (name, cond) = if cond then () else raise Fail name

fun main () =
  let
    val ctG2 = CharacterTablesReductive.simple_character_table (LieType.parse "G2")
    val () = expect ("G2 orthogonal", CharacterTables.check_orthogonality ctG2)
    val () = expect ("G2 order", CharacterTables.order_W ctG2 = 12)

    val ctA3 = CharacterTablesReductive.simple_character_table (LieType.parse "A3")
    val () = expect ("A3 orthogonal", CharacterTables.check_orthogonality ctA3)
    val () = expect ("A3 order", CharacterTables.order_W ctA3 = 24)

    (* Product case: G2 x G2 *)
    val rdG2G2 = RootDatum.fromLieType (LieType.parse "G2G2")
    val ctG2G2 = CharacterTablesReductive.character_table rdG2G2
    val () = RootDatum.free rdG2G2
    val () = expect ("G2xG2 orthogonal", CharacterTables.check_orthogonality ctG2G2)
    val () = expect ("G2xG2 order", CharacterTables.order_W ctG2G2 = 144)
    val () = expect ("G2xG2 n_irreps", CharacterTables.n_irreps ctG2G2 = 36)
  in
    print "test_character_tables_reductive_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);
