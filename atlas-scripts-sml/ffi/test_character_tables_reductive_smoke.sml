(* Smoke test for `character_tables_reductive.sml` (exceptional-only dispatcher). *)

use "atlas-scripts-sml/character_tables_reductive.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";

fun expect (name, cond) = if cond then () else raise Fail name

fun main () =
  let
    val ctF4 = CharacterTablesReductive.simple_character_table (LieType.parse "F4")
    val () = expect ("F4 orthogonal", CharacterTables.check_orthogonality ctF4)
    val () = expect ("F4 order", CharacterTables.order_W ctF4 = 1152)

    val ctG2 = CharacterTablesReductive.simple_character_table (LieType.parse "G2")
    val () = expect ("G2 orthogonal", CharacterTables.check_orthogonality ctG2)
    val () = expect ("G2 order", CharacterTables.order_W ctG2 = 12)

    val ctE6 = CharacterTablesReductive.simple_character_table (LieType.parse "E6")
    val () = expect ("E6 orthogonal", CharacterTables.check_orthogonality ctE6)
    val () = expect ("E6 order", CharacterTables.order_W ctE6 = 51840)

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
