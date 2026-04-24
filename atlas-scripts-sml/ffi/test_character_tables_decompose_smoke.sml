(* Smoke test for `character_tables.sml`: decompose(chi_i) = e_i. *)

use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/character_table_G.sml";
use "atlas-scripts-sml/character_table_F.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/RootDatum.sml";

fun expect (name, cond) = if cond then () else raise Fail name

fun isStandardBasisVector (v: int list, i: int) : bool =
  List.all (fn (x, j) => if j = i then x = 1 else x = 0) (ListPair.zip (v, List.tabulate (length v, fn k => k)))

fun testG2 () =
  let
    val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
    val ct = CharacterTable_G.character_table_G2 g
    val n = CharacterTables.n_irreps ct
    fun check i =
      let
        val chi = CharacterTables.character (ct, i)
        val mults = CharacterTables.decompose (ct, chi)
      in
        expect ("G2 decompose basis " ^ Int.toString i, isStandardBasisVector (mults, i))
      end
    val () = List.app check (List.tabulate (n, fn i => i))
    val () = AtlasFFI.atlas_group_free g
  in
    ()
  end

fun testF4 () =
  let
    val rd = RootDatum.newSimple (#"F", 4, false)
    val ct = CharacterTable_F.character_table_F4 rd
    val n = CharacterTables.n_irreps ct
    fun check i =
      let
        val chi = CharacterTables.character (ct, i)
        val mults = CharacterTables.decompose (ct, chi)
      in
        expect ("F4 decompose basis " ^ Int.toString i, isStandardBasisVector (mults, i))
      end
    val () = List.app check (List.tabulate (n, fn i => i))
    val () = RootDatum.free rd
  in
    ()
  end

fun main () =
  (testG2 (); testF4 (); print "test_character_tables_decompose_smoke: ok\n")

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

