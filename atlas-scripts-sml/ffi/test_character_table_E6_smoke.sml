(* Smoke test for `character_table_E6.sml`. *)

use "atlas-scripts-sml/character_table_E6.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    open CharacterTable_E6
    val () = expect ("e6_table rows", length e6_table, 25)
    val () = List.app (fn row => expect ("e6_table width", length row, 25)) e6_table
    val () = expect ("orders length", length e6_orders_magma, 25)
    val () = expect ("sizes length", length e6_sizes_magma, 25)
    val () = expect ("profile cols", length e6_profile_cols, 25)
    val () = List.app (fn col => expect ("profile width", length col, 4)) e6_profile_cols
    val () = expect ("to_special length", length to_special_E6_table, 25)
    val () = if to_special_E6 0 = 0 andalso to_special_E6 24 = 21 then () else raise Fail "to_special sanity"
  in
    print "test_character_table_E6_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

