(* Smoke test for `character_table_E8.sml` (data-only port). *)

use "atlas-scripts-sml/character_table_E8.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun absInt x = if x < 0 then ~x else x

fun main () =
  let
    open CharacterTable_E8
    val () = expect ("E8 table rows", length e8_table, 112)
    val () = List.app (fn row => expect ("E8 table width", length row, 112)) e8_table
    val () = expect ("E8 profile cols", length e8_profile_cols, 112)
    val () = List.app (fn col => expect ("E8 profile width", length col, 5)) e8_profile_cols
    val () = expect ("to_special length", length to_special_E8_table, 112)

    val triv = List.nth (e8_table, 0)
    val () =
      if List.all (fn x => x = 1) triv then
        ()
      else
        raise Fail "E8 trivial character row not all 1s"

    val sign = List.nth (e8_table, 1)
    val () =
      if List.all (fn x => absInt x = 1) sign then
        ()
      else
        raise Fail "E8 sign character row not all +/-1"

    val () = expect ("to_special(0)", to_special_E8 0, 0)
    val () = expect ("to_special(1)", to_special_E8 1, 1)
  in
    print "test_character_table_E8_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);
