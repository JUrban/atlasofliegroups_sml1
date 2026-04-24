(* Smoke test for `character_table_F.sml` (data-only port). *)

use "atlas-scripts-sml/character_table_F.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun absInt x = if x < 0 then ~x else x

fun main () =
  let
    open CharacterTable_F
    val () = expect ("F4 irreps", length character_table_F4_data, 25)
    val () = List.app (fn (chi, _, _) => expect ("F4 row width", length chi, 25)) character_table_F4_data
    val () = expect ("to_special length", length to_special_F4_table, 25)

    val (triv, _, _) = List.nth (character_table_F4_data, 0)
    val () =
      if List.all (fn x => x = 1) triv then
        ()
      else
        raise Fail "F4 trivial character row not all 1s"

    val () =
      case List.nth (irreps_kondo, 0) of
        (_, name) => if String.isPrefix "phi(" name then () else raise Fail "F4 naming scheme unexpected"

    val (chi1, _, _) = List.nth (character_table_F4_data, 1)
    val () =
      if List.all (fn x => absInt x = 1) chi1 then
        ()
      else
        raise Fail "F4 sign-like row 1 not all +/-1"
  in
    print "test_character_table_F_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

