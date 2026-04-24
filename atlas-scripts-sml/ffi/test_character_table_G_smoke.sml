(* Smoke test for `character_table_G.sml` (data-only port). *)

use "atlas-scripts-sml/character_table_G.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun absInt x = if x < 0 then ~x else x

fun main () =
  let
    open CharacterTable_G
    val () = expect ("G2 class_names", length class_names, 6)
    val () = expect ("G2 irreps", length irreps, 6)
    val () = List.app (fn (chi, _) => expect ("G2 row width", length chi, 6)) irreps
    val () = expect ("to_special length", length to_special_G2_table, 6)

    val (triv, _) = List.nth (irreps, 0)
    val () =
      if List.all (fn x => x = 1) triv then
        ()
      else
        raise Fail "G2 trivial character row not all 1s"

    val (signRow, _) = List.nth (irreps, 3)
    val () =
      if List.all (fn x => absInt x = 1) signRow then
        ()
      else
        raise Fail "G2 full sign row not all +/-1"
  in
    print "test_character_table_G_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

