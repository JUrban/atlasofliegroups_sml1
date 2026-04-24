(* Smoke test for `character_table_E7_data.sml`. *)

use "atlas-scripts-sml/character_table_E7_data.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    open CharacterTable_E7_Data
    val () = expect ("class_words_E7 length", length class_words_E7, 60)
    val () = expect ("class_orders_E7 length", length class_orders_E7, 60)
    val () = expect ("class_sizes_E7 length", length class_sizes_E7, 60)
    val () = expect ("class_centralizer_orders_E7 length", length class_centralizer_orders_E7, 60)
    val () = expect ("class_powers_E7 rows", length class_powers_E7, 60)
    val () = List.app (fn row => expect ("class_powers_E7 row width", length row, 60)) class_powers_E7

    val () =
      if List.take (class_orders_E7, 5) = [1, 2, 2, 3, 6] then
        ()
      else
        raise Fail "class_orders_E7 prefix mismatch"

    val () =
      if hd class_sizes_E7 = 1 andalso List.last class_sizes_E7 = 1 then
        ()
      else
        raise Fail "class_sizes_E7 endpoints mismatch"
  in
    print "test_character_table_E7_data_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

