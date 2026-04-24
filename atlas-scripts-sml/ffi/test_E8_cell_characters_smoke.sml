(* Smoke test for `E8_big_block_cell_characters`. *)

use "atlas-scripts-sml/E8_big_block_cell_characters.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    val chars = E8_big_block_cell_characters.cell_characters_big
    val () = expect ("row count", length chars, 104)
    val () = List.app (fn row => expect ("row width", length row, 112)) chars

    val row0 = hd chars
    val row1 = hd (tl chars)
    val () = if List.all (fn x => x = 1) row0 then () else raise Fail "row0 not all ones"
    val () =
      if List.take (row1, 10) = [8, ~8, ~6, 6, 0, ~4, 4, ~2, 2, 0] then
        ()
      else
        raise Fail "row1 prefix mismatch"
  in
    print "test_E8_cell_characters_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

