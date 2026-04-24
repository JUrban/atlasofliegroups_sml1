(* Smoke test for `e8_gap.sml`. *)

use "atlas-scripts-sml/e8_gap.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    open E8_gap
    val () = expect ("table rows", length e8_gap_table, 112)
    val () = List.app (fn row => expect ("table width", length row, 112)) e8_gap_table
    val () = expect ("orders", length e8_gap_orders, 112)
    val () = expect ("centralizers", length e8_gap_centralizer_sizes, 112)
    val () = expect ("classes", length e8_gap_classes, 112)
    val () = expect ("profile cols", length e8_gap_profile_cols, 112)
    val () = List.app (fn col => expect ("profile width", length col, 5)) e8_gap_profile_cols

    val () =
      if hd e8_gap_classes = "1a" andalso List.nth (e8_gap_orders, 0) = 1 then
        ()
      else
        raise Fail "first class label/order mismatch"

    val row0 = hd e8_gap_table
    val () = if List.all (fn x => x = 1) row0 then () else raise Fail "row0 not all ones"
  in
    print "test_e8_gap_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

