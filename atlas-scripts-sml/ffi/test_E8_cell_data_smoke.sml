(* Smoke test for the E8 cell-number data ports. *)

use "atlas-scripts-sml/E8_big_block_cell_parameter_numbers.sml";
use "atlas-scripts-sml/E8_small_block_cell_parameter_numbers.sml";
use "atlas-scripts-sml/cells.E8.repsonly.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    val big = E8_big_block_cell_parameter_numbers.cells_big
    val small = E8_small_block_cell_parameter_numbers.cells_small
    val reps = Cells_E8_repsonly.cells

    (* The underlying `.at` files contain 104/31 cell lists; the extra lines in
       the text fixtures are `#` comments. *)
    val () = expect ("big cell count", length big, 104)
    val () = expect ("repsonly cell count", length reps, 104)
    val () = expect ("small cell count", length small, 31)

    val () =
      if hd big = [0] andalso hd reps = [0] andalso hd small = [0] then
        ()
      else
        raise Fail "first cell mismatch (expected [0])"
  in
    print "test_E8_cell_data_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);
