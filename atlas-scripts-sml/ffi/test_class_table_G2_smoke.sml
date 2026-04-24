(* Smoke test for the G2 portion of `class_tables.sml`. *)

use "atlas-scripts-sml/class_tables.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
    val wct = ClassTables.class_table_G g

    val () = expect ("n_classes", #n_classes wct, 6)
    val () = expect ("reps length", length (#class_representatives wct), 6)
    val () = expect ("sizes length", length (#class_sizes wct), 6)
    val () = expect ("orders length", length (#class_orders wct), 6)

    fun checkRep i =
      let
        val w = List.nth (#class_representatives wct, i)
        val cls = (#class_of wct) w
      in
        expect ("class_of(rep " ^ Int.toString i ^ ")", cls, i)
      end

    val () = List.app checkRep [0, 1, 2, 3, 4, 5]

    (* sanity for power map on order-6 class *)
    val cp = #class_power wct
    val () = expect ("power(5,0)", cp (5, 0), 0)
    val () = expect ("power(5,1)", cp (5, 1), 5)
    val () = expect ("power(5,2)", cp (5, 2), 4)
    val () = expect ("power(5,3)", cp (5, 3), 3)
    val () = expect ("power(5,4)", cp (5, 4), 4)
    val () = expect ("power(5,5)", cp (5, 5), 5)

    val () = AtlasFFI.atlas_group_free g
  in
    print "test_class_table_G2_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

