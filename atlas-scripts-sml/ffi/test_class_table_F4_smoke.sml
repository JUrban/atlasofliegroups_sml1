(* Smoke test for the F4 portion of `class_tables.sml` (Kondo order). *)

use "atlas-scripts-sml/class_tables.sml";
use "atlas-scripts-sml/RootDatum.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    val rd = RootDatum.newSimple (#"F", 4, false)
    val wct = ClassTables.class_table_F4_kondo rd

    val () = expect ("n_classes", #n_classes wct, 25)
    val () = expect ("reps length", length (#class_representatives wct), 25)
    val () = expect ("sizes length", length (#class_sizes wct), 25)
    val () = expect ("orders length", length (#class_orders wct), 25)
    val () = expect ("sum class sizes", List.foldl (op +) 0 (#class_sizes wct), 1152)

    fun checkRep i =
      let
        val w = List.nth (#class_representatives wct, i)
        val cls = (#class_of wct) w
      in
        expect ("class_of(rep " ^ Int.toString i ^ ")", cls, i)
      end
    val () = List.app checkRep (List.tabulate (25, fn i => i))

    val cp = #class_power wct
    val () = expect ("power(7,0)=id", cp (7, 0), 0)
    val () = expect ("power(7,1)=self", cp (7, 1), 7)

    val () = RootDatum.free rd
  in
    print "test_class_table_F4_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

