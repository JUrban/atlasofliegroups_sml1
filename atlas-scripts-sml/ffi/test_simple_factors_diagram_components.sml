use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/simple_factors.sml";

fun checkTrue (name, b) = if b then () else raise Fail (name ^ ": expected true")
fun checkEqInt (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ " got " ^ Int.toString got)

fun checkPartition (n: int, blocks: int list list) : unit =
  let
    val seen = Array.array (n, 0)
    fun add i =
      if i < 0 orelse i >= n then
        raise Fail "partition: index out of range"
      else
        Array.update (seen, i, Array.sub (seen, i) + 1)
    val () = List.app (fn b => List.app add b) blocks
    val () = checkTrue ("partition: all seen once", List.all (fn i => Array.sub (seen, i) = 1) (List.tabulate (n, fn i => i)))
  in
    ()
  end

val lt = LieType.parse "A2D4A1"
val rd = RootDatum.fromLieType lt
val ssr = RootDatum.semisimpleRank rd
val comps = SimpleFactors.diagram_components rd

val () = checkEqInt ("ssr", ssr, 7)
val () = checkEqInt ("num components", length comps, 3)
val () = checkPartition (ssr, comps)

val sizes = List.map length comps
val sizesSorted = Basic.sort (op <=) sizes
val () = checkTrue ("component sizes", sizesSorted = [1, 2, 4])

val (rd2, pi) = SimpleFactors.reorder_diagram_Bourbaki rd
val () = checkEqInt ("pi length", length pi, ssr)
val comps2 = SimpleFactors.diagram_components rd2
val () = checkEqInt ("num components 2", length comps2, 3)
val () = checkPartition (RootDatum.semisimpleRank rd2, comps2)

val () = RootDatum.free rd
val () = RootDatum.free rd2

val () = TextIO.print "ok\n";

