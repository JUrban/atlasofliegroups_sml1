use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/MatrixAT.sml";

fun isDistinguished (rd: RootDatum.t, delta: IntMatrix.mat) : bool =
  let
    val simple = RootDatum.simpleRootsCols rd
    fun contains v = List.exists (fn w => w = v) simple
    fun ok alpha = contains (IntMatrix.matVecMul (delta, alpha))
  in
    List.all ok simple
  end

fun checkCount (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ " got " ^ Int.toString got)

fun checkTrue (name, b) = if b then () else raise Fail (name ^ ": expected true")

val rdA2 = RootDatum.newSimple (#"A", 2, false)
val autosA2 = RootDatum.diagramAutomorphismMatrices rdA2
val () = checkCount ("A2 automorphisms", length autosA2, 2)
val () = checkTrue ("A2 distinguished", List.all (fn m => isDistinguished (rdA2, m)) autosA2)
val () = checkTrue ("A2 has identity", List.exists (fn m => m = IntMatrix.identity (RootDatum.rank rdA2)) autosA2)
val () = RootDatum.free rdA2

val rdD4 = RootDatum.newSimple (#"D", 4, false)
val autosD4 = RootDatum.diagramAutomorphismMatrices rdD4
val () = checkCount ("D4 automorphisms", length autosD4, 6)
val () = checkTrue ("D4 distinguished", List.all (fn m => isDistinguished (rdD4, m)) autosD4)
val () = checkTrue ("D4 has identity", List.exists (fn m => m = IntMatrix.identity (RootDatum.rank rdD4)) autosD4)
val maxOrd = List.foldl Int.max 1 (List.map MatrixAT.order autosD4)
val () = checkCount ("D4 max order", maxOrd, 3)
val () = RootDatum.free rdD4

val () = TextIO.print "ok\n";

