use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun checkTrue (name, b) = if b then () else raise Fail (name ^ ": expected true")
fun checkEqInt (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ " got " ^ Int.toString got)

val rd = RootDatum.newSimple (#"A", 2, false)
val npr = RootDatum.numPosRoots rd
val () = checkTrue ("A2 npr>0", npr > 0)

val idxs = List.tabulate (2 * npr, fn k => k - npr)

fun checkIdx i =
  let
    val v = RootDatum.rootByIndex (rd, i)
    val j = RootDatum.rootIndex (rd, v)
    val () = checkEqInt ("rootIndex(rootByIndex)", j, i)

    val c = RootDatum.corootByIndex (rd, i)
    val k = RootDatum.corootIndex (rd, c)
    val () = checkEqInt ("corootIndex(corootByIndex)", k, i)
  in
    ()
  end

val () = List.app checkIdx idxs

val bogus = [999, 999]
val () = checkEqInt ("rootIndex(bogus)", RootDatum.rootIndex (rd, bogus), npr)
val () = checkEqInt ("corootIndex(bogus)", RootDatum.corootIndex (rd, bogus), npr)

val rd2 = RootDatum.subDatumByRootIndices (rd, [0, 1])
val () = checkTrue ("subDatum simples Cartan", RootDatum.cartanMatrix rd2 = RootDatum.cartanMatrix rd)
val () = RootDatum.free rd2

val rd3 = RootDatum.subDatumByRootIndices (rd, [~1, ~2])
val () = checkTrue ("subDatum neg simples Cartan", RootDatum.cartanMatrix rd3 = RootDatum.cartanMatrix rd)
val () = RootDatum.free rd3

val () = RootDatum.free rd

val () = TextIO.print "ok\n";

