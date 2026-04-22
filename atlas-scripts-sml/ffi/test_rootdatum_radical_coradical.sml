use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";

fun checkTrue (name, b) = if b then () else raise Fail (name ^ ": expected true")
fun checkEqInt (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ " got " ^ Int.toString got)

val rdA2 = RootDatum.newSimple (#"A", 2, false)
val () = checkEqInt ("A2 rank", RootDatum.rank rdA2, 2)
val () = checkEqInt ("A2 ssr", RootDatum.semisimpleRank rdA2, 2)
val () = checkTrue ("A2 coradical empty", null (RootDatum.coradicalBasisCols rdA2))
val () = checkTrue ("A2 radical empty", null (RootDatum.radicalBasisCols rdA2))
val () = RootDatum.free rdA2

(* A1 x T1 as a 2-dimensional torus with one A1 factor.
   Choose simple root [2,0] and simple coroot [1,0]. *)
val rdA1T1 =
  RootDatum.newFromSimpleMats
    ( [[2], [0]]
    , [[1], [0]]
    , false
    )
val () = checkEqInt ("A1T1 rank", RootDatum.rank rdA1T1, 2)
val () = checkEqInt ("A1T1 ssr", RootDatum.semisimpleRank rdA1T1, 1)

val corad = RootDatum.coradicalBasisCols rdA1T1
val rad = RootDatum.radicalBasisCols rdA1T1
val () = checkEqInt ("A1T1 corad rank", length corad, 1)
val () = checkEqInt ("A1T1 rad rank", length rad, 1)
val () = checkTrue ("A1T1 corad basis", List.hd corad = [0, 1])
val () = checkTrue ("A1T1 rad basis", List.hd rad = [0, 1])

val () = RootDatum.free rdA1T1

val () = TextIO.print "ok\n";

