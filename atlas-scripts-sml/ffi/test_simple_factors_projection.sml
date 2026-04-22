use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/simple_factors.sml";
use "atlas-scripts-sml/Lattice.sml";

fun checkTrue (name, b) = if b then () else raise Fail (name ^ ": expected true")

fun ratvecEq (u: Lattice.ratvec, v: Lattice.ratvec) : bool =
  let
    val u' = Lattice.ratvecNormalize u
    val v' = Lattice.ratvecNormalize v
  in
    #den u' = #den v' andalso #nums u' = #nums v'
  end

fun maskKeep (keep: int list, xs: int list) : int list =
  let
    fun ok i = List.exists (fn j => j = i) keep
    fun mapi f ys =
      let
        fun loop (_, [], acc) = List.rev acc
          | loop (i, z :: zs, acc) = loop (i + 1, zs, f (i, z) :: acc)
      in
        loop (0, ys, [])
      end
  in
    mapi (fn (i, x) => if ok i then x else 0) xs
  end

val lt = LieType.parse "A2D4A1"
val rd = RootDatum.fromLieType lt
val r = RootDatum.rank rd
val comps = SimpleFactors.diagram_components rd

val wt = {den = 1, nums = List.tabulate (r, fn i => i + 1)}
val cwt = {den = 1, nums = List.tabulate (r, fn i => (i + 1) * 3)}

fun testOne comp =
  let
    val i = List.hd comp
    val p = SimpleFactors.project_on_simple_factor_weight (rd, i, wt)
    val expected = {den = 1, nums = maskKeep (comp, #nums wt)}
    val () = checkTrue ("weight projection mask", ratvecEq (p, expected))
    val p2 = SimpleFactors.project_on_simple_factor_weight (rd, i, p)
    val () = checkTrue ("weight projection idempotent", ratvecEq (p2, p))

    val q = SimpleFactors.project_on_simple_factor_coweight (rd, i, cwt)
    val expectedQ = {den = 1, nums = maskKeep (comp, #nums cwt)}
    val () = checkTrue ("coweight projection mask", ratvecEq (q, expectedQ))
    val q2 = SimpleFactors.project_on_simple_factor_coweight (rd, i, q)
    val () = checkTrue ("coweight projection idempotent", ratvecEq (q2, q))
  in
    ()
  end

val () = List.app testOne comps
val () = RootDatum.free rd

val () = TextIO.print "ok\n";
