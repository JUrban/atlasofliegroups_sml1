use "atlas-scripts-sml/misc.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/W_orbit.sml";

(*
  File: atlas-scripts-sml/wrhosize.sml

  Purpose
  - SML translation of `atlas-scripts/wrhosize.at`.
  - Estimates the volume of `conv(W*rho)` (via lattice point counting in a
    bounding box in adjoint coordinates) and compares it to a target count.

  Atlas correspondence
  - The `.at` script converts the input root datum to its adjoint (coderived)
    form so that simple roots form a basis; then it enumerates dominant points
    in a box `[0..d*2rho]` and counts those also lying in the FPP-style region.

  Caveats
  - This is exponential in rank and polynomial in `d`; only practical for small
    ranks and small `d`.
*)

structure WRhoSize = struct
  type rootdatum = RootDatum.t
  type vec = int list

  fun vecAdd (a: vec, b: vec) : vec = ListPair.mapEq (op +) (a, b)
  fun vecSub (a: vec, b: vec) : vec = ListPair.mapEq (op -) (a, b)
  fun vecScale (k: int, a: vec) : vec = List.map (fn x => k * x) a

  fun sumVecs (xs: vec list) : vec =
    (case xs of
       [] => []
     | v0 :: rest => List.foldl vecAdd v0 rest)

  fun two_rho (rd: rootdatum) : vec =
    sumVecs (RootDatum.posRootsCols rd)

  fun countTrue (bs: bool list) : IntInf.int =
    List.foldl (fn (b, acc) => if b then acc + 1 else acc) 0 bs

  fun powIntInf (a: IntInf.int, n: int) : IntInf.int =
    if n < 0 then raise Fail "WRhoSize.powIntInf: negative exponent"
    else
      let
        fun loop (acc, base, k) =
          if k = 0 then acc
          else if k mod 2 = 1 then loop (acc * base, base * base, k div 2)
          else loop (acc, base * base, k div 2)
      in
        loop (1, a, n)
      end

  fun counts (rdGiven: rootdatum, d: int) : unit =
    let
      val rd = RootDatum.adjoint rdGiven
      val r = RootDatum.rank rd
      val twoRho = two_rho rd
      val twodRho = vecScale (d, twoRho)
      val size = vecAdd (twodRho, List.tabulate (r, fn _ => 1))

      val domBox =
        List.filter
          (fn b => WOrbit.is_dominant (rd, b))
          (Misc.box_heights size)

      val hullCount = IntInf.fromInt (length domBox)

      val fppCount =
        countTrue (List.map (fn b => WOrbit.is_dominant (rd, vecSub (twodRho, b))) domBox)

      val target = powIntInf (IntInf.fromInt (2 * d + 1), r)

      val () =
        TextIO.print
          ("hullCount = "
           ^ IntInf.toString hullCount
           ^ ", FPPCount = "
           ^ IntInf.toString fppCount
           ^ ", target = "
           ^ IntInf.toString target
           ^ "\n")

      val () = RootDatum.free rd
    in
      ()
    end
end

