use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/basic.sml";

(* Fundamental-alcove combinatorics from `atlas-scripts/FPP_faces_geom.at`. *)
structure FPP_fundamental_alcove = struct
  type rootdatum = RootDatum.t
  type ratvec = Lattice.ratvec
  type face_verts = ratvec list

  fun gcdIntInf (x: IntInf.int, y: IntInf.int) : IntInf.int =
    let
      val x = IntInf.abs x
      val y = IntInf.abs y
      fun loop (u, 0) = u
        | loop (u, v) = loop (v, IntInf.mod (u, v))
    in
      if x = 0 then y else loop (x, y)
    end

  fun lcmIntInf (x: IntInf.int, y: IntInf.int) : IntInf.int =
    if x = 0 orelse y = 0 then 0 else IntInf.div (IntInf.abs (x * y), gcdIntInf (x, y))

  fun ratvecZero (n: int) : ratvec = {den = 1, nums = List.tabulate (n, fn _ => 0)}

  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
      val v = Lattice.ratvecNormalize v
      val du = IntInf.fromInt (#den u)
      val dv = IntInf.fromInt (#den v)
      val numsU = List.map IntInf.fromInt (#nums u)
      val numsV = List.map IntInf.fromInt (#nums v)
      val () = if length numsU = length numsV then () else raise Fail "ratvecAdd: length mismatch"
      val d = lcmIntInf (du, dv)
      val () = if d <> 0 then () else raise Fail "ratvecAdd: zero lcm"
      val mu = IntInf.div (d, du)
      val mv = IntInf.div (d, dv)
      fun addOne (a, b) = a * mu + b * mv
      val nums = ListPair.mapEq addOne (numsU, numsV)
      val den = IntInf.toInt d handle _ => raise Fail "ratvecAdd: denom overflow"
      val numsI = List.map (fn z => IntInf.toInt z handle _ => raise Fail "ratvecAdd: numerator overflow") nums
    in
      Lattice.ratvecNormalize {den = den, nums = numsI}
    end

  fun ratvecSum (n: int, us: ratvec list) : ratvec =
    List.foldl ratvecAdd (ratvecZero n) us

  (* `labels(SimpleAffine affd)` from `FPP_faces_geom.at`. *)
  fun labels (rd: rootdatum) : int list =
    let
      val d = RootDatum.dual rd
      val highestCoroot = RootDatum.highestRoot d
      val () = RootDatum.free d
      val corootsMat = RootDatum.simpleCorootsMat rd
    in
      case Lattice.solve (corootsMat, highestCoroot) of
        NONE => raise Fail "FPP_fundamental_alcove.labels: no coroot expression"
      | SOME coeffs => 1 :: coeffs
    end

  (* `fundamental_vertices(SimpleAffine affd)` from `FPP_faces_geom.at`. *)
  fun fundamental_vertices (rd: rootdatum) : ratvec list =
    let
      val r = RootDatum.rank rd
      val labs = List.tl (labels rd)
      val fws = RootDatum.fundamentalWeights rd
      val () = if length labs = length fws then () else raise Fail "fundamental_vertices: label/weight mismatch"
      val verts = ListPair.mapEq (fn (w, lab) => Lattice.ratvecScale (w, 1, lab)) (fws, labs)
    in
      ratvecZero r :: verts
    end

  (* `faces_fundamental(SimpleAffine affd, int d)` from `FPP_faces_geom.at`. *)
  fun faces_fundamental (rd: rootdatum, d: int) : face_verts list =
    Basic.choices_from (fundamental_vertices rd, d + 1)

  (* `barycenter([ratvec] verts)` from `FPP_faces_geom.at`. *)
  fun barycenter (verts: ratvec list) : ratvec =
    (case verts of
       [] => raise Fail "barycenter: empty"
     | v0 :: _ =>
         let
           val n = length (#nums (Lattice.ratvecNormalize v0))
           val sum = ratvecSum (n, verts)
         in
           Lattice.ratvecScale (sum, 1, length verts)
         end)

  fun fund_barycenters (rd: rootdatum, d: int) : ratvec list =
    List.map barycenter (faces_fundamental (rd, d))
end

