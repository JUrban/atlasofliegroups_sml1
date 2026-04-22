use "atlas-scripts-sml/cofolded.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/MatrixAT.sml";

(* Small folded-FPP geometry primitives from `atlas-scripts/FPP_faces_geom.at`. *)
structure FPP_faces_geom_fold = struct
  type vec = int list
  type mat = IntMatrix.mat
  type ratvec = Lattice.ratvec

  fun vecMatMul (v: vec, m: mat) : vec =
    IntMatrix.matVecMul (IntMatrix.transpose m, v)

  fun replaceAt (xs: 'a list, idx: int, v: 'a) : 'a list =
    if idx < 0 orelse idx >= length xs then
      raise Subscript
    else
      List.take (xs, idx) @ (v :: List.drop (xs, idx + 1))

  (* Edges of the fundamental parallelepiped (in the ambient weight lattice). *)
  fun FPP_lines (g: AtlasFFI.group) : ratvec list =
    let
      val (affd, m, j0) = Cofolded.cofolded g
      val ws = RootDatum.fundamentalWeights affd
      val vs = List.map (fn w => Lattice.matVecMulRatvec m w) ws
      val vs' =
        if j0 >= 0 then
          replaceAt (vs, j0, Lattice.ratvecScale (List.nth (vs, j0), 2, 1))
        else
          vs
      val () = RootDatum.free affd
    in
      vs'
    end

  (* Simple coroots of the cofolded datum, expressed back in the ambient lattice. *)
  fun FPP_coroots (g: AtlasFFI.group) : vec list =
    let
      val (affd, m, j0) = Cofolded.cofolded g
      val minv = MatrixAT.left_inverse m
      val avs0 = List.map (fn avAff => vecMatMul (avAff, minv)) (RootDatum.simpleCorootsCols affd)

      fun halfVec v =
        if List.all (fn x => x mod 2 = 0) v then List.map (fn x => x div 2) v
        else raise Fail "FPP_coroots: expected divisible by 2"

      val avs =
        if j0 >= 0 then
          replaceAt (avs0, j0, halfVec (List.nth (avs0, j0)))
        else
          avs0

      val () = RootDatum.free affd
    in
      avs
    end
end

