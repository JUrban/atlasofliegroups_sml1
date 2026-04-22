use "atlas-scripts-sml/cofolded.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/MatrixAT.sml";

(*
  File: atlas-scripts-sml/FPP_faces_geom_fold.sml

  Purpose
  - Small folded-FPP geometry primitives extracted from `atlas-scripts/FPP_faces_geom.at`.
  - Provides:
      - the edge directions of the (folded) fundamental parallelepiped (FPP)
      - the corresponding simple coroot directions in ambient coordinates
*)
structure FPP_faces_geom_fold = struct
  type vec = int list
  type mat = IntMatrix.mat
  type ratvec = Lattice.ratvec

  (* Multiply a row vector by a matrix (implemented via transpose). *)
  fun vecMatMul (v: vec, m: mat) : vec =
    IntMatrix.matVecMul (IntMatrix.transpose m, v)

  (* Replace element at index `idx` in a list. *)
  fun replaceAt (xs: 'a list, idx: int, v: 'a) : 'a list =
    if idx < 0 orelse idx >= length xs then
      raise Subscript
    else
      List.take (xs, idx) @ (v :: List.drop (xs, idx + 1))

  (* Edges of the fundamental parallelepiped (in the ambient weight lattice). *)
  (* Folded-FPP edge directions as rational vectors in ambient coordinates. *)
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
  (* Folded simple coroots pulled back to ambient integer vectors. *)
  fun FPP_coroots (g: AtlasFFI.group) : vec list =
    let
      val (affd, m, j0) = Cofolded.cofolded g
      val minv = MatrixAT.left_inverse m
      val avs0 = List.map (fn avAff => vecMatMul (avAff, minv)) (RootDatum.simpleCorootsCols affd)

      (* Divide a vector by 2 if all entries are even. *)
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
