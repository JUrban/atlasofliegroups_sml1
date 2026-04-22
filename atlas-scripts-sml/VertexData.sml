use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/FPP_fundamental_alcove.sml";

structure VertexData = struct
  type ratvec = Lattice.ratvec

  fun ratvecKey (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  fun sort_u (us: ratvec list) : ratvec list =
    Basic.sort_u_by (ratvecKey, Sort.rlex_leq) us

  type t = {verts: ratvec array}

  fun fromList (us: ratvec list) : t =
    {verts = Array.fromList (sort_u us)}

  fun toList ({verts}: t) : ratvec list = Array.foldr (op ::) [] verts

  fun size ({verts}: t) : int = Array.length verts

  fun lookup ({verts}: t, u: ratvec) : int option =
    let
      val keyU = ratvecKey u
      val n = Array.length verts
      fun keyAt i = ratvecKey (Array.sub (verts, i))
      val i = Basic.binary_search_first (fn j => Sort.rlex_leq (keyU, keyAt j), 0, n)
    in
      if i < n andalso Sort.rlex_leq (keyAt i, keyU) then SOME i else NONE
    end

  fun lookupExn (vd: t, u: ratvec) : int =
    case lookup (vd, u) of
      SOME i => i
    | NONE => raise Fail "VertexData.lookupExn: vertex not found"

  fun face_bary (vd: t, idxs: int list) : ratvec =
    let
      val verts = #verts vd
      fun fetch i = Array.sub (verts, i)
      val vs = List.map fetch idxs
    in
      FPP_fundamental_alcove.barycenter vs
    end
end

