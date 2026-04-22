use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/FPP_fundamental_alcove.sml";

(*
  File: atlas-scripts-sml/VertexData.sml

  Purpose
  - Provide a searchable, deduplicated container for rational vertices used by
    the folded-FPP geometry code.

  Design
  - Vertices are stored normalized and sorted by a stable key `[den, nums...]`,
    enabling binary search lookup.
*)
structure VertexData = struct
  type ratvec = Lattice.ratvec

  (* Stable key for a rational vector: `[den, nums...]` after normalization. *)
  fun ratvecKey (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  (* Sort and unique a list of rational vectors under the key ordering. *)
  fun sort_u (us: ratvec list) : ratvec list =
    Basic.sort_u_by (ratvecKey, Sort.rlex_leq) us

  (* Vertex store: an array of sorted, normalized vertices. *)
  type t = {verts: ratvec array}

  (* Construct from an (unsorted) list; sorts and removes duplicates. *)
  fun fromList (us: ratvec list) : t =
    {verts = Array.fromList (sort_u us)}

  (* Convert back to a list in stored order. *)
  fun toList ({verts}: t) : ratvec list = Array.foldr (op ::) [] verts

  (* Number of stored vertices. *)
  fun size ({verts}: t) : int = Array.length verts

  (* Lookup a vertex by value; returns its index if present. *)
  fun lookup ({verts}: t, u: ratvec) : int option =
    let
      val keyU = ratvecKey u
      val n = Array.length verts
      fun keyAt i = ratvecKey (Array.sub (verts, i))
      val i = Basic.binary_search_first (fn j => Sort.rlex_leq (keyU, keyAt j), 0, n)
    in
      if i < n andalso Sort.rlex_leq (keyAt i, keyU) then SOME i else NONE
    end

  (* Lookup that raises if the vertex is absent. *)
  fun lookupExn (vd: t, u: ratvec) : int =
    case lookup (vd, u) of
      SOME i => i
    | NONE => raise Fail "VertexData.lookupExn: vertex not found"

  (* Compute the barycenter of a face specified by vertex indices. *)
  fun face_bary (vd: t, idxs: int list) : ratvec =
    let
      val verts = #verts vd
      fun fetch i = Array.sub (verts, i)
      val vs = List.map fetch idxs
    in
      FPP_fundamental_alcove.barycenter vs
    end
end
