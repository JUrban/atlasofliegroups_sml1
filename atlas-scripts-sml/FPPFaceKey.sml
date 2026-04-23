use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_vertices_fold.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/VertexData.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/FPP_fundamental_alcove.sml";

(*
  File: atlas-scripts-sml/FPPFaceKey.sml

  Purpose
  - Given a barycenter `gamma` of a folded-FPP face, compute a “face key”:
    the list of vertex indices whose barycenter is `gamma`.

  Why this exists
  - Some of the `.at` FPP code reasons about faces by working with their vertex
    index sets. For the SML port, we precompute a vertex table and provide an
    inverse mapping:
      gamma  ->  {vertex indices of the corresponding face}

  Design notes
  - Works only for small dimensions (F4 rank = 4), where face sizes are ≤ 5.
  - Uses meet-in-the-middle via precomputed pair sums for K=3..5 reconstruction.
*)
structure FPPFaceKey = struct
  type ratvec = Lattice.ratvec
  type key = int list
  type face_key = int list

  (* Stable key `[den, nums...]` after normalization. *)
  fun ratvecKey (u: ratvec) : key =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  (* Key equality under lex ordering. *)
  fun keyEq (a: key, b: key) : bool = Sort.rlex_leq (a, b) andalso Sort.rlex_leq (b, a)

  (* Key ordering. *)
  fun keyLeq (a: key, b: key) : bool = Sort.rlex_leq (a, b)

  (* A face-key helper for a fixed group. *)
  type t =
    { verts: ratvec array
    , vd: VertexData.t
    , gammaDim: key -> int option
    , pairSums: (key * (int * int)) array (* sorted by key *)
    }

  (* Build a lookup `gammaKey -> face dimension` for the folded-FPP barycenters. *)
  fun mkGammaDimLookup (g: AtlasFFI.group) : key -> int option =
    let
      val pairs =
        List.concat
          (List.tabulate
             (5, fn d => List.map (fn gam => (ratvecKey gam, d)) (FPP_barycenters_fold.barycenters_dim (g, d))))

      val sorted = Basic.sort_by (fn (k, _) => k, keyLeq) pairs
      val arr = Array.fromList sorted
      val n = Array.length arr
      fun keyAt i = #1 (Array.sub (arr, i))
      fun dimAt i = #2 (Array.sub (arr, i))

      fun lookup (k: key) : int option =
        let
          val i = Basic.binary_search_first (fn j => keyLeq (k, keyAt j), 0, n)
        in
          if i < n andalso keyEq (keyAt i, k) then SOME (dimAt i) else NONE
        end
    in
      lookup
    end

  (* Precompute all pair sums of vertices:
       key(v_i + v_j) -> (i,j)
     Stored sorted by key to allow efficient lookup. *)
  fun buildPairSums (verts: ratvec array) : (key * (int * int)) array =
    let
      val n = Array.length verts
      fun at i = Array.sub (verts, i)
      fun pairsForI i =
        List.tabulate (n - i - 1, fn t => (i, i + 1 + t))
      val pairs = List.concat (List.tabulate (n, pairsForI))

      fun sumPair (i, j) =
        let
          val s = FPP_fundamental_alcove.ratvecAdd (at i, at j)
        in
          (ratvecKey s, (i, j))
        end

      val entries = List.map sumPair pairs
      val sorted = Basic.sort_by (fn (k, _) => k, keyLeq) entries
    in
      Array.fromList sorted
    end

  (* Construct the face-key context for `g`, including vertices and pair sums. *)
  fun create (g: AtlasFFI.group) : t =
    let
      val vertsList = FPP_vertices_fold.vertices g
      val vd = VertexData.fromList vertsList
      val verts = #verts vd
      val gammaDim = mkGammaDimLookup g
      val pairSums = buildPairSums verts
    in
      {verts = verts, vd = vd, gammaDim = gammaDim, pairSums = pairSums}
    end

  (*
    Fast-path constructor for callers that only need the global folded-FPP
    *vertex table* (and derived `VertexData`) but do not need to reconstruct
    face keys from barycenters.

    In particular, `FPP_localDirac.create_ctx` uses this in the `F4_s` case
    when a precomputed `gamma -> face_key` table is available on disk.

    Limitations
    - `gammaDim` always returns `NONE`, so `faceKeyOfGamma` will return `NONE`
      for every input.
    - `pairSums` is empty.
  *)
  fun createVerticesOnly (g: AtlasFFI.group) : t =
    let
      val vertsList = FPP_vertices_fold.vertices g
      val vd = VertexData.fromList vertsList
      val verts = #verts vd
      val gammaDim = (fn (_: key) => NONE)
      val pairSums : (key * (int * int)) array = Array.fromList []
    in
      {verts = verts, vd = vd, gammaDim = gammaDim, pairSums = pairSums}
    end

  (* Lookup all vertex index pairs `(i,j)` with `v_i + v_j` having key `k`. *)
  fun lookupPairSums ({pairSums, ...}: t, k: key) : (int * int) list =
    let
      val arr = pairSums
      val n = Array.length arr
      fun keyAt i = #1 (Array.sub (arr, i))
      fun pairAt i = #2 (Array.sub (arr, i))
      val i0 = Basic.binary_search_first (fn j => keyLeq (k, keyAt j), 0, n)
      fun loop i acc =
        if i < n andalso keyEq (keyAt i, k) then
          loop (i + 1) (pairAt i :: acc)
        else
          List.rev acc
    in
      if i0 < n andalso keyEq (keyAt i0, k) then loop i0 [] else []
    end

  (* Decide whether all indices are pairwise distinct. *)
  fun isDistinct (xs: int list) : bool =
    let
      fun loop [] = true
        | loop [_] = true
        | loop (a :: b :: rest) = a <> b andalso loop (b :: rest)
    in
      loop (Basic.sort (op <=) xs)
    end

  (* Solve `v_i + v_j = target` with `i<j`. *)
  fun solveK2 (ctx as {verts, vd, ...}: t, target: ratvec) : (int * int) option =
    let
      val n = Array.length verts
      fun at i = Array.sub (verts, i)
      fun loop i =
        if i >= n then
          NONE
        else
          let
            val w = Lattice.ratvecSub (target, at i)
          in
            case VertexData.lookup (vd, w) of
              NONE => loop (i + 1)
            | SOME j => if i < j then SOME (i, j) else loop (i + 1)
          end
    in
      loop 0
    end

  (* Solve `v_i + v_j + v_k = target`, returning sorted indices. *)
  fun solveK3 (ctx as {verts, ...}: t, target: ratvec) : face_key option =
    let
      val n = Array.length verts
      fun at i = Array.sub (verts, i)

      fun loop i =
        if i >= n then
          NONE
        else
          let
            val rem = Lattice.ratvecSub (target, at i)
            val pairs = lookupPairSums (ctx, ratvecKey rem)
            fun pick [] = loop (i + 1)
              | pick ((j, k) :: rest) =
                  if i <> j andalso i <> k then
                    SOME (Basic.sort (op <=) [i, j, k])
                  else
                    pick rest
          in
            pick pairs
          end
    in
      loop 0
    end

  (* Solve `v_i + v_j + v_k + v_l = target`, returning sorted indices. *)
  fun solveK4 (ctx as {verts, ...}: t, target: ratvec) : face_key option =
    let
      val nPairs = Array.length (#pairSums ctx)
      fun pairEntry i = Array.sub (#pairSums ctx, i)
      fun at i = Array.sub (verts, i)

      fun loop p =
        if p >= nPairs then
          NONE
        else
          let
            val (_, (i, j)) = pairEntry p
            val sumIJ = FPP_fundamental_alcove.ratvecAdd (at i, at j)
            val rem = Lattice.ratvecSub (target, sumIJ)
            val pairs = lookupPairSums (ctx, ratvecKey rem)
            fun pick [] = loop (p + 1)
              | pick ((k, l) :: rest) =
                  let
                    val idxs = Basic.sort (op <=) [i, j, k, l]
                  in
                    if isDistinct idxs then SOME idxs else pick rest
                  end
          in
            pick pairs
          end
    in
      loop 0
    end

  (* Solve `v_i + v_j + v_k + v_l + v_m = target`, returning sorted indices. *)
  fun solveK5 (ctx as {verts, ...}: t, target: ratvec) : face_key option =
    let
      val n = Array.length verts
      fun at i = Array.sub (verts, i)
      fun loop i =
        if i >= n then
          NONE
        else
          let
            val rem = Lattice.ratvecSub (target, at i)
          in
            case solveK4 (ctx, rem) of
              NONE => loop (i + 1)
            | SOME idxs4 =>
                let
                  val idxs = Basic.sort (op <=) (i :: idxs4)
                in
                  if isDistinct idxs then SOME idxs else loop (i + 1)
                end
          end
    in
      loop 0
    end

  (* Given a barycenter `gamma`, return the corresponding vertex index list.
     Returns `NONE` if `gamma` is not recognized as a folded-FPP barycenter. *)
  fun faceKeyOfGamma (ctx as {gammaDim, ...}: t, gamma: ratvec) : face_key option =
    case gammaDim (ratvecKey gamma) of
      NONE => NONE
    | SOME d =>
        let
          val k = d + 1
          val target = Lattice.ratvecScale (gamma, k, 1)
        in
          case k of
            1 =>
              (case VertexData.lookup (#vd ctx, gamma) of
                 NONE => NONE
               | SOME i => SOME [i])
          | 2 =>
              (case solveK2 (ctx, target) of
                 NONE => NONE
               | SOME (i, j) => SOME [i, j])
          | 3 => solveK3 (ctx, target)
          | 4 => solveK4 (ctx, target)
          | 5 => solveK5 (ctx, target)
          | _ => raise Fail "FPPFaceKey.faceKeyOfGamma: unexpected k"
        end
end
