use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/hash.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/KTypePolHash.sml";

(*
  File: atlas-scripts-sml/FaceClasses.sml

  Purpose
  - Foundational graph/class utilities inspired by `atlas-scripts/face_classes.at`.
  - The `.at` code builds “face graphs” and computes equivalence classes of
    faces via strong connected components (SCCs), producing a `GraphData` value:
      - `classes`: SCCs as lists of node indices
      - `covers`: edges between SCCs in the condensed DAG

  Current scope
  - This file provides only the core graph machinery needed by future ports:
      - `strong_components`: Kosaraju SCC on a directed graph
      - `class_of`: map node -> class id
      - `class_list_linear`: linear class map as an `int list`
      - `full_down_classes`: compute downward-closure class lists in the DAG

  Notes
  - Unlike the `.at` implementation, we do not attempt to compute a transitive
    reduction (“covers” in the strict poset sense). `covers` here is the raw
    condensed adjacency (with duplicates removed), which is sufficient for
    closure-style consumers.
*)

structure FaceClasses = struct
  type graph = int list array

  type graph_data = {classes: int list list, covers: int list list}

  fun graph_of_lists (adj: int list list) : graph =
    Array.fromList adj

  fun size (g: graph) : int = Array.length g

  fun transpose (g: graph) : graph =
    let
      val n = size g
      val rev = Array.array (n, ([]: int list))
      fun addEdge (u, v) =
        if v < 0 orelse v >= n then
          raise Fail "FaceClasses.transpose: edge out of bounds"
        else
          Array.update (rev, v, u :: Array.sub (rev, v))
      fun loopU u =
        if u = n then
          ()
        else
          (List.app (fn v => addEdge (u, v)) (Array.sub (g, u)); loopU (u + 1))
      val () = loopU 0
    in
      Array.tabulate (n, fn i => Array.sub (rev, i))
    end

  (*
    Kosaraju SCC

    Returns:
    - `compOf[u]`: component id for node u (0..k-1)
    - `comps`: list of components in the order they were discovered
  *)
  fun kosaraju (g: graph) : int array * int list list =
    let
      val n = size g
      val seen = Array.array (n, false)
      val order : int list ref = ref []

      fun dfs1 u =
        if Array.sub (seen, u) then
          ()
        else
          (Array.update (seen, u, true);
           List.app dfs1 (Array.sub (g, u));
           order := u :: !order)

      val () = List.app dfs1 (List.tabulate (n, fn i => i))

      val gt = transpose g
      val compOf = Array.array (n, ~1)
      val comps : int list list ref = ref []

      fun dfs2 (u, acc: int list) : int list =
        if Array.sub (compOf, u) <> ~1 then
          acc
        else
          let
            val cid = length (!comps)
            val () = Array.update (compOf, u, cid)
            val acc = u :: acc
            val acc = List.foldl (fn (v, a) => dfs2 (v, a)) acc (Array.sub (gt, u))
          in
            acc
          end

      fun pass [] = ()
        | pass (u :: rest) =
            if Array.sub (compOf, u) <> ~1 then
              pass rest
            else
              let
                val nodes = dfs2 (u, [])
                val nodes = Sort.sort_u (op <=) nodes
              in
                comps := nodes :: !comps;
                pass rest
              end

      val () = pass (!order)
    in
      (compOf, List.rev (!comps))
    end

  fun strong_components (adj: int list list) : graph_data =
    let
      val g = graph_of_lists adj
      val n = size g
      val (compOf, comps) = kosaraju g
      val k = length comps
      val succ = Array.array (k, ([]: int list))

      fun addSucc (a, b) =
        if a = b then
          ()
        else
          Array.update (succ, a, b :: Array.sub (succ, a))

      fun loopU u =
        if u = n then
          ()
        else
          let
            val cu = Array.sub (compOf, u)
            val () = if cu < 0 then raise Fail "FaceClasses.strong_components: missing component" else ()
            val () =
              List.app
                (fn v =>
                   let val cv = Array.sub (compOf, v) in if cv < 0 then () else addSucc (cu, cv) end)
                (Array.sub (g, u))
          in
            loopU (u + 1)
          end

      val () = loopU 0

      fun uniq xs = Sort.sort_u (op <=) xs
      val covers = List.tabulate (k, fn i => uniq (Array.sub (succ, i)))
    in
      {classes = comps, covers = covers}
    end

  fun class_of ({classes, ...}: graph_data) : int array =
    let
      val n = List.foldl (fn (c, acc) => acc + length c) 0 classes
      val a = Array.array (n, ~1)
      fun oneClass (cid, nodes) =
        List.app (fn u => Array.update (a, u, cid)) nodes
      val () = List.app oneClass (ListPair.zipEq (List.tabulate (length classes, fn i => i), classes))
    in
      a
    end

  (* `class_list_linear` from `face_classes.at`: vector mapping node -> class id. *)
  fun class_list_linear (gd: graph_data, n: int) : int list =
    let
      val compOf = class_of gd
      val () = if Array.length compOf = n then () else raise Fail "FaceClasses.class_list_linear: size mismatch"
    in
      List.tabulate (n, fn i => Array.sub (compOf, i))
    end

  (*
    Downward closure of classes in the condensed DAG.

    Returns an array `down[i]` listing all classes `j` such that `j <= i` in the
    reachability preorder (including `i` itself). This matches the semantic use
    of `full_down_classes` in `face_classes.at`, but is computed by BFS rather
    than polynomial tricks.
  *)
  fun full_down_classes ({covers, ...}: graph_data) : int list array =
    let
      val k = length covers
      val pred = Array.array (k, ([]: int list))
      fun addPred (i, j) = Array.update (pred, j, i :: Array.sub (pred, j))
      fun loop i =
        if i = k then
          ()
        else
          (List.app (fn j => addPred (i, j)) (List.nth (covers, i)); loop (i + 1))
      val () = loop 0

      fun closureFrom (start: int) : int list =
        let
          val seen = Array.array (k, false)
          fun push i = if Array.sub (seen, i) then () else Array.update (seen, i, true)
          val () = push start
          fun bfs [] = ()
            | bfs (x :: xs) =
                let
                  val nbrs = Array.sub (pred, x)
                  val new =
                    List.filter
                      (fn y =>
                        if Array.sub (seen, y) then false else (Array.update (seen, y, true); true))
                      nbrs
                in
                  bfs (xs @ new)
                end
          val () = bfs [start]
          val all = List.tabulate (k, fn i => i)
        in
          Sort.sort_u (op <=) (List.filter (fn i => Array.sub (seen, i)) all)
        end
    in
      Array.tabulate (k, closureFrom)
    end

  (*
    Upward closure of classes in the condensed DAG.

    Returns an array `up[i]` listing all classes `j` such that `i <= j` in the
    reachability preorder (including `i` itself). This is the dual of
    `full_down_classes` and is useful for propagating “nonunitary” statuses
    upward when `covers` points from subfaces to faces.
  *)
  fun full_up_classes ({covers, ...}: graph_data) : int list array =
    let
      val k = length covers

      fun closureFrom (start: int) : int list =
        let
          val seen = Array.array (k, false)
          val () = Array.update (seen, start, true)
          fun bfs [] = ()
            | bfs (x :: xs) =
                let
                  val nbrs = List.nth (covers, x)
                  val new =
                    List.filter
                      (fn y =>
                        if Array.sub (seen, y) then false else (Array.update (seen, y, true); true))
                      nbrs
                in
                  bfs (xs @ new)
                end
          val () = bfs [start]
          val all = List.tabulate (k, fn i => i)
        in
          Sort.sort_u (op <=) (List.filter (fn i => Array.sub (seen, i)) all)
        end
    in
      Array.tabulate (k, closureFrom)
    end

  (* ---------------------------------------------------------------------- *)
  (* Face-table helpers (`face_classes.at` indexing utilities).              *)
  (* ---------------------------------------------------------------------- *)

  (* Prefix sums of row lengths. `levels fd` has length `#fd + 1` and starts with 0. *)
  fun levels (fd: 'a list list) : int list =
    let
      fun loop ([], total, acc) = List.rev (total :: acc)
        | loop (row :: rest, total, acc) =
            let
              val total' = total + length row
            in
              loop (rest, total', total :: acc)
            end
    in
      loop (fd, 0, [])
    end

  fun size_fd (fd: 'a list list) : int =
    case List.rev (levels fd) of
      [] => 0
    | n :: _ => n

  (* Flattened index function `(d,j) -> n` for a face table. *)
  fun index_f (fd: 'a list list) : int * int -> int =
    let
      val lvls = levels fd
    in
      fn (d, j) => List.nth (lvls, d) + j
    end

  (* Inverse of `index_f`: map flattened `n` to `(d,j)`. *)
  fun coords_f (fd: 'a list list) : int -> int * int =
    let
      val lvls = levels fd
      val nLvls = length lvls
    in
      fn n =>
        let
          val d = Basic.binary_search_first (fn i => List.nth (lvls, i) > n, 0, nLvls) - 1
          val j = n - List.nth (lvls, d)
        in
          (d, j)
        end
    end

  (* Build per-dimension lookup functions for a `[[FaceVertsKHash]]`-style table.
     For dimension `d`, the lookup maps the vertex-index list (length `d+1`) to
     the face index `j` in that dimension, or `~1` if absent. *)
  fun lookups_face_verts (fd: int list list list) : (int list -> int) list =
    let
      fun oneDim (d: int, row: int list list) : int list -> int =
        let
          val keys = List.map (fn v => List.take (v, d + 1)) row
          val h = Hash.make_vec_hash_data keys
        in
          fn key => Hash.lookup h key
        end
    in
      ListPair.mapEq oneDim (List.tabulate (length fd, fn i => i), fd)
    end

  (* ---------------------------------------------------------------------- *)
  (* `up_graph_gens` / `up_data` for `[[FaceVertsKHash]]`-style tables.       *)
  (* ---------------------------------------------------------------------- *)

  (* Delete an element at position `i` from a list (0-based). *)
  fun deleteAt (xs: 'a list, i: int) : 'a list =
    List.take (xs, i) @ List.drop (xs, i + 1)

  (* Base `up_graph_gens(FDKH)` from `face_classes.at` (red_count_flag=false case).
     - Always adds closure edges from a codim-1 subface to a face.
     - Adds a reverse edge when the “tail data” matches.

     Representation
     - `fd[d][j]` is an `int list` whose first `d+1` entries are vertex indices.
     - Any remaining entries encode auxiliary invariants (e.g. K-character hash indices).
  *)
  fun up_graph_gens_FDKH (fd: int list list list) : int list list =
    let
      val dims = length fd
      val index = index_f fd
      val lvls = levels fd
      val total = size_fd fd
      val edgeGens = Array.array (total, ([]: int list))
      val lookups = lookups_face_verts fd

      fun addEdge (u: int, v: int) =
        Array.update (edgeGens, u, v :: Array.sub (edgeGens, u))

      fun nth2 (xs: 'a list list, d: int, j: int) : 'a =
        List.nth (List.nth (xs, d), j)

      fun loopD d =
        if d >= dims then
          ()
        else if d = 0 then
          loopD 1
        else
          let
            val row = List.nth (fd, d)
            val lookupSub = List.nth (lookups, d - 1)
            fun loopJ (j1, []) = ()
              | loopJ (j1, find :: rest) =
                  let
                    val verts = List.take (find, d + 1)
                    fun loopE e =
                      if e > d then
                        ()
                      else
                        let
                          val subVerts = deleteAt (verts, e)
                          val j0 = lookupSub subVerts
                        in
                          if j0 >= 0 then
                            let
                              val u = index (d - 1, j0)
                              val v = index (d, j1)
                              val () = addEdge (u, v)
                              val sub = nth2 (fd, d - 1, j0)
                            in
                              if List.drop (find, d + 1) = List.drop (sub, d) then
                                addEdge (v, u)
                              else
                                ();
                              loopE (e + 1)
                            end
                          else
                            loopE (e + 1)
                        end
                  in
                    loopE 0;
                    loopJ (j1 + 1, rest)
                  end
          in
            loopJ (0, row);
            loopD (d + 1)
          end

      val () = loopD 0

      fun uniq xs = Sort.sort_u (op <=) xs
    in
      List.tabulate (total, fn i => uniq (Array.sub (edgeGens, i)))
    end

  fun up_data_FDKH (fd: int list list list) : graph_data =
    strong_components (up_graph_gens_FDKH fd)

  (* `up_graph_gens(FDKH, KTypePol_hash, level)` variant from `face_classes.at` (red_count_flag configurable).
     Compares truncated K-type polynomials (to `level`) for reverse-edge creation.

     Parameters
     - `polHash`: owning table of KTypePol (the face entries store indices into this table)
     - `level`: truncation height (must be >= 0 to enable this mode)
     - `redShift`: 0 when no `red_count` coord is stored; 1 when it is (shifts the indices)
  *)
  fun up_graph_gens_FDKH_kpol (fd: int list list list, polHash: KTypePolHash.t, level: int, redShift: int) : int list list =
    if level < 0 then
      up_graph_gens_FDKH fd
    else
      let
        val dims = length fd
        val index = index_f fd
        val total = size_fd fd
        val edgeGens = Array.array (total, ([]: int list))
        val lookups = lookups_face_verts fd

        fun addEdge (u: int, v: int) =
          Array.update (edgeGens, u, v :: Array.sub (edgeGens, u))

        fun nth2 (xs: 'a list list, d: int, j: int) : 'a =
          List.nth (List.nth (xs, d), j)

        fun truncEq (childIdx: int, parentIdx: int) : bool =
          let
            val child = KTypePolHash.index polHash childIdx
            val parent = KTypePolHash.index polHash parentIdx
            val tc = KTypePol.toHT (child, level)
            val tp = KTypePol.toHT (parent, level)
            val eq = AtlasFFI.atlas_ktypepol_equal (tc, tp) = 1
            val () = KTypePol.free tc
            val () = KTypePol.free tp
          in
            eq
          end

        fun loopD d =
          if d >= dims then
            ()
          else if d = 0 then
            loopD 1
          else
            let
              val row = List.nth (fd, d)
              val lookupSub = List.nth (lookups, d - 1)
              fun loopJ (j1, []) = ()
                | loopJ (j1, find :: rest) =
                    let
                      val verts = List.take (find, d + 1)
                      val childCharIdx = List.nth (find, d + redShift + 1)
                      val childLang = List.nth (find, d + redShift + 2)
                      fun loopE e =
                        if e > d then
                          ()
                        else
                          let
                            val subVerts = deleteAt (verts, e)
                            val j0 = lookupSub subVerts
                          in
                            if j0 >= 0 then
                              let
                                val u = index (d - 1, j0)
                                val v = index (d, j1)
                                val () = addEdge (u, v)
                                val sub = nth2 (fd, d - 1, j0)
                                val parentCharIdx = List.nth (sub, d + redShift)
                                val parentLang = List.nth (sub, d + redShift + 1)
                              in
                                if childLang = parentLang andalso truncEq (childCharIdx, parentCharIdx) then
                                  addEdge (v, u)
                                else
                                  ();
                                loopE (e + 1)
                              end
                            else
                              loopE (e + 1)
                          end
                    in
                      loopE 0;
                      loopJ (j1 + 1, rest)
                    end
            in
              loopJ (0, row);
              loopD (d + 1)
            end

        val () = loopD 0

        fun uniq xs = Sort.sort_u (op <=) xs
      in
        List.tabulate (total, fn i => uniq (Array.sub (edgeGens, i)))
      end

  fun up_data_FDKH_kpol (fd: int list list list, polHash: KTypePolHash.t, level: int, redShift: int) : graph_data =
    strong_components (up_graph_gens_FDKH_kpol (fd, polHash, level, redShift))

  (* `class_lists(FDKH, GD)` from `face_classes.at`: return per-dimension vectors
     mapping each face index to its SCC class id. *)
  fun class_lists_FDKH (fd: int list list list, gd: graph_data) : int list list =
    let
      val lvls = levels fd
      val total = size_fd fd
      val cll = class_list_linear (gd, total)

      fun slice (start: int, len: int) =
        List.take (List.drop (cll, start), len)

      fun oneDim d =
        let
          val start = List.nth (lvls, d)
          val len = length (List.nth (fd, d))
        in
          slice (start, len)
        end
    in
      List.tabulate (length fd, oneDim)
    end
end
