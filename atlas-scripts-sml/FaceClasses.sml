use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

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
end

