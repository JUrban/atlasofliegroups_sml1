use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/tabulate.sml";

(*
  File: atlas-scripts-sml/poset.sml

  Purpose
  - SML translation of `atlas-scripts/poset.at`.
  - Provides a small, generic partially-ordered-set utility module used by
    various higher-level scripts (e.g. Weyl-group and cell utilities).

  Representation
  - A poset is represented as an adjacency list `P : int list list` where
    `P[i]` contains the (possibly non-transitively-closed) set of `j` such that
    `i < j`.
  - The underlying carrier set is `{0,1,...,n-1}` where `n = length P`.

  Differences from the `.at` implementation
  - The `.at` version uses interpreter bitset primitives for an efficient
    transitive closure. This SML version computes closure via per-node DFS/BFS
    over adjacency lists.
  - `closure(P)` returns the reflexive+transitive closure (each row includes
    its own node `i`), matching the behavior of `trans_close` in `poset.at`.

  Intended usage
  - These utilities are designed primarily for “library script” scale posets
    (tens/hundreds of nodes). For very large posets, additional bitset-based
    optimization may be needed.
*)

structure Poset = struct
  type t = int list list

  fun appi f xs =
    let
      fun loop ([], _, _) = ()
        | loop (x :: rest, i, f) = (f (i, x); loop (rest, i + 1, f))
    in
      loop (xs, 0, f)
    end

  fun mapi f xs =
    let
      fun loop ([], _, acc) = List.rev acc
        | loop (x :: rest, i, acc) = loop (rest, i + 1, f (i, x) :: acc)
    in
      loop (xs, 0, [])
    end

  fun mapPartial f xs =
    let
      fun loop ([], acc) = List.rev acc
        | loop (x :: rest, acc) =
            (case f x of
               NONE => loop (rest, acc)
             | SOME y => loop (rest, y :: acc))
    in
      loop (xs, [])
    end

  fun size (P: t) : int = length P

  fun checkIndex (n: int) (j: int) : bool = j >= 0 andalso j < n

  fun normalizeRow (n: int, i: int) (row: int list) : int list =
    Basic.sort_u Int.<= (List.filter (checkIndex n) (i :: row))

  (* Reflexive+transitive closure. Row `i` contains `i` and all nodes reachable
     from `i` by following edges in `P`. *)
  fun closure (P: t) : t =
    let
      val n = size P
      val parr = Array.fromList P

      (* Normalize adjacency rows but do not add reflexivity here; we add it
         when exploring each start node. *)
      val adj =
        Array.tabulate
          (n, fn i => Basic.sort_u Int.<= (List.filter (checkIndex n) (Array.sub (parr, i))))

      fun reachFrom (start: int) : int list =
        let
          val seen = Array.array (n, false)
          fun pushAll ([], stack) = stack
            | pushAll (x :: xs, stack) = pushAll (xs, x :: stack)

          fun loop stack =
            (case stack of
               [] => ()
             | v :: rest =>
                 if Array.sub (seen, v) then
                   loop rest
                 else
                   let
                     val () = Array.update (seen, v, true)
                     val rest' = pushAll (Array.sub (adj, v), rest)
                   in
                     loop rest'
                   end)

          val () = loop [start]

          fun collect j acc =
            if j = n then
              List.rev acc
            else if Array.sub (seen, j) then
              collect (j + 1) (j :: acc)
            else
              collect (j + 1) acc
        in
          collect 0 []
        end
    in
      List.tabulate (n, reachFrom)
    end

  fun equal (A: t, B: t) : bool = A = B

  fun posets_equal (P: t, Q: t) : bool = closure P = closure Q

  fun poset_inverse (P: t) : t =
    let
      val P = closure P
      val n = size P
      val inv = Array.array (n, ([]: int list))

      fun addEdge (i: int, j: int) : unit = Array.update (inv, j, i :: Array.sub (inv, j))

      val () =
        appi (fn (i, row) => List.app (fn j => addEdge (i, j)) row) P
    in
      List.tabulate (n, fn j => Basic.sort_u Int.<= (Array.sub (inv, j)))
    end

  fun less (P: t, i: int, j: int) : bool =
    let
      val Q = closure P
      val n = size Q
      val () =
        if checkIndex n i andalso checkIndex n j then () else raise Fail "Poset.less: index out of range"
    in
      List.exists (fn k => k = j) (List.nth (Q, i))
    end

  fun greater (P: t, i: int, j: int) : bool = less (P, j, i)

  (* Nodes `i` such that nothing is strictly above `i` (i.e. closure row is `[i]`). *)
  fun minimal_nodes (P: t) : int list =
    let
      val Q = closure P
      val n = size Q
      fun isMin i = (List.nth (Q, i) = [i])
    in
      List.filter isMin (List.tabulate (n, fn i => i))
    end

  (* Nodes that are minimal and lie strictly below some other node. *)
  fun basic_nodes (P: t) : int list =
    let
      val mins = minimal_nodes P
      val inv = poset_inverse P
      fun hasStrictPred i = List.exists (fn j => j <> i) (List.nth (inv, i))
    in
      List.filter hasStrictPred mins
    end

  (* Remove all edges pointing to node `i`, keeping the node itself. *)
  fun delete_node (i: int, P: t) : t =
    let
      val n = size P
      val () = if checkIndex n i then () else raise Fail "Poset.delete_node: index out of range"
    in
      List.map (fn row => List.filter (fn j => j <> i) row) P
    end

  fun delete_nodes (S: int list, P: t) : t = List.foldl (fn (i, acc) => delete_node (i, acc)) P S

  (* Check whether `f` is monotone with respect to the poset relation.
     `f[i] = f(i)` for `i=0..n-1`. *)
  fun is_monotone (P: t, f: int list) : bool =
    let
      val n = size P
      val () = if length f = n then () else raise Fail "Poset.is_monotone: length mismatch"
      val Q = closure P

      fun leqIdx (a: int, b: int) : bool = List.exists (fn k => k = b) (List.nth (Q, a))
      fun fAt i = List.nth (f, i)
      fun checkRow (i: int, row: int list) : bool =
        List.all (fn j => leqIdx (fAt i, fAt j)) row
    in
      List.all (fn (i, row) => checkRow (i, row)) (ListPair.zip (List.tabulate (n, fn i => i), Q))
    end

  fun subPoset (P: t, S: int list) : t =
    let
      val Q = closure P
      val n = size Q
      val index = Array.array (n, ~1)
      val () =
        appi
          (fn (idx, node) =>
             if checkIndex n node then
               Array.update (index, node, idx)
             else
               raise Fail "Poset.subPoset: index out of range")
          S

      fun projectRow (row: int list) : int list =
        let
          val mapped =
            mapPartial
              (fn k =>
                 let
                   val idx = Array.sub (index, k)
                 in
                   if idx >= 0 then SOME idx else NONE
                 end)
              row
        in
          Basic.sort_u Int.<= mapped
        end
    in
      List.map (fn node => projectRow (List.nth (Q, node))) S
    end

  fun graph (P: t) : string =
    let
      fun labels i = Int.toString i
      fun colors (_: int) = "black"
    in
      graphWith (P, labels, colors)
    end

  and graphWith (P: t, labels: int -> string, colors: int -> string) : string =
    let
      val n = size P
      val header =
        String.concat
          ["strict digraph  { \n",
           "size=\"30.0,30.0!\"; \n",
           "center=true;  \n",
           "node [color=black,fontcolor=black] \n",
           " edge [arrowhead=none,color=black]; "]

      fun nodeLine i =
        "\n"
        ^ Int.toString i
        ^ "[label=\""
        ^ labels i
        ^ "\",color="
        ^ colors i
        ^ "];"

      fun edgeLines i row =
        String.concat (List.map (fn j => Int.toString i ^ "->" ^ Int.toString j ^ ";") row)

      fun one i =
        let
          val row = List.nth (P, i)
        in
          nodeLine i ^ edgeLines i row
        end
    in
      header ^ String.concat (List.tabulate (n, one)) ^ "\n}"
    end

  fun graphLabels (P: t, labels: int -> string) : string =
    graphWith (P, labels, fn _ => "black")

  fun graphLabelArray (P: t, labels: string list) : string =
    graphLabels (P, fn i => List.nth (labels, i))

  fun show (P: t) : unit =
    let
      fun intsToString xs =
        "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"
      val table = mapi (fn (i, row) => [Int.toString i, intsToString row]) P
    in
      Tabulate.tabulateDefault table
    end
end
