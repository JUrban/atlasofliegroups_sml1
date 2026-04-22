use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/poset.sml

  Purpose
  - Partial SML translation of `atlas-scripts/poset.at`.
  - Provides a simple partially ordered set type `Poset.t` encoded by adjacency
    lists, plus transitive/reflexive closure and a few query helpers.

  Representation
  - A poset is a list `P : int list list` of length `n`.
  - The underlying set is `{0,1,...,n-1}`.
  - `P[i]` lists the elements *strictly above* `i` (i.e. edges `i -> j`).
    (As in the `.at` file, `i` itself need not be included in `P[i]`.)

  Closure conventions
  - `closure P` returns a poset `Q` where:
      - `i` is included in `Q[i]` (reflexive closure)
      - `Q` is transitively closed
    This matches the behavior of `poset.at`’s `closure`.

  Performance notes
  - The `.at` implementation uses bitsets for speed.
  - This SML port uses BFS/DFS from each vertex; it is suitable for moderate
    sizes and can be improved later if needed.
*)

structure Poset = struct
  type t = int list list

  fun size (p: t) : int = length p

  fun checkIndex (n: int) (i: int) : unit =
    if 0 <= i andalso i < n then () else raise Fail "Poset: index out of bounds"

  fun closure (p: t) : t =
    let
      val n = size p
      val () = List.app (fn row => List.app (checkIndex n) row) p

      fun reachableFrom (start: int) : int list =
        let
          val seen = Array.array (n, false)
          fun push i = if Array.sub (seen, i) then () else Array.update (seen, i, true)
          val () = push start

          fun bfs [] = ()
            | bfs (x :: xs) =
                let
                  val nbrs = List.nth (p, x)
                  val new =
                    List.filter
                      (fn y =>
                        if Array.sub (seen, y) then false else (Array.update (seen, y, true); true))
                      nbrs
                in
                  bfs (xs @ new)
                end
        in
          bfs [start];
          let
            val all = List.tabulate (n, fn i => i)
            val kept = List.filter (fn i => Array.sub (seen, i)) all
          in
            Sort.sort_u (op <=) kept
          end
        end

      fun row i = reachableFrom i
    in
      List.tabulate (n, row)
    end

  fun posets_equal (p: t, q: t) : bool =
    let
      val pc = closure p
      val qc = closure q
    in
      pc = qc
    end

  fun poset_inverse (p: t) : t =
    let
      val pc = closure p
      val n = size pc
      val inv = Array.array (n, ([]: int list))
      fun add (i, j) = Array.update (inv, j, i :: Array.sub (inv, j))
      fun appi f xs =
        let
          fun loop ([], _) = ()
            | loop (x :: rest, i) = (f (i, x); loop (rest, i + 1))
        in
          loop (xs, 0)
        end
      val () =
        appi (fn (i, row) => List.app (fn j => add (i, j)) row) pc
    in
      List.tabulate (n, fn j => Sort.sort_u (op <=) (Array.sub (inv, j)))
    end

  (* `less(P,i,j)` in `poset.at`: whether `i <= j` in the reflexive/transitive closure. *)
  fun less (p: t, i: int, j: int) : bool =
    let
      val pc = closure p
      val n = size pc
      val () = (checkIndex n i; checkIndex n j)
    in
      List.exists (fn x => x = j) (List.nth (pc, i))
    end

  fun greater (p: t, i: int, j: int) : bool = less (p, j, i)

  (* Nodes which point to nothing beyond themselves. *)
  fun minimal_nodes (p: t) : int list =
    let
      val pc = closure p
      val n = size pc
    in
      List.filter (fn i => List.nth (pc, i) = [i]) (List.tabulate (n, fn i => i))
    end

  (* “Basic nodes” as described in the `.at` comments:
     - closed orbit (minimal node)
     - contained in the closure of a strictly bigger orbit
     Equivalently: minimal nodes whose inverse-closure set has size > 1. *)
  fun basic_nodes (p: t) : int list =
    let
      val mins = minimal_nodes p
      val inv = poset_inverse p
    in
      List.filter (fn j => length (List.nth (inv, j)) > 1) mins
    end

  fun delete_node (i: int, p: t) : t =
    let
      val n = size p
      val () = checkIndex n i
      fun del row = List.filter (fn x => x <> i) row
    in
      List.map del p
    end

  fun delete_nodes (is: int list, p: t) : t =
    List.foldl (fn (i, acc) => delete_node (i, acc)) p is

  (* DOT graph output, similar to `graph(Poset)` in `poset.at`.

     Note: This emits all edges in `p` (not the transitive reduction).
  *)
  fun graph (p: t, labelOf: int -> string, colorOf: int -> string) : string =
    let
      val n = size p
      val header =
        "strict digraph  { \n" ^
        "size=\"30.0,30.0!\";\n" ^
        "center=true;\n" ^
        "node [color=black,fontcolor=black]\n" ^
        "edge [arrowhead=none,color=black];"

      fun nodeLine i =
        "\n" ^ Int.toString i ^ "[label=\"" ^ labelOf i ^ "\",color=" ^ colorOf i ^ "];"

      fun edgeLines i =
        String.concat (List.map (fn j => Int.toString i ^ "->" ^ Int.toString j ^ ";") (List.nth (p, i)))

      val body =
        String.concat
          (List.tabulate
             ( n
             , fn i => nodeLine i ^ edgeLines i
             ))
    in
      header ^ body ^ "\n}"
    end

  fun graph_default (p: t) : string =
    graph (p, Int.toString, fn _ => "black")

  fun graph_labels (p: t, labels: int -> string) : string =
    graph (p, labels, fn _ => "black")
end
