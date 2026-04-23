use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/strong_comp_test.sml

  Purpose
  - SML translation of `atlas-scripts/strong_comp_test.at`.
  - This is a small synthetic graph generator used to stress-test
    strongly-connected-components implementations.

  Graph model
  - Vertices are subsets of `{0,...,n-1}` encoded as integers `S` with bits:
      bit `s` is set  <=>  `s ∈ S`.
  - Always include edges `S \ {s} -> S` for each `s ∈ S`.
  - Also include reverse edges `S -> S \ {s}` when `s mod m = 0`.

  Notes
  - The number of vertices is `2^n`, so this is only meaningful for small `n`.
*)

structure StrongCompTest = struct
  (* Return list of bit positions set in `x` (LSB = position 0). *)
  fun set_bit_positions (x: int) : int list =
    if x < 0 then
      raise Fail "StrongCompTest.set_bit_positions: negative input"
    else
      let
        fun loop (k, y, acc) =
          if y = 0 then
            List.rev acc
          else
            let
              val acc' = if (y mod 2) = 1 then k :: acc else acc
            in
              loop (k + 1, y div 2, acc')
            end
      in
        loop (0, x, [])
      end

  fun pow2 (n: int) : int =
    if n < 0 then raise Fail "StrongCompTest.pow2: negative exponent"
    else if n >= 30 then raise Fail "StrongCompTest.pow2: exponent too large for int"
    else
      let
        fun loop (0, acc) = acc
          | loop (k, acc) = loop (k - 1, acc * 2)
      in
        loop (n, 1)
      end

  (* Port of `.at` `UGG(n,m)`; returns adjacency lists by vertex id. *)
  fun UGG (n: int, m: int) : int list list =
    if n < 0 then
      raise Fail "StrongCompTest.UGG: negative n"
    else if m <= 0 then
      raise Fail "StrongCompTest.UGG: m must be positive"
    else
      let
        val N = pow2 n
        val edges = Array.array (N, ([]: int list))

        fun addEdge (src: int, dst: int) =
          Array.update (edges, src, dst :: Array.sub (edges, src))

        fun visitS S =
          let
            val bits = set_bit_positions S
            fun oneBit s =
              let
                val Sdown = S - pow2 s
                val () = addEdge (Sdown, S)
                val () = if s mod m = 0 then addEdge (S, Sdown) else ()
              in
                ()
              end
          in
            List.app oneBit bits
          end

        val () = List.app visitS (List.tabulate (N, fn i => i))

        val out = if (n mod m) > 0 then (n div m) + 1 else (n div m)
        val E = Array.foldl (fn (xs, acc) => acc + length xs) 0 edges
        val () =
          TextIO.print
            ("created graph of "
             ^ Int.toString N
             ^ " vertices, "
             ^ Int.toString E
             ^ " edges, "
             ^ Int.toString (pow2 (n - out))
             ^ " equiv classes.\n")
      in
        Array.foldr (op ::) [] edges
      end

  fun graph_size (edges: int list list) : int * int =
    let
      val N = length edges
      val E = List.foldl (fn (xs, acc) => acc + length xs) 0 edges
    in
      (N, E)
    end

  fun num_classes (eq: int list list, _: int list list) : int = length eq

  fun line (n: int) : int list list =
    if n < 0 then raise Fail "StrongCompTest.line: negative n"
    else
      List.tabulate (n, fn j => if j < n - 1 then [j + 1] else [])
end
