use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(* Partial SML analogue of `atlas-scripts/combinatorics.at`, focused on permutations. *)
structure Combinatorics = struct
  type permutation = int list
  type mat = IntMatrix.mat

  val is_permutation = MatrixAT.is_permutation

  fun permutation_matrix (pi: permutation) : mat =
    MatrixAT.permutation_matrix pi

  fun compose_permutations (sigma: permutation, pi: permutation) : permutation =
    let
      val n = length sigma
      val () = if length pi = n then () else raise Fail "Combinatorics.compose_permutations: length mismatch"
      fun at xs i = List.nth (xs, i)
    in
      List.map (fn i => at sigma i) pi
    end

  fun inverse (pi: permutation) : permutation =
    if not (is_permutation pi) then
      raise Fail "Combinatorics.inverse: not a permutation"
    else
      let
        val n = length pi
        val out = Array.array (n, 0)
        fun loop (j, []) = ()
          | loop (j, x :: xs) = (Array.update (out, x, j); loop (j + 1, xs))
        val () = loop (0, pi)
      in
        List.tabulate (n, fn i => Array.sub (out, i))
      end

  fun permutation_inverse (pi: permutation) : permutation =
    let
      val n = length pi
      val out = Array.array (n, 0)
      fun loop (j, []) = ()
        | loop (j, x :: xs) = (Array.update (out, x, j); loop (j + 1, xs))
      val () = loop (0, pi)
    in
      List.tabulate (n, fn i => Array.sub (out, i))
    end

  fun cyclic_permutation (n: int) (cycle: int list) : permutation =
    if n < 0 then
      raise Fail "Combinatorics.cyclic_permutation: negative n"
    else
      let
        val pi = Array.tabulate (n, fn i => i) (* identity *)
        val l = length cycle
        val () = if l = 0 then () else ()
        fun nth xs i = List.nth (xs, i)
        fun loop (idx, []) = ()
          | loop (idx, e :: es) =
              let
                val next = nth cycle ((idx + 1) mod l)
              in
                Array.update (pi, e, next);
                loop (idx + 1, es)
              end
        val () =
          if l = 0 then ()
          else if List.all (fn e => 0 <= e andalso e < n) cycle then loop (0, cycle)
          else raise Fail "Combinatorics.cyclic_permutation: cycle out of range"
      in
        List.tabulate (n, fn i => Array.sub (pi, i))
      end
end

