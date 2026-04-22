use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(*
  File: atlas-scripts-sml/combinatorics.sml

  Purpose
  - Partial SML analogue of `atlas-scripts/combinatorics.at`, focused on
    permutation utilities used by the diagram/folding code.
*)
structure Combinatorics = struct
  type permutation = int list
  type mat = IntMatrix.mat

  val is_permutation = MatrixAT.is_permutation

  (* Permutation matrix for `pi`. *)
  fun permutation_matrix (pi: permutation) : mat =
    MatrixAT.permutation_matrix pi

  (* Composition `sigma ∘ pi` (apply `pi` then `sigma`). *)
  fun compose_permutations (sigma: permutation, pi: permutation) : permutation =
    let
      val n = length sigma
      val () = if length pi = n then () else raise Fail "Combinatorics.compose_permutations: length mismatch"
      fun at xs i = List.nth (xs, i)
    in
      List.map (fn i => at sigma i) pi
    end

  (* Inverse permutation, validating input. *)
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

  (* Inverse permutation without validation (assumes `pi` is a permutation). *)
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

  (* Construct the permutation that cycles the listed elements and fixes others. *)
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
