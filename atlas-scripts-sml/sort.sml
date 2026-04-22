use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/sort.sml

  Purpose
  - Partial SML analogue of `atlas-scripts/sort.at`, specialized to list-based
    representations.

  Notes
  - This module currently provides only what the translated scripts need:
      - sortedness predicate
      - stable sort wrappers (delegating to `Basic`)
      - a restricted lex ordering on fixed-size integer vectors (rlex)
*)
structure Sort = struct
  (* Check whether a list is nondecreasing under `leq`. *)
  fun is_sorted (leq: 'a * 'a -> bool) (xs: 'a list) : bool =
    let
      fun loop [] = true
        | loop [_] = true
        | loop (a :: b :: rest) = leq (a, b) andalso loop (b :: rest)
    in
      loop xs
    end

  val sort = Basic.sort
  val sort_u = Basic.sort_u

  (* Restricted lex ordering for fixed-size integer vectors. *)
  (* Lexicographic ordering on fixed-length integer vectors. *)
  fun rlex_leq (v: int list, w: int list) : bool =
    let
      val () = if length v = length w then () else raise Fail "Sort.rlex_leq: length mismatch"
      fun loop ([], []) = true
        | loop (a :: as', b :: bs') = if a <> b then a < b else loop (as', bs')
        | loop _ = raise Fail "Sort.rlex_leq: length mismatch"
    in
      loop (v, w)
    end

  (* Sort and unique integer vectors under `rlex_leq`. *)
  fun sort_u_rlex (xs: int list list) : int list list =
    sort_u rlex_leq xs
end
