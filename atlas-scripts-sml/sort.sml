use "atlas-scripts-sml/basic.sml";

(* Partial SML analogue of `atlas-scripts/sort.at`, specialised to lists. *)
structure Sort = struct
  fun is_sorted (leq: 'a * 'a -> bool) (xs: 'a list) : bool =
    let
      fun loop [] = true
        | loop [_] = true
        | loop (a :: b :: rest) = leq (a, b) andalso loop (b :: rest)
    in
      loop xs
    end

  fun merge (leq: 'a * 'a -> bool) (a: 'a list, b: 'a list) : 'a list =
    (case (a, b) of
       ([], _) => b
     | (_, []) => a
     | (x :: xs, y :: ys) =>
         if leq (x, y) then x :: merge leq (xs, b) else y :: merge leq (a, ys))

  fun merge_u (leq: 'a * 'a -> bool) (a: 'a list, b: 'a list) : 'a list =
    (case (a, b) of
       ([], _) => b
     | (_, []) => a
     | (x :: xs, y :: ys) =>
         if not (leq (x, y)) then y :: merge_u leq (a, ys)
         else if not (leq (y, x)) then x :: merge_u leq (xs, b)
         else x :: merge_u leq (xs, ys))

  fun sort (leq: 'a * 'a -> bool) (xs: 'a list) : 'a list =
    let
      fun split xs =
        (case xs of
           [] => ([], [])
         | [x] => ([x], [])
         | x :: y :: rest =>
             let
               val (a, b) = split rest
             in
               (x :: a, y :: b)
             end)

      fun ms xs =
        (case xs of
           [] => []
         | [_] => xs
         | _ =>
             let
               val (a, b) = split xs
             in
               merge leq (ms a, ms b)
             end)
    in
      ms xs
    end

  fun sort_u (leq: 'a * 'a -> bool) (xs: 'a list) : 'a list =
    let
      fun split xs =
        (case xs of
           [] => ([], [])
         | [x] => ([x], [])
         | x :: y :: rest =>
             let
               val (a, b) = split rest
             in
               (x :: a, y :: b)
             end)

      fun ms xs =
        (case xs of
           [] => []
         | [_] => xs
         | _ =>
             let
               val (a, b) = split xs
             in
               merge_u leq (ms a, ms b)
             end)
    in
      ms xs
    end

  (* Restricted lex ordering for fixed-size integer vectors. *)
  fun rlex_leq (v: int list, w: int list) : bool =
    let
      val () = if length v = length w then () else raise Fail "Sort.rlex_leq: length mismatch"
      fun loop ([], []) = true
        | loop (a :: as', b :: bs') = if a <> b then a < b else loop (as', bs')
        | loop _ = raise Fail "Sort.rlex_leq: length mismatch"
    in
      loop (v, w)
    end

  fun sort_u_rlex (xs: int list list) : int list list =
    sort_u rlex_leq xs
end

