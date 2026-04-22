use "atlas-scripts-sml/MatrixAT.sml";

(*
  File: atlas-scripts-sml/basic.sml

  Purpose
  - Minimal SML analogue of `atlas-scripts/basic.at` providing core list/matrix
    constructors and sorting/search helpers that many translated scripts rely on.

  Scope
  - This is intentionally not a full port of `basic.at`; it contains only the
    pieces needed by the current SML translations.
  - When new `.at` scripts are translated, add functionality here in small,
    reviewable increments and keep behavior compatible with the `.at` versions.
*)
structure Basic = struct
  type mat = MatrixAT.mat

  structure Maybe = struct
    type 'a t = 'a option
    val none : 'a t = NONE
    fun some (x: 'a) : 'a t = SOME x
  end

  val null = MatrixAT.null
  val id_mat = MatrixAT.id_mat
  val block_matrix = MatrixAT.block_matrix

  (* Create a length-`n` list by tabulating a function. *)
  fun vector (n: int, f: int -> 'a) : 'a list =
    if n < 0 then raise Fail "Basic.vector: negative length" else List.tabulate (n, f)

  (* Create an integer matrix by tabulating an `(i,j) -> int` function. *)
  fun matrix ((nRows: int, nCols: int), f: int * int -> int) : mat =
    if nRows < 0 orelse nCols < 0 then
      raise Fail "Basic.matrix: negative shape"
    else
      List.tabulate (nRows, fn i => List.tabulate (nCols, fn j => f (i, j)))

  (* Indices `0..length(xs)-1`. *)
  fun indices (xs: 'a list) : int list =
    List.tabulate (length xs, fn i => i)

  (* Binary search for the first index where `pred` becomes true on `[low,high)`. *)
  fun binary_search_first (pred: int -> bool, low: int, high: int) : int =
    let
      fun loop (l, h) =
        if l >= h then l
        else
          let
            val mid = (l + h) div 2
          in
            if pred mid then loop (l, mid) else loop (mid + 1, h)
          end
    in
      if low < 0 orelse high < low then
        raise Fail "Basic.binary_search_first: bad bounds"
      else
        loop (low, high)
    end

  (* Binary search in a sorted list using a total preorder `leq`.
     Returns the index of an equal element, or `NONE` if absent. *)
  fun binary_search_in (a: 't list, leq: 't * 't -> bool) : 't -> int option =
    fn x =>
      let
        val n = length a
        fun at i = List.nth (a, i)
        val i = binary_search_first (fn j => leq (x, at j), 0, n)
      in
        if i < n andalso leq (at i, x) then SOME i else NONE
      end

  (* Binary search in a list sorted by `f`, comparing by `leq`. *)
  fun binary_search_in_by (a: 's list, f: 's -> 't, leq: 't * 't -> bool) : 't -> int option =
    fn x =>
      let
        val n = length a
        fun at i = f (List.nth (a, i))
        val i = binary_search_first (fn j => leq (x, at j), 0, n)
      in
        if i < n andalso leq (at i, x) then SOME i else NONE
      end

  (* Merge two sorted lists. *)
  fun merge (leq: 'a * 'a -> bool) (a: 'a list, b: 'a list) : 'a list =
    (case (a, b) of
       ([], _) => b
     | (_, []) => a
     | (x :: xs, y :: ys) =>
         if leq (x, y) then x :: merge leq (xs, b) else y :: merge leq (a, ys))

  (* Merge two sorted lists, removing duplicates under `leq`. *)
  fun merge_u (leq: 'a * 'a -> bool) (a: 'a list, b: 'a list) : 'a list =
    (case (a, b) of
       ([], _) => b
     | (_, []) => a
     | (x :: xs, y :: ys) =>
         if not (leq (x, y)) then y :: merge_u leq (a, ys)
         else if not (leq (y, x)) then x :: merge_u leq (xs, b)
         else x :: merge_u leq (xs, ys))

  (* Stable merge-sort. *)
  (* Sort a list using a total preorder `leq`. *)
  fun sort (leq: 'a * 'a -> bool) (xs: 'a list) : 'a list =
    let
      fun split xs =
        let
          fun loop (slow, fast, acc) =
            (case fast of
               [] => (List.rev acc, slow)
             | [_] => (List.rev acc, slow)
             | _ :: _ :: fast' =>
                 (case slow of
                    [] => (List.rev acc, [])
                  | s :: slow' => loop (slow', fast', s :: acc)))
        in
          loop (xs, xs, [])
        end

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

  (* Sort and remove duplicates under `leq` (stable for first occurrences). *)
  fun sort_u (leq: 'a * 'a -> bool) (xs: 'a list) : 'a list =
    let
      fun split xs =
        let
          fun loop (slow, fast, acc) =
            (case fast of
               [] => (List.rev acc, slow)
             | [_] => (List.rev acc, slow)
             | _ :: _ :: fast' =>
                 (case slow of
                    [] => (List.rev acc, [])
                  | s :: slow' => loop (slow', fast', s :: acc)))
        in
          loop (xs, xs, [])
        end

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

  (* Return the permutation of indices that sorts a list by `leq`. *)
  fun ranking (leq: 'a * 'a -> bool) (a: 'a list) : int list =
    let
      fun at i = List.nth (a, i)
      fun leqIdx (i, j) = leq (at i, at j)
    in
      sort leqIdx (indices a)
    end

  (* Sort a list by a projection `f`. *)
  fun sort_by (f: 'a -> 'b, leq: 'b * 'b -> bool) (a: 'a list) : 'a list =
    let
      val vals = List.map f a
      val r = ranking leq vals
    in
      List.map (fn i => List.nth (a, i)) r
    end

  (* Sort a list by a projection `f`, removing duplicates under `leq`. *)
  fun sort_u_by (f: 'a -> 'b, leq: 'b * 'b -> bool) (a: 'a list) : 'a list =
    let
      val vals = List.map f a
      val r = ranking leq vals
      fun eq (x, y) = leq (x, y) andalso leq (y, x)
      fun loop ([], _) = []
        | loop (i :: is, NONE) = List.nth (a, i) :: loop (is, SOME (List.nth (vals, i)))
        | loop (i :: is, SOME prev) =
            let
              val v = List.nth (vals, i)
            in
              if eq (v, prev) then loop (is, SOME prev) else List.nth (a, i) :: loop (is, SOME v)
            end
    in
      loop (r, NONE)
    end

  (* Power set of a list (exponential in length). *)
  fun power_set (xs: 'a list) : 'a list list =
    let
      fun loop [] = [[]]
        | loop (x :: rest) =
            let
              val p = loop rest
            in
              p @ List.map (fn s => x :: s) p
            end
    in
      loop xs
    end

  (* All k-subsets of xs, preserving original order inside each subset. *)
  (* Combinations of size `k` from `xs`, preserving original order. *)
  fun choices_from (xs: 'a list, k: int) : 'a list list =
    if k < 0 then
      raise Fail "Basic.choices_from: negative k"
    else if k = 0 then
      [[]]
    else
      (case xs of
         [] => []
       | x :: rest =>
           if k > length xs then []
           else
             List.map (fn ys => x :: ys) (choices_from (rest, k - 1)) @ choices_from (rest, k))

  (* All 0/1 vectors of length `n` with coordinate sum `k`. *)
  fun all_0_1_vecs_with_sum (n: int, k: int) : int list list =
    if n < 0 then
      raise Fail "Basic.all_0_1_vecs_with_sum: negative n"
    else
      let
        val idxs = choices_from (List.tabulate (n, fn i => i), k)
        fun mk is =
          let
            val v = Array.array (n, 0)
            val () = List.app (fn i => Array.update (v, i, 1)) is
          in
            List.tabulate (n, fn i => Array.sub (v, i))
          end
      in
        List.map mk idxs
      end
end
