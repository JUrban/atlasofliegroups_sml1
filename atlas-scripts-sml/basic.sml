use "atlas-scripts-sml/MatrixAT.sml";

(* Minimal SML analogue of `atlas-scripts/basic.at` (partial).
   Provide core constructors used throughout `.at` scripts. *)
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

  fun vector (n: int, f: int -> 'a) : 'a list =
    if n < 0 then raise Fail "Basic.vector: negative length" else List.tabulate (n, f)

  fun matrix ((nRows: int, nCols: int), f: int * int -> int) : mat =
    if nRows < 0 orelse nCols < 0 then
      raise Fail "Basic.matrix: negative shape"
    else
      List.tabulate (nRows, fn i => List.tabulate (nCols, fn j => f (i, j)))

  fun indices (xs: 'a list) : int list =
    List.tabulate (length xs, fn i => i)

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

  fun binary_search_in (a: 't list, leq: 't * 't -> bool) : 't -> int option =
    fn x =>
      let
        val n = length a
        fun at i = List.nth (a, i)
        val i = binary_search_first (fn j => leq (x, at j), 0, n)
      in
        if i < n andalso leq (at i, x) then SOME i else NONE
      end

  fun binary_search_in_by (a: 's list, f: 's -> 't, leq: 't * 't -> bool) : 't -> int option =
    fn x =>
      let
        val n = length a
        fun at i = f (List.nth (a, i))
        val i = binary_search_first (fn j => leq (x, at j), 0, n)
      in
        if i < n andalso leq (at i, x) then SOME i else NONE
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

  (* Stable merge-sort. *)
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

  fun ranking (leq: 'a * 'a -> bool) (a: 'a list) : int list =
    let
      fun at i = List.nth (a, i)
      fun leqIdx (i, j) = leq (at i, at j)
    in
      sort leqIdx (indices a)
    end

  fun sort_by (f: 'a -> 'b, leq: 'b * 'b -> bool) (a: 'a list) : 'a list =
    let
      val vals = List.map f a
      val r = ranking leq vals
    in
      List.map (fn i => List.nth (a, i)) r
    end

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
end
