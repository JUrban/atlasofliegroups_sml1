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
end
