use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/partitions.sml

  Purpose
  - Partial SML translation of `atlas-scripts/partitions.at`.
  - Provides the purely combinatorial partition helpers:
      - transpose of a partition
      - hook-length and hook-length formula dimension
      - weak compositions with bounded total

  Representation
  - A partition is represented as an `int list` of row lengths in nonincreasing
    order, e.g. `[4,2,2,1]`.

  Scope / limitations
  - The `.at` file also contains partition-function / Kostant-function code
    based on `ParamPol`. That part is not ported here yet.
  - Hook-length dimensions grow extremely quickly; we return `IntInf.int`.
*)

structure Partitions = struct
  type partition = int list

  fun size (p: partition) : int = List.foldl (op +) 0 p

  (* Transpose of a partition (Ferrers diagram transpose). *)
  fun transpose (p: partition) : partition =
    let
      val maxRow = List.foldl Int.max 0 p
      fun colHeight j = List.foldl (fn (r, acc) => if r >= j then acc + 1 else acc) 0 p
    in
      List.tabulate (maxRow, fn t => colHeight (t + 1))
    end

  (* Hook length at 0-based cell coordinates (i,j): i row index, j column index. *)
  fun hook_length (p: partition, i: int, j: int) : int =
    let
      val rowLen = List.nth (p, i)
      val arm = rowLen - j
      val leg = List.nth (transpose p, j) - i
    in
      arm + leg - 1
    end

  fun factorial (n: int) : IntInf.int =
    if n < 0 then raise Fail "Partitions.factorial: negative"
    else
      let
        fun loop (k, acc) =
          if k <= 1 then acc else loop (k - 1, acc * IntInf.fromInt k)
      in
        loop (n, 1)
      end

  (* Dimension of the irreducible S_n representation indexed by partition p. *)
  fun dim_rep (p: partition) : IntInf.int =
    let
      val n = size p
      val dim0 = factorial n
      fun loopRow (i, dim) =
        if i >= length p then dim
        else
          let
            val rowLen = List.nth (p, i)
            fun loopCol (j, d) =
              if j >= rowLen then d
              else
                let
                  val h = hook_length (p, i, j)
                in
                  loopCol (j + 1, IntInf.div (d, IntInf.fromInt h))
                end
          in
            loopRow (i + 1, loopCol (0, dim))
          end
    in
      loopRow (0, dim0)
    end

  (* All weak compositions of numbers <= limit into n parts. *)
  fun compositions_le (limit: int, n: int) : int list list =
    if limit < 0 then raise Fail "Partitions.compositions_le: negative limit"
    else if n < 0 then raise Fail "Partitions.compositions_le: negative n"
    else if n = 0 then
      [[]]
    else
      let
        fun loop last =
          if last > limit then []
          else
            let
              val rest = compositions_le (limit - last, n - 1)
              val withLast = List.map (fn r => r @ [last]) rest
            in
              withLast @ loop (last + 1)
            end
      in
        loop 0
      end
end

