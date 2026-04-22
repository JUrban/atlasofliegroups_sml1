use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/BigRat.sml";

(*
  File: atlas-scripts-sml/Gaussian_elim.sml

  Purpose
  - SML translation of `atlas-scripts/Gaussian_elim.at`.
  - Implements Gauss–Jordan style elimination on rational matrices represented
    as lists of columns, returning pivot data that can be applied to solve
    linear systems, compute determinants, and invert matrices.

  Representation (matching the `.at` script)
  - A matrix `M` is `ratvec list` where each `ratvec` is a column, i.e.:
      `type mat = rat list list` with `M[j][i]` = entry at row `i`, column `j`.
  - Vectors are lists of rationals.

  Notes
  - The `.at` code uses a union type for pivot data; here we use an SML datatype.
  - This is intended as a utility for other translated scripts; it is not
    performance-tuned yet.
*)

structure Gaussian_elim = struct
  type rat = BigRat.t
  type vec = rat list
  type mat = vec list (* list of columns *)

  datatype pivot_info =
    Pivot of int * vec (* pivot row index k, and row-operation coefficients *)
  | NoPivot of vec (* initial part of the column up to the pivot level *)

  type affine_subspace = {base_point: vec, tangent_space: mat}

  fun rat0 () = BigRat.zero ()
  fun rat1 () = BigRat.one ()

  fun vecLength v = length v

  fun nth (xs, i) = List.nth (xs, i)

  fun take (xs, k) = if k <= 0 then [] else List.take (xs, k)
  fun drop (xs, k) = if k <= 0 then xs else List.drop (xs, k)

  fun allZero xs = List.all BigRat.isZero xs

  (* Multiply (column-major) rational matrix by vector. *)
  fun timesMatVec (m: mat, v: vec) : vec =
    (case m of
       [] => []
     | col0 :: _ =>
         let
           val n = length col0
           val () = if length v = length m then () else raise Fail "Gaussian_elim.timesMatVec: dim mismatch"
           val () = if List.all (fn c => length c = n) m then () else raise Fail "Gaussian_elim.timesMatVec: ragged columns"
           fun entry i =
             let
               fun step ((col, coeff), acc) =
                 BigRat.add (acc, BigRat.mul (nth (col, i), coeff))
             in
               List.foldl step (rat0 ()) (ListPair.zipEq (m, v))
             end
         in
           List.tabulate (n, entry)
         end)

  fun timesMatMat (m: mat, n: mat) : mat =
    List.map (fn col => timesMatVec (m, col)) n

  (* Choose the first nonzero entry as pivot. Return index in the list, or ~1. *)
  fun greedy (choices: vec) : int =
    let
      fun loop ([], _) = ~1
        | loop (q :: qs, i) = if BigRat.isZero q then loop (qs, i + 1) else i
    in
      loop (choices, 0)
    end

  (* One pivot step on the first column of `m`, with `i` prior pivots. *)
  fun pivot_step (choose_pivot: vec -> int) (m: mat, i: int) : pivot_info * mat =
    (case m of
       [] => raise Fail "Gaussian_elim.pivot_step: empty matrix"
     | col :: rest =>
         let
           val pivot = choose_pivot (drop (col, i))
         in
           if pivot < 0 then
             (NoPivot (take (col, i)), rest)
           else
             let
               val k = i + pivot
               val n = length col
               val f = nth (col, k)
               val () = if BigRat.isZero f then raise Fail "Gaussian_elim.pivot_step: zero pivot" else ()

               fun src r =
                 if r < i orelse r > k then r
                 else if r = i then k
                 else r - 1

               fun coef r =
                 if r = i then BigRat.inv f else BigRat.neg (BigRat.divRat (nth (col, src r), f))
               val coefs = List.tabulate (n, coef)

               fun reduceColumn (c: vec) : vec =
                 let
                   val ck = nth (c, k)
                   fun entry r =
                     if r = i then
                       BigRat.divRat (ck, f)
                     else
                       BigRat.add (nth (c, src r), BigRat.mul (ck, nth (coefs, r)))
                 in
                   List.tabulate (n, entry)
                 end

               val rest' = List.map reduceColumn rest
             in
               (Pivot (k, coefs), rest')
             end
         end)

  val step = pivot_step greedy

  (* Collect pivot info for successive columns until matrix is empty. *)
  fun get_data (m0: mat) : pivot_info list =
    let
      fun loop (m, i, acc) =
        (case m of
           [] => List.rev acc
         | _ =>
             let
               val (pi, m1) = step (m, i)
               val i' =
                 (case pi of
                    Pivot _ => i + 1
                  | NoPivot _ => i)
             in
               loop (m1, i', pi :: acc)
             end)
    in
      loop (m0, 0, [])
    end

  (* `split_data` from the `.at` script: combine pivot info, compute kernel, and
     produce a “spread” function embedding reduced RHS coordinates. *)
  fun split_data (data: pivot_info list) : (int * vec) list * mat * (vec -> vec) =
    let
      val n = length data (* number of original columns *)

      fun embed (pivot_cols: int list) (v: vec) : vec =
        let
          val result = Array.array (n, rat0 ())
          fun loop ([], _, _) = ()
            | loop (c :: cs, idx, vs) =
                (case vs of
                   [] => raise Fail "Gaussian_elim.embed: length mismatch"
                 | value :: rest =>
                     (Array.update (result, c, value); loop (cs, idx + 1, rest)))
          val () = loop (pivot_cols, 0, v)
        in
          List.tabulate (n, fn j => Array.sub (result, j))
        end

      fun fold ([], _, pivots, kernel, pivot_cols) =
            let
              val spread = embed (List.rev pivot_cols)
            in
              (List.rev pivots, List.rev kernel, spread)
            end
        | fold (d :: ds, j, pivots, kernel, pivot_cols) =
            (case d of
               Pivot (k, coefs) => fold (ds, j + 1, (k, coefs) :: pivots, kernel, j :: pivot_cols)
             | NoPivot col =>
                 let
                   val piv_cols_rev = List.rev pivot_cols
                   val kc = embed piv_cols_rev col
                   val kcArr = Array.fromList kc
                   val () = Array.update (kcArr, j, BigRat.neg (rat1 ()))
                   val kc' = Array.foldr (op ::) [] kcArr
                 in
                   fold (ds, j + 1, pivots, kc' :: kernel, pivot_cols)
                 end)
    in
      fold (data, 0, [], [], [])
    end

  (* Determinant from pivot data; returns 0 if kernel nontrivial. *)
  fun det (a: mat) : rat =
    let
      val (pivots, kernel, _) = split_data (get_data a)
    in
      if not (null kernel) then
        rat0 ()
      else
        let
          fun signPow n = if n mod 2 = 0 then 1 else ~1
          fun step ((k, coefs), i, acc) =
            let
              val s = signPow (k - i)
              val diag = nth (coefs, i)
              val invDiag = BigRat.inv diag
              val factor = if s = 1 then invDiag else BigRat.neg invDiag
            in
              BigRat.mul (acc, factor)
            end
          fun loop ([], _, acc) = acc
            | loop (p :: ps, i, acc) = loop (ps, i + 1, step (p, i, acc))
        in
          loop (pivots, 0, rat1 ())
        end
    end

  (* Apply one pivot row operation to a column vector. *)
  fun apply_one (i: int, k: int, coefs: vec) (c: vec) : vec =
    let
      val n = length coefs
      val () = if length c = n then () else raise Fail "Gaussian_elim.apply_one: dim mismatch"
      val ck = nth (c, k)
      fun src r = if r < i orelse r > k then r else r - 1
      fun entry r =
        let
          val a = BigRat.mul (ck, nth (coefs, r))
        in
          if r = i then
            a
          else if i = k then
            BigRat.add (nth (c, r), a)
          else
            BigRat.add (nth (c, src r), a)
        end
    in
      List.tabulate (n, entry)
    end

  (* Compose all pivot operations. *)
  fun apply (pivots: (int * vec) list) (col: vec) : vec =
    let
      fun loop ([], _, c) = c
        | loop ((k, coefs) :: ps, i, c) = loop (ps, i + 1, apply_one (i, k, coefs) c)
    in
      loop (pivots, 0, col)
    end

  (* Solve `A*x=b`, returning an affine subspace of solutions if consistent. *)
  fun full_solve (a: mat, b: vec) : affine_subspace option =
    let
      val (pivots, kernel, spread) = split_data (get_data a)
      val reduced_rhs = apply pivots b
      val rank = length pivots
      val tail = drop (reduced_rhs, rank)
    in
      if allZero tail then
        SOME {base_point = spread (take (reduced_rhs, rank)), tangent_space = kernel}
      else
        NONE
    end

  fun a_solution (a: mat, b: vec) : vec =
    (case full_solve (a, b) of
       SOME sol => #base_point sol
     | NONE => raise Fail "Gaussian_elim.a_solution: no solution")

  (* Rational identity matrix, column-major. *)
  fun id_mat (n: int) : mat =
    List.tabulate
      ( n
      , fn j => List.tabulate (n, fn i => if i = j then rat1 () else rat0 ())
      )

  fun make_inverse (pivots: (int * vec) list) : mat =
    List.map (apply pivots) (id_mat (length pivots))

  fun inverse (m: mat) : mat =
    let
      val n =
        (case m of
           [] => 0
         | col0 :: _ => length col0)
      val () = if length m = n then () else raise Fail "Gaussian_elim.inverse: non-square matrix"
      val (pivots, kernel, _) = split_data (get_data m)
    in
      if null kernel then
        make_inverse pivots
      else
        raise Fail "Gaussian_elim.inverse: matrix has nontrivial kernel"
    end
end
