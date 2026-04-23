use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/polynomial.sml

  Purpose
  - Minimal SML translation of the “integer polynomial” core of
    `atlas-scripts/polynomial.at`, sufficient to support ports that manipulate
    unitriangular polynomial matrices (e.g. `inverse.at`).

  Representation
  - An integer polynomial is a dense coefficient list:
      `type i_poly = int list`
    where the coefficient of X^k is at index k.
  - The zero polynomial is represented as `[]`.
  - `i_poly_mat` is a square matrix represented by rows:
      `type i_poly_mat = i_poly list list`

  Scope / limitations
  - This is not a full port of `polynomial.at` (no `int_poly` / `rat_poly`).
  - Arithmetic uses `int` and may overflow on large coefficients.
*)

structure Polynomial = struct
  type i_poly = int list
  type i_poly_mat = i_poly list list (* rows *)

  val poly_0 : i_poly = []
  val poly_1 : i_poly = [1]
  val poly_2 : i_poly = [2]
  val poly_q : i_poly = [0, 1]
  val poly_q2 : i_poly = [0, 0, 1]

  fun strip (p: i_poly) : i_poly =
    let
      fun drop0 xs =
        (case xs of
           [] => []
         | 0 :: rest => drop0 rest
         | _ => xs)
    in
      List.rev (drop0 (List.rev p))
    end

  fun isZero (p: i_poly) : bool = null (strip p)

  fun degree (p: i_poly) : int = length (strip p) - 1

  fun constant_poly (n: int) : i_poly = if n = 0 then poly_0 else [n]

  fun add (a: i_poly, b: i_poly) : i_poly =
    let
      val na = length a
      val nb = length b
      val n = Int.max (na, nb)
      fun coeff (xs, i) = if i < length xs then List.nth (xs, i) else 0
      val res = List.tabulate (n, fn i => coeff (a, i) + coeff (b, i))
    in
      strip res
    end

  fun neg (a: i_poly) : i_poly = strip (List.map (fn x => ~x) a)

  fun sub (a: i_poly, b: i_poly) : i_poly = add (a, neg b)

  fun mul (a: i_poly, b: i_poly) : i_poly =
    let
      val a = strip a
      val b = strip b
      val na = length a
      val nb = length b
    in
      if na = 0 orelse nb = 0 then
        poly_0
      else
        let
          val res = Array.array (na + nb - 1, 0)
          fun loopA i =
            if i >= na then ()
            else
              let
                val ai = List.nth (a, i)
              in
                if ai = 0 then loopA (i + 1)
                else
                  let
                    fun loopB j =
                      if j >= nb then ()
                      else
                        let
                          val bj = List.nth (b, j)
                          val idx = i + j
                        in
                          if bj = 0 then ()
                          else Array.update (res, idx, Array.sub (res, idx) + ai * bj);
                          loopB (j + 1)
                        end
                  in
                    loopB 0;
                    loopA (i + 1)
                  end
              end
          val () = loopA 0
        in
          strip (Array.foldr (op ::) [] res)
        end
    end

  (* Alias matching `.at` naming used by `inverse.at`. *)
  val poly_product = mul

  fun identity_poly_matrix (n: int) : i_poly_mat =
    if n < 0 then raise Fail "Polynomial.identity_poly_matrix: negative n"
    else
      List.tabulate (n, fn i => List.tabulate (n, fn j => if i = j then poly_1 else poly_0))

  fun matShape (m: i_poly_mat) : int * int =
    (case m of
       [] => (0, 0)
     | r0 :: rs =>
         let
           val k = length r0
           val () = if List.all (fn r => length r = k) rs then () else raise Fail "Polynomial.matShape: ragged"
         in
           (length m, k)
         end)

  fun matMul (a: i_poly_mat, b: i_poly_mat) : i_poly_mat =
    let
      val (ra, ca) = matShape a
      val (rb, cb) = matShape b
      val () = if ca = rb then () else raise Fail "Polynomial.matMul: dim mismatch"
      fun dotRowCol (row: i_poly list, j: int) : i_poly =
        let
          fun entry i = List.nth (List.nth (b, i), j)
          fun step (i, acc) = add (acc, mul (List.nth (row, i), entry i))
        in
          List.foldl step poly_0 (List.tabulate (ca, fn i => i))
        end
      fun row i =
        let
          val r = List.nth (a, i)
        in
          List.tabulate (cb, fn j => dotRowCol (r, j))
        end
    in
      List.tabulate (ra, row)
    end

  (* ---------- evaluation (subset of `polynomial.at`) ---------- *)

  fun evaluate_at_1 (p: i_poly) : int =
    List.foldl (op +) 0 (strip p)

  (* Horner evaluation at an integer. *)
  fun eval_int (p: i_poly, k: int) : int =
    let
      fun step (e, acc) = e + k * acc
    in
      List.foldr step 0 (strip p)
    end

  (* ---------- polynomial matrices (subset of `polynomial.at`) ---------- *)

  fun shape (m: i_poly_mat) : int * int =
    matShape m

  fun poly_list_add (v: i_poly list, w: i_poly list) : i_poly list =
    if length v <> length w then raise Fail "Polynomial.poly_list_add: length mismatch"
    else ListPair.mapEq add (v, w)

  fun poly_list_sub (v: i_poly list, w: i_poly list) : i_poly list =
    if length v <> length w then raise Fail "Polynomial.poly_list_sub: length mismatch"
    else ListPair.mapEq sub (v, w)

  fun matNeg (m: i_poly_mat) : i_poly_mat =
    List.map (fn row => List.map neg row) m

  fun matAdd (a: i_poly_mat, b: i_poly_mat) : i_poly_mat =
    if length a <> length b then raise Fail "Polynomial.matAdd: row mismatch"
    else ListPair.mapEq poly_list_add (a, b)

  fun matSub (a: i_poly_mat, b: i_poly_mat) : i_poly_mat =
    if length a <> length b then raise Fail "Polynomial.matSub: row mismatch"
    else ListPair.mapEq poly_list_sub (a, b)

  fun scalar_multiply_row (row: i_poly list, f: i_poly) : i_poly list =
    List.map (fn p => mul (p, f)) row

  fun matScalarMulPoly (f: i_poly, m: i_poly_mat) : i_poly_mat =
    List.map (fn row => scalar_multiply_row (row, f)) m

  fun matScalarMulInt (c: int, m: i_poly_mat) : i_poly_mat =
    let
      fun mulIntPoly p =
        if c = 0 then poly_0 else strip (List.map (fn x => c * x) p)
    in
      List.map (fn row => List.map mulIntPoly row) m
    end

  fun update_row (r: i_poly list, j: int, v: i_poly) : i_poly list =
    List.tabulate (length r, fn k => if k = j then v else List.nth (r, k))

  fun update_matrix_row (m: i_poly_mat, i: int, row: i_poly list) : i_poly_mat =
    List.tabulate (length m, fn k => if k = i then row else List.nth (m, k))
end
