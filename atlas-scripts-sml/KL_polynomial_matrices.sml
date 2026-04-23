use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/polynomial.sml";
use "atlas-scripts-sml/ParamBlocks.sml";
use "atlas-scripts-sml/representations.sml";

(*
  File: atlas-scripts-sml/KL_polynomial_matrices.sml

  Purpose
  - Partial SML translation of `atlas-scripts/KL_polynomial_matrices.at`.
  - Exposes Kazhdan–Lusztig P-polynomial matrices for a block (or a provided
    list of parameters), using a small C++ FFI helper that computes the
    condensed KL-polynomial index matrix plus a polynomial pool.

  Atlas correspondence
  - The `.at` function `KL_block(p)` returns a 4-tuple
      (block, start_index, P_index_matrix, polys)
    where `P_index_matrix[i][j]` is an index into `polys`, and `polys[k]` is a
    dense coefficient vector (an `i_poly` in `.at`).
  - We expose `KL_P_polynomials` / `KL_P_signed_polynomials` and evaluation
    helpers matching the `.at` API.

  Implementation notes
  - The Atlas interpreter’s `KL_block` is implemented in the interpreter layer;
    this repo’s Poly/ML FFI does not link the interpreter. Instead we compute
    the same data directly from the C++ `blocks::common_block` / `kl::KL_table`
    APIs inside the shim function `atlas_param_KL_block_data_text`.
  - We currently define `KL_Q_polynomials` as the inverse of the signed P-matrix
    (`Polynomial.upper_unitriangular_inverse`). This matches the twisted
    fallback in `KL_polynomial_matrices.at`, but may differ from the dedicated
    `dual_KL_block` implementation used by `.at` for untwisted Q.

  Ownership
  - This module does not allocate new parameter handles; it only reads from
    existing ones. Callers remain responsible for freeing `AtlasFFI.param`
    handles they own.
*)

structure KL_polynomial_matrices = struct
  type param = AtlasFFI.param
  type i_poly = Polynomial.i_poly
  type i_poly_mat = Polynomial.i_poly_mat
  type mat = int list list

  fun parseInts (s: string) : int list =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("KL_polynomial_matrices: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun last (xs: 'a list) : 'a =
    (case xs of
       [] => raise Fail "KL_polynomial_matrices.last: empty"
     | [x] => x
     | _ :: ys => last ys)

  fun reshapeRows (n: int, flat: 'a list) : 'a list list =
    let
      val () = if n < 0 then raise Fail "KL_polynomial_matrices.reshapeRows: negative n" else ()
      val need = n * n
      val () = if length flat = need then () else raise Fail "KL_polynomial_matrices.reshapeRows: wrong length"
      fun row i = List.take (List.drop (flat, i * n), n)
    in
      List.tabulate (n, row)
    end

  (* Parse `atlas_param_KL_block_data_text` output into:
       (n, start_pos, P_index_matrix, polys)
     where polys are dense coefficient lists. *)
  fun parse_KL_block_data (text: string) : int * int * int list list * i_poly list =
    let
      val xs = parseInts text
      fun take (k, ys) = (List.take (ys, k), List.drop (ys, k))
    in
      case xs of
        n :: startPos :: rest =>
          let
            val (matFlat, rest1) = take (n * n, rest)
            val P = reshapeRows (n, matFlat)
          in
            (case rest1 of
               m :: rest2 =>
                 let
                   fun readPolys (0, ys, acc) = (List.rev acc, ys)
                     | readPolys (k, ys, acc) =
                         (case ys of
                            len :: zs =>
                              let
                                val (coeffs, zs') = take (len, zs)
                              in
                                readPolys (k - 1, zs', (coeffs: i_poly) :: acc)
                              end
                          | _ => raise Fail "KL_polynomial_matrices: truncated polys")
                   val (polys, leftover) = readPolys (m, rest2, [])
                   val () = if null leftover then () else raise Fail "KL_polynomial_matrices: trailing data"
                 in
                   (n, startPos, P, polys)
                 end
             | _ => raise Fail "KL_polynomial_matrices: truncated header")
          end
      | _ => raise Fail "KL_polynomial_matrices: truncated KL_block data"
    end

  (* Evaluate a polynomial matrix at `k`. *)
  fun eval (m: i_poly_mat, k: int) : mat =
    List.map (fn row => List.map (fn p => Polynomial.eval_int (p, k)) row) m

  (* Compute `block` (survivors in the full block), plus KL index data. *)
  fun KL_block (p: param) : param list * int * int list list * i_poly list =
    let
      val (terms, _) = ParamBlocks.block_survivors p
      val block = List.map #1 terms
      val text = AtlasFFI.atlas_param_KL_block_data_text p
      val () =
        if text = "-1" then
          raise Fail ("KL_polynomial_matrices.KL_block: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val (n, startPos, P, polys) = parse_KL_block_data text
      val () =
        if length block <> n then
          raise Fail
            ("KL_polynomial_matrices.KL_block: block size mismatch: block_of="
             ^ Int.toString (length block)
             ^ " KL_data="
             ^ Int.toString n)
        else
          ()
    in
      (* caller owns the cloned params in `block` *)
      (block, startPos, P, polys)
    end

  (* Map from index in B1 to matching index in B2, and whether it's well-defined. *)
  fun permutation (B1: param list, B2: param list) : int list * bool =
    let
      val n2 = length B2
      val buckets = Array.array (Int.max (1, 2 * n2 + 1), ([]: (param * int) list))
      val modN = Array.length buckets

      fun bucketIndex q =
        let
          val h = AtlasFFI.atlas_param_hash (q, 1000003)
          val i = Int.abs h mod modN
        in
          i
        end

      fun addAll ([], _) = ()
        | addAll (q :: qs, i) =
            let
              val b = bucketIndex q
              val cur = Array.sub (buckets, b)
              val () = Array.update (buckets, b, (q, i) :: cur)
            in
              addAll (qs, i + 1)
            end
      val () = addAll (B2, 0)

      fun findIndex p =
        let
          val b = bucketIndex p
          fun loop [] = NONE
            | loop ((q, i) :: rest) =
                if AtlasFFI.atlas_param_equal (p, q) = 1 then SOME i else loop rest
        in
          loop (Array.sub (buckets, b))
        end

      fun loop ([], acc) = (List.rev acc, true)
        | loop (p :: ps, acc) =
            (case findIndex p of
               NONE => ([], false)
             | SOME i => loop (ps, i :: acc))
    in
      loop (B1, [])
    end

  fun KL_P_polynomials (p: param) : i_poly_mat =
    let
      val text = AtlasFFI.atlas_param_KL_block_data_text p
      val () =
        if text = "-1" then raise Fail ("KL_P_polynomials: failed: " ^ AtlasFFI.atlas_last_error ()) else ()
      val (_, _, P, polys) = parse_KL_block_data text
      fun polyAt idx =
        if idx < 0 orelse idx >= length polys then
          raise Fail ("KL_P_polynomials: bad poly index " ^ Int.toString idx)
        else
          List.nth (polys, idx)
    in
      List.map (fn row => List.map polyAt row) P
    end

  fun KL_P_polynomials_at_minus_one (p: param) : mat =
    eval (KL_P_polynomials p, ~1)

  fun KL_P_signed_polynomials (p: param) : i_poly_mat =
    let
      val text = AtlasFFI.atlas_param_KL_block_data_text p
      val () =
        if text = "-1" then raise Fail ("KL_P_signed_polynomials: failed: " ^ AtlasFFI.atlas_last_error ()) else ()
      val (n, _, P, polys) = parse_KL_block_data text
      val (terms, _) = ParamBlocks.block_survivors p
      val block = List.map #1 terms
      val () =
        if length block <> n then
          (ParamBlocks.freeTerms terms;
           raise Fail "KL_P_signed_polynomials: block survivor size mismatch")
        else
          ()
      val lens = List.map (fn q => AtlasFFI.atlas_param_length q) block
      fun sign (i: int, j: int) : int =
        let
          val d = List.nth (lens, j) - List.nth (lens, i)
        in
          if d mod 2 = 0 then 1 else ~1
        end
      fun polyAt idx = List.nth (polys, idx)
      fun entry (i, j) =
        let
          val p = polyAt (List.nth (List.nth (P, i), j))
        in
          if sign (i, j) = 1 then p else Polynomial.neg p
        end
    in
      (List.tabulate (n, fn i => List.tabulate (n, fn j => entry (i, j)))
       before ParamBlocks.freeTerms terms)
    end

  fun KL_P_signed_polynomials_at_minus_one (p: param) : mat =
    eval (KL_P_signed_polynomials p, ~1)

  fun KL_P_polynomials_B (B: param list) : i_poly_mat =
    let
      val () = if null B then raise Fail "KL_P_polynomials: empty B" else ()
      val p0 = last B
      val text = AtlasFFI.atlas_param_KL_block_data_text p0
      val () =
        if text = "-1" then raise Fail ("KL_P_polynomials(B): failed: " ^ AtlasFFI.atlas_last_error ()) else ()
      val (n, _, P, polys) = parse_KL_block_data text
      val (terms, _) = ParamBlocks.block_survivors p0
      val block = List.map #1 terms
      val () =
        if length block <> n then
          (ParamBlocks.freeTerms terms;
           raise Fail "KL_P_polynomials(B): block survivor size mismatch")
        else
          ()
      val (perm, ok) = permutation (B, block)
      val () =
        if ok then () else (ParamBlocks.freeTerms terms; raise Fail "KL_P_polynomials(B): B does not agree with block")
      fun at i j = List.nth (List.nth (P, List.nth (perm, i)), List.nth (perm, j))
      fun polyAt idx = List.nth (polys, idx)
    in
      (List.tabulate (n, fn i => List.tabulate (n, fn j => polyAt (at i j)))
       before ParamBlocks.freeTerms terms)
    end

  fun KL_P_polynomials_at_minus_one_B (B: param list) : mat =
    eval (KL_P_polynomials_B B, ~1)

  fun KL_P_signed_polynomials_B (B: param list) : i_poly_mat =
    let
      val () = if null B then raise Fail "KL_P_signed_polynomials: empty B" else ()
      val p0 = last B
      val text = AtlasFFI.atlas_param_KL_block_data_text p0
      val () =
        if text = "-1" then raise Fail ("KL_P_signed_polynomials(B): failed: " ^ AtlasFFI.atlas_last_error ()) else ()
      val (n, _, P, polys) = parse_KL_block_data text
      val (terms, _) = ParamBlocks.block_survivors p0
      val block = List.map #1 terms
      val () =
        if length block <> n then
          (ParamBlocks.freeTerms terms;
           raise Fail "KL_P_signed_polynomials(B): block survivor size mismatch")
        else
          ()
      val (perm, ok) = permutation (B, block)
      val () =
        if ok then () else (ParamBlocks.freeTerms terms; raise Fail "KL_P_signed_polynomials(B): B does not agree with block")
      val lens = List.map (fn q => AtlasFFI.atlas_param_length q) block
      fun sign (i: int, j: int) : int =
        let
          val d = List.nth (lens, j) - List.nth (lens, i)
        in
          if d mod 2 = 0 then 1 else ~1
        end
      fun polyAt idx = List.nth (polys, idx)
      fun idxAt i j = List.nth (List.nth (P, List.nth (perm, i)), List.nth (perm, j))
      fun entry (i, j) =
        let
          val p = polyAt (idxAt i j)
        in
          if sign (List.nth (perm, i), List.nth (perm, j)) = 1 then p else Polynomial.neg p
        end
    in
      (List.tabulate (n, fn i => List.tabulate (n, fn j => entry (i, j)))
       before ParamBlocks.freeTerms terms)
    end

  fun KL_P_signed_polynomials_at_minus_one_B (B: param list) : mat =
    eval (KL_P_signed_polynomials_B B, ~1)

  (* Untwisted Q: currently computed as inverse of signed P. *)
  fun KL_Q_polynomials (p: param) : i_poly_mat =
    Polynomial.upper_unitriangular_inverse (KL_P_signed_polynomials p)

  fun KL_Q_polynomials_B (B: param list) : i_poly_mat =
    Polynomial.upper_unitriangular_inverse (KL_P_signed_polynomials_B B)
end
