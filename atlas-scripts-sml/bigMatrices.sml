use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/extended_misc.sml";
use "atlas-scripts-sml/KL_polynomial_matrices.sml";
use "atlas-scripts-sml/polynomial.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(*
  File: atlas-scripts-sml/bigMatrices.sml

  Purpose
  - Partial SML translation of `atlas-scripts/bigMatrices.at`.
  - This file supports “big” KL polynomial matrix computations for extended
    (delta-twisted) blocks, as used in Atlas scripts that lift computations from
    delta-fixed blocks to the full extended parameter set.

  Key idea (from `bigMatrices.at`)
  - Given an ordinary block `B` and an outer automorphism/involution `delta`,
    build an extended indexing set where:
      - each delta-fixed parameter contributes two indices `(i,+)` and `(i,-)`
      - each 2-element delta orbit contributes one “induced” index `(i,0)`
    Then assemble a larger KL P-matrix by combining untwisted and delta-twisted
    P-matrices on appropriate sub-blocks.

  Dependencies
  - `Extended_misc.twist/is_fixed` for twisting parameters by a matrix `delta`.
  - `KL_polynomial_matrices` for:
      - `KL_P_polynomials_B` (untwisted)
      - `KL_P_polynomials_B_twisted` (twisted; delta-fixed block)
  - `Polynomial` for integer polynomial arithmetic and unitriangular inversion.

  Ownership
  - Functions in this module do not free input parameter handles.
  - Any twisting performed internally allocates temporary params that are freed
    immediately.
*)

structure BigMatrices = struct
  type param = AtlasFFI.param
  type mat = IntMatrix.mat
  type i_poly = Polynomial.i_poly
  type i_poly_mat = Polynomial.i_poly_mat

  fun findParam (ps: param list, p: param) : int =
    let
      fun loop ([], _) = ~1
        | loop (q :: qs, i) = if AtlasFFI.atlas_param_equal (p, q) = 1 then i else loop (qs, i + 1)
    in
      loop (ps, 0)
    end

  fun fixed_subset (B: param list, delta: mat) : param list =
    List.filter (fn p => Extended_misc.is_fixed (delta, p)) B

  fun indices_subset (subset: param list, whole: param list) : int list =
    List.map (fn p => findParam (whole, p)) subset

  (* Extract one index from each delta-orbit (same rule as `.at`). *)
  fun delta_orbit_reps (B: param list, delta: mat) : int list =
    let
      fun keep i =
        let
          val p = List.nth (B, i)
          val q = Extended_misc.twist (delta, p)
          val j = findParam (B, q)
          val fixed = AtlasFFI.atlas_param_equal (p, q) = 1
          val () = AtlasFFI.atlas_param_free q
        in
          fixed orelse j < 0 orelse i < j
        end
    in
      List.filter keep (List.tabulate (length B, fn i => i))
    end

  (* Complete extended indices:
       fixed p_i contributes (i,1) and (i,~1)
       non-fixed pair contributes (i,0) for representative i. *)
  fun complete_indices (B: param list, delta: mat) : (int * int) list =
    let
      fun one (p: param, i: int) : (int * int) list =
        let
          val q = Extended_misc.twist (delta, p)
          val j = findParam (B, q)
          val fixed = (j >= 0 andalso AtlasFFI.atlas_param_equal (p, q) = 1)
          val () = AtlasFFI.atlas_param_free q
        in
          if fixed then [(i, 1), (i, ~1)] else if i < j then [(i, 0)] else []
        end
    in
      List.concat (List.tabulate (length B, fn i => one (List.nth (B, i), i)))
    end

  (* Tabulate indices for a block with respect to delta.

     Returns:
     - `I_delta`: indices in `B` of delta-fixed parameters, in order.
     - `g`: array mapping `i in [0..|B|)` to position in `I_delta` or ~1.
     - `complete`: full extended indices (pairs (i,epsilon)).
     - `delta_action`: array mapping `i` to index of `twist(delta,B[i])` in `B`.
  *)
  fun tabulate_indices (B: param list, delta: mat) : int list * int list * (int * int) list * int list =
    let
      val B_delta = fixed_subset (B, delta)
    in
      if null B_delta then
        ([], [], [], [])
      else
        let
          val I_delta = indices_subset (B_delta, B)
          val g = Array.array (length B, ~1)
          fun fill ([], _) = ()
            | fill (i :: is, pos) = (Array.update (g, i, pos); fill (is, pos + 1))
          val () = fill (I_delta, 0)
          val complete = complete_indices (B, delta)
          fun actOne p =
            let
              val q = Extended_misc.twist (delta, p)
              val j = findParam (B, q)
              val () = AtlasFFI.atlas_param_free q
            in
              j
            end
          val delta_action = List.map actOne B
        in
          (I_delta, Array.foldr (op ::) [] g, complete, delta_action)
        end
    end

  (* Exact divide by 2 for integer polynomials. *)
  fun poly_div2 (p: i_poly) : i_poly =
    let
      val p = Polynomial.strip p
      val () = if List.all (fn c => c mod 2 = 0) p then () else raise Fail "BigMatrices: poly_div2 not exact"
    in
      Polynomial.strip (List.map (fn c => c div 2) p)
    end

  fun poly_add (f: i_poly, g: i_poly) : i_poly = Polynomial.add (f, g)
  fun poly_sub (f: i_poly, g: i_poly) : i_poly = Polynomial.sub (f, g)

  fun zero_square (n: int) : i_poly_mat = Polynomial.zero_poly_matrix (n, n)

  fun big_KL_P_polynomials (B: param list, delta: mat) : i_poly_mat =
    let
      val (I_delta, gmap, complete, delta_action) = tabulate_indices (B, delta)
      val B_delta = List.map (fn i => List.nth (B, i)) I_delta
      val P = KL_polynomial_matrices.KL_P_polynomials_B B
      val () = if null I_delta then () else ()
      val P_delta =
        if null I_delta then
          ([]: i_poly_mat)
        else
          KL_polynomial_matrices.KL_P_polynomials_B_twisted (B_delta, delta)

      val () =
        if null I_delta then
          () (* fall back below *)
        else if length P_delta = 0 then
          raise Fail "big_KL_P_polynomials: twisted P_delta unexpectedly empty"
        else
          ()

      val n = length complete
      val result = Array.tabulate (n, fn _ => Array.array (n, Polynomial.poly_0))

      fun put (i: int, j: int, p: i_poly) =
        (Array.update (Array.sub (result, i), j, p))

      fun getG i = List.nth (gmap, i)
      fun entryP (i, j) = List.nth (List.nth (P, i), j)
      fun entryPdelta (x, y) = List.nth (List.nth (P_delta, x), y)

      fun loopA a =
        if a = n then ()
        else
          let
            val (i, eps_i) = List.nth (complete, a)
            fun loopB b =
              if b = n then ()
              else
                let
                  val (j, eps_j) = List.nth (complete, b)
                in
                  if a = b then
                    put (a, b, Polynomial.poly_1)
                  else if eps_i * eps_j = 1 then
                    let
                      val x = getG i
                      val y = getG j
                      val v = poly_div2 (poly_add (entryP (i, j), entryPdelta (x, y)))
                    in
                      put (a, b, v)
                    end
                  else if eps_i * eps_j = ~1 then
                    let
                      val x = getG i
                      val y = getG j
                      val v = poly_div2 (poly_sub (entryP (i, j), entryPdelta (x, y)))
                    in
                      put (a, b, v)
                    end
                  else if Int.abs eps_i = 1 andalso eps_j = 0 then
                    put (a, b, entryP (i, j))
                  else if eps_i = 0 andalso Int.abs eps_j = 1 then
                    put (a, b, entryP (i, j))
                  else if eps_i = 0 andalso eps_j = 0 then
                    let
                      val dj = List.nth (delta_action, j)
                      val () =
                        if dj < 0 then
                          raise Fail "big_KL_P_polynomials: delta_action lookup failed"
                        else
                          ()
                      val v = poly_add (entryP (i, j), entryP (i, dj))
                    in
                      put (a, b, v)
                    end
                  else
                    ()
                  ; loopB (b + 1)
                end
          in
            loopB a;
            loopA (a + 1)
          end

      val () =
        if null I_delta then
          () (* no delta-fixed subset => return untwisted P below *)
        else
          loopA 0
    in
      if null I_delta then
        P
      else
        Array.foldr (fn (row, acc) => (Array.foldr (op ::) [] row) :: acc) [] result
    end

  fun big_KL_P_polynomials_at_minus_one (B: param list, delta: mat) : int list list =
    KL_polynomial_matrices.eval (big_KL_P_polynomials (B, delta), ~1)

  fun big_KL_P_signed_polynomials (B: param list, delta: mat) : i_poly_mat =
    let
      val (I_delta, gmap, complete, _) = tabulate_indices (B, delta)
      val () = if null I_delta then () else ()
      val P = big_KL_P_polynomials (B, delta)
      val lengths = List.map (fn p => AtlasFFI.atlas_param_length p) B
      val n = length complete
      fun sign i j = if (List.nth (lengths, i) + List.nth (lengths, j)) mod 2 = 0 then 1 else ~1
      fun entry a b =
        let
          val (i, _) = List.nth (complete, a)
          val (j, _) = List.nth (complete, b)
          val p = List.nth (List.nth (P, a), b)
        in
          if sign i j = 1 then p else Polynomial.neg p
        end
    in
      List.tabulate (n, fn a => List.tabulate (n, fn b => entry a b))
    end

  fun is_non_negative_poly (p: i_poly) : bool =
    List.all (fn c => c >= 0) (Polynomial.strip p)

  fun is_non_negative_list (ps: i_poly list) : bool = List.all is_non_negative_poly ps

  fun is_non_negative_mat (m: i_poly_mat) : bool = List.all is_non_negative_list m

  fun big_KL_Q_polynomials (B: param list, delta: mat) : i_poly_mat =
    let
      val () = TextIO.print ("computing big_KL_Q polynomials for B, #B=" ^ Int.toString (length B) ^ "\n")
      val P = big_KL_P_signed_polynomials (B, delta)
      val M = Polynomial.upper_unitriangular_inverse P
    in
      if is_non_negative_mat M then
        M
      else
        raise Fail "big_KL_Q_polynomials: result not non-negative"
    end

  (* ---------- caching layer (safe keying) ---------- *)

  val done_keys : string list ref = ref []
  val p_matrices : i_poly_mat list ref = ref []

  fun param_key (p: param) : string =
    Int.toString (AtlasFFI.atlas_param_x p) ^ "|" ^ AtlasFFI.atlas_param_lambda_text p ^ "|" ^ AtlasFFI.atlas_param_nu_text p

  fun findKey (k: string, ks: string list) : int =
    let
      fun loop ([], _) = ~1
        | loop (x :: xs, i) = if x = k then i else loop (xs, i + 1)
    in
      loop (ks, 0)
    end

  fun calculate_big_P_signed_polynomials (B: param list, delta: mat, index: int) : i_poly_mat =
    let
      val p = List.nth (B, index)
      val k = param_key p
      val i = findKey (k, !done_keys)
    in
      if i >= 0 then
        List.nth (!p_matrices, i)
      else
        let
          val P = big_KL_P_signed_polynomials (B, delta)
          val () = done_keys := k :: !done_keys
          val () = p_matrices := P :: !p_matrices
        in
          P
        end
    end

  fun calculate_big_P_signed_polynomials_at_minus_one (B: param list, delta: mat, index: int) : int list list =
    KL_polynomial_matrices.eval (calculate_big_P_signed_polynomials (B, delta, index), ~1)
end
