use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/extended_misc.sml";
use "atlas-scripts-sml/AtlasParam.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/polynomial.sml";
use "atlas-scripts-sml/KL_polynomial_matrices.sml";
use "atlas-scripts-sml/tabulate.sml";

(*
  File: atlas-scripts-sml/test_extended.sml

  Purpose
  - SML translation of `atlas-scripts/test_extended.at`.
  - The `.at` script tests the relationship between untwisted KL polynomials
    `P(q,p)` and delta-twisted KL polynomials `P_delta(q,p)` (Hermitian paper,
    Thm 19.4):
      1) |b_j| <= |a_j|
      2) b_j = a_j (mod 2)
    where `P(q,p) = Σ a_j q^j` and `P_delta(q,p) = Σ b_j q^j`.

  Implementation notes
  - Untwisted data comes from `KL_polynomial_matrices.KL_block`.
  - Twisted data comes from `KL_polynomial_matrices.partial_extended_KL_block`.
  - Parameter equality uses `AtlasParam.eq` (Atlas semantic equality).

  Ownership
  - `KL_block` and `partial_extended_KL_block` return cloned parameter handles.
    This module frees those handles internally.
*)

structure Test_extended = struct
  type param = AtlasFFI.param
  type mat = IntMatrix.mat
  type i_poly = Polynomial.i_poly

  fun expect (where': string, b: bool, msg: string) : unit =
    if b then () else raise Fail ("Test_extended." ^ where' ^ ": " ^ msg)

  fun findParam (ps: param list, p: param) : int =
    let
      fun loop ([], _) = ~1
        | loop (q :: qs, i) = if AtlasParam.eq (q, p) then i else loop (qs, i + 1)
    in
      loop (ps, 0)
    end

  fun padTo (xs: int list, n: int) : int list =
    if length xs >= n then xs else xs @ List.tabulate (n - length xs, fn _ => 0)

  fun absInt x = if x < 0 then ~x else x
  fun is_even x = (x mod 2) = 0

  (* comparison_test(poly,poly_delta) from the `.at` script. *)
  fun comparison_test (poly: i_poly, poly_delta: i_poly) : bool =
    let
      val n = Int.max (length poly, length poly_delta)
      val a = padTo (poly, n)
      val b = padTo (poly_delta, n)
      fun ok i =
        let
          val ai = List.nth (a, i)
          val bi = List.nth (b, i)
        in
          absInt bi <= absInt ai andalso is_even (bi - ai)
        end
    in
      List.all ok (List.tabulate (n, fn i => i))
    end

  (* Untwisted polynomial P(q,p). Returns [] if q is not in the block. *)
  fun P_untwisted (q: param, p: param) : i_poly =
    let
      val (B, startPos, Pind, polys) = KL_polynomial_matrices.KL_block p
      val _ = startPos
      val iP = findParam (B, p)
      val iQ = findParam (B, q)
      val res =
        if iP < 0 orelse iQ < 0 then []
        else List.nth (polys, List.nth (List.nth (Pind, iP), iQ))
      val () = List.app AtlasFFI.atlas_param_free B
    in
      res
    end

  (* Twisted polynomial P_delta(q,p) (signed, as returned by the library). *)
  fun P_twisted (q: param, p: param, delta: mat) : i_poly =
    let
      val (B, Pind, polys) = KL_polynomial_matrices.partial_extended_KL_block (p, delta)
      val iP = findParam (B, p)
      val iQ = findParam (B, q)
      val res =
        if iP < 0 orelse iQ < 0 then []
        else List.nth (polys, List.nth (List.nth (Pind, iP), iQ))
      val () = List.app AtlasFFI.atlas_param_free B
    in
      res
    end

  (* test_KL(p,delta) from the `.at` script. *)
  fun test_KL_one (p: param, delta: mat) : (i_poly * i_poly * bool) list * bool =
    if not (Extended_misc.is_fixed (delta, p)) then
      ([([], [], true)], true)
    else
      let
        val (Bdelta, Pdelta, vdelta) = KL_polynomial_matrices.partial_extended_KL_block (p, delta)
        fun one q =
          let
            val poly = P_untwisted (q, p)
            val poly_delta =
              let
                val iP = findParam (Bdelta, p)
                val iQ = findParam (Bdelta, q)
              in
                if iP < 0 orelse iQ < 0 then []
                else List.nth (vdelta, List.nth (List.nth (Pdelta, iP), iQ))
              end
            val ok = comparison_test (poly, poly_delta)
          in
            (poly, poly_delta, ok)
          end

        val results = List.map one Bdelta
        val allOk = List.all #3 results
        val () = List.app AtlasFFI.atlas_param_free Bdelta
      in
        (results, allOk)
      end

  (* Pretty-print the results table, mirroring `.at`. *)
  fun print_results (results: (i_poly * i_poly * bool) list) : unit =
    let
      fun b2s b = if b then "true" else "false"
      val rows =
        List.map
          (fn (c, cd, t) => [Polynomial.poly_format (c, "q"), Polynomial.poly_format (cd, "q"), b2s t])
          results
    in
      Tabulate.tabulate (rows, "lll", 2, " ")
    end

  (* test_KL([params],delta) from `.at`, returning (all, per-param, tests, failedIndices). *)
  fun test_KL_many (params: param list, delta: mat)
    : bool * ((i_poly * i_poly * bool) list * bool) list * bool list * int list =
    let
      fun step (p, i, acc, tests, failed) =
        let
          val (results, ok) = test_KL_one (p, delta)
          val tests' = ok :: tests
          val failed' = if ok then failed else i :: failed
        in
          ((results, ok) :: acc, tests', failed')
        end

      fun loop ([], _, acc, tests, failed) =
        let
          val accR = List.rev acc
          val testsR = List.rev tests
          val failedR = List.rev failed
          val allOk = List.all (fn x => x) testsR
        in
          (allOk, accR, testsR, failedR)
        end
        | loop (p :: ps, i, acc, tests, failed) =
            let
              val (acc', tests', failed') = step (p, i, acc, tests, failed)
            in
              loop (ps, i + 1, acc', tests', failed')
            end
    in
      loop (params, 0, [], [], [])
    end
end
