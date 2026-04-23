use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/combinatorics.sml";
use "atlas-scripts-sml/lazy_lists.sml";

(*
  File: atlas-scripts-sml/exp-generating-series.sml

  Purpose
  - SML translation of `atlas-scripts/exp-generating-series.at`.
  - Implements several operators on exponential generating series (EGFs),
    represented as lazy infinite lists (streams) of integer coefficients:
        f = (a_0, a_1, a_2, ...)
    meaning the formal series  Σ a_n * X^n / n!.

  Representation
  - Uses `Lazy_lists.inf_list` streams of `int` as in `lazy_lists.at`.

  Implemented subset
  - `exp_multiply`: EGF product via binomial-weighted convolution.
  - `exp_divide`: division by a series with constant term 1.
  - `exp_diff`: derivative operator on EGFs (drops constant term).

  Notes / limitations
  - This is intended as a dependency for combinatorial scripts; it uses `int`
    arithmetic and can overflow quickly for nontrivial series.
  - More advanced operators from the `.at` file (substitution, compositional
    inverse, exp-of-series) can be ported later if needed.
*)

structure Exp_generating_series = struct
  structure S = Lazy_lists

  type inf_list = S.inf_list
  type inf_node = S.inf_node

  (* Helper: get nth element from a vector, 0-based. *)
  fun vecNth (v, i) = Vector.sub (v, i)

  (* Binomial-weighted convolution for EGF multiplication:
       (f*g)_n = Σ_{i=0..n} binom(n,i) * f_i * g_{n-i}.
  *)
  fun exp_multiply (f0: inf_list, g0: inf_list) : inf_list =
    let
      val fRef = ref f0
      val gRef = ref g0
      val coefs : (int * int) list ref = ref []
      fun m () : inf_node =
        let
          val n = length (!coefs)
          val S.InfNode (a, f1) = S.force (!fRef)
          val S.InfNode (b, g1) = S.force (!gRef)
          val () = fRef := f1
          val () = gRef := g1
          val () = coefs := (!coefs) @ [(a, b)]

          val v = Vector.fromList (!coefs)
          fun term i =
            let
              val (ai, _) = vecNth (v, i)
              val (_, bj) = vecNth (v, n - i)
              val c = Combinatorics.binom (n, i)
            in
              c * ai * bj
            end
          val out = List.foldl (op +) 0 (List.tabulate (n + 1, term))
        in
          S.InfNode (out, S.InfList m)
        end
    in
      S.memoize (S.InfList m)
    end

  (* Division by a series with constant term 1.
     Solves for q in f = g*q under EGF multiplication. *)
  fun exp_divide (f0: inf_list, g0: inf_list) : inf_list =
    let
      val fRef = ref f0
      val denomRef = ref g0
      val S.InfNode (c0, denomTail) = S.force (!denomRef)
      val () = if c0 = 1 then () else raise Fail "Exp_generating_series.exp_divide: denom constant term != 1"
      val () = denomRef := denomTail

      (* pairs (d_i, q_i) for i>=1 as we compute them; the constant term is implicit *)
      val coefs : (int * int) list ref = ref []
      fun m () : inf_node =
        let
          val S.InfNode (c, f1) = S.force (!fRef)
          val () = fRef := f1
          val n = length (!coefs)

          val subSum =
            if n = 0 then
              0
            else
              let
                val v = Vector.fromList (!coefs)
                fun term i =
                  let
                    val (_, qi) = vecNth (v, i)
                    val (dj, _) = vecNth (v, (n - 1) - i)
                    val coeff = Combinatorics.binom (n, i)
                  in
                    coeff * dj * qi
                  end
              in
                List.foldl (op +) 0 (List.tabulate (n, term))
              end
          val qn = c - subSum

          val S.InfNode (dNext, denom1) = S.force (!denomRef)
          val () = denomRef := denom1
          val () = coefs := (!coefs) @ [(dNext, qn)]
        in
          S.InfNode (qn, S.InfList m)
        end
    in
      S.memoize (S.InfList m)
    end

  (* EGF derivative: drop constant term. *)
  fun exp_diff (f: inf_list) : inf_list =
    let
      val S.InfNode (_, tail) = S.force f
    in
      tail
    end
end
