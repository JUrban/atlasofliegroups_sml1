use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/partitions.sml";
use "atlas-scripts-sml/combinatorics.sml";

(*
  File: atlas-scripts-sml/W_reps_type_BC.sml

  Purpose
  - Partial SML port of `atlas-scripts/W_reps_type_BC.at`.

  Background
  - In types B/C, nilpotent Jordan types (partitions with parity constraints)
    correspond to Weyl-group irreps (bipartitions) via the 2-core/2-quotient.
  - Lusztig symbols are an alternate combinatorial parameterization that makes
    the notion of “special” representations particularly simple.

  What this file provides today (pure combinatorics)
  - Conversions:
      - `wrep_C` / `wrep_B` : Orbit partition -> bipartition (H_n rep)
      - `orbit_C_from_rep` / `orbit_B_from_rep` : bipartition -> Orbit option
      - `symbol_of_orbit` : Orbit -> Symbol
      - `orbit_C_from_symbol` / `orbit_B_from_symbol` : Symbol -> Orbit option
  - Predicates:
      - `is_special_symbol`, `is_special_rep`, `is_special_orbit_C`,
        `is_special_orbit_B`
  - Sizes:
      - `dimension_rep` : bipartition -> IntInf.int (hook-length formula)

  Scope / limitations
  - The original `.at` file also connects these combinatorics to Atlas
    `RootDatum`, `nilpotent_orbits`, `W_rep`, and cell/tau-invariant machinery.
    Those Atlas-facing pieces are not yet ported here.

  Status
  - This is an incremental port focused on reusable pure functions.
*)

structure W_reps_type_BC = struct
  structure B = Basic
  structure P = Partitions
  structure Cb = Combinatorics

  type Orbit = P.partition
  type Hn_rep = Cb.bipartition
  type Symbol = Cb.symbol

  fun fail msg = raise Fail ("W_reps_type_BC: " ^ msg)

  fun sumInts (xs: int list) : int = List.foldl (op +) 0 xs

  (* Histogram-style frequencies:
       freq[i] = multiplicity of value i in the list. *)
  fun value_frequencies (xs: int list) : int list =
    let
      val maxVal = List.foldl (fn (x, acc) => Int.max (x, acc)) 0 xs
      val () = if List.exists (fn x => x < 0) xs then fail "value_frequencies: negative value" else ()
      val a = Array.array (maxVal + 1, 0)
      val () = List.app (fn x => Array.update (a, x, Array.sub (a, x) + 1)) xs
    in
      Array.foldr (op ::) [] a
    end

  fun partition_frequencies (p: Orbit) : int list = value_frequencies (Cb.strip_to_partition p)

  fun is_valid_C (p0: Orbit) : bool =
    let
      val p = Cb.strip_to_partition p0
      val freq = partition_frequencies p
      val n = length freq
      fun freqAt i = if i < 0 orelse i >= n then 0 else List.nth (freq, i)
      fun ok i = if i mod 2 = 1 then freqAt i mod 2 = 0 else true
    in
      sumInts p mod 2 = 0 andalso List.all ok (List.tabulate (n, fn i => i))
    end

  fun is_valid_B (p0: Orbit) : bool =
    let
      val p = Cb.strip_to_partition p0
      val freq = partition_frequencies p
      val n = length freq
      fun freqAt i = if i < 0 orelse i >= n then 0 else List.nth (freq, i)
      fun evenOk i = if i mod 2 = 0 then freqAt i mod 2 = 0 else true
      fun oddCount i acc =
        if i >= n then acc
        else if i mod 2 = 1 then oddCount (i + 1) (acc + freqAt i) else oddCount (i + 1) acc
      val odds = oddCount 0 0
    in
      sumInts p mod 2 = 1 andalso List.all evenOk (List.tabulate (n, fn i => i)) andalso odds mod 2 = 1
    end

  (* Rank of a Lusztig symbol row: sum(row) - binom(len,2). *)
  fun symbol_ranks ((f, g): Symbol) : int * int =
    let
      fun binom2 k = k * (k - 1) div 2
    in
      (sumInts f - binom2 (length f), sumInts g - binom2 (length g))
    end

  (* Orbit -> bipartition (Weyl-group irrep parameter). *)
  fun wrep_C (p0: Orbit) : Hn_rep =
    let
      val p = Cb.strip_to_partition p0
      val (d, (lambda, mu)) = Cb.core_quotient_2 p
      val () = if d = 0 then () else ()
    in
      (mu, lambda) (* swap for type C *)
    end

  fun wrep_B (p0: Orbit) : Hn_rep =
    let
      val p = Cb.strip_to_partition p0
      val (d, pair) = Cb.core_quotient_2 p
      val () = if d = 1 then () else ()
    in
      pair
    end

  (* Orbit -> Symbol (normalized) as in the `.at` file. *)
  fun symbol_of_orbit (p0: Orbit) : Symbol =
    let
      val p = Cb.strip_to_partition p0
      val typeC = (sumInts p) mod 2 = 0
      val p' = if typeC andalso (length p) mod 2 = 0 then p @ [0] else p
      val e = List.tabulate (length p', fn i => List.nth (p', i) + i)
      val f = List.map (fn x => x div 2) (List.filter (fn x => x mod 2 = 1) e)
      val g = List.map (fn x => x div 2) (List.filter (fn x => x mod 2 = 0) e)
    in
      if typeC then (g, f) else (f, g)
    end

  (* Symbol -> Orbit, via the shared merger routine from the `.at` file.
     Returns `NONE` when the input is not a valid (normalized) symbol. *)
  fun orbit_from_symbol_common (t0: int list, t1: int list) : Orbit option =
    if length t0 <> length t1 + 1 then
      NONE
    else
      let
        val t0a = Array.fromList t0
        val t1a = Array.fromList t1
        val l = Array.length t1a

        fun step i =
          if i >= l then
            true
          else
            let
              val a = Array.sub (t0a, i)
              val b = Array.sub (t1a, i)
              val c = Array.sub (t0a, i + 1)
              val () =
                if a > b then
                  if a = b + 1 then (Array.update (t0a, i, b); Array.update (t1a, i, b + 1))
                  else raise Fail "bad"
                else if b > c then
                  if b - 1 = c then (Array.update (t0a, i + 1, b); Array.update (t1a, i, b - 1))
                  else raise Fail "bad"
                else
                  ()
            in
              step (i + 1)
            end
      in
        let
          val ok = (step 0 handle Fail _ => false)
        in
          if not ok then
            NONE
          else
            let
              val a = Array.foldr (op ::) [] t0a
              val b = Array.foldr (op ::) [] t1a
              val parts =
                List.tabulate
                  (length a + length b, fn j =>
                     if j mod 2 = 0 then List.nth (a, j div 2) - j
                     else List.nth (b, j div 2) - j)
              val partsRev = List.rev parts
            in
              SOME (Cb.strip_to_partition partsRev)
            end
        end
      end

  fun orbit_C_from_symbol (s: Symbol) : Orbit option =
    let
      val (f, g) = s
      val t0 = List.map (fn x => 2 * x) f
      val t1 = List.map (fn x => 2 * x + 1) g
    in
      orbit_from_symbol_common (t0, t1)
    end

  fun orbit_B_from_symbol (s: Symbol) : Orbit option =
    let
      val (f, g) = s
      val t0 = List.map (fn x => 2 * x + 1) f
      val t1 = List.map (fn x => 2 * x) g
    in
      orbit_from_symbol_common (t0, t1)
    end

  (* bipartition -> Orbit, with type-dependent core number.
     Returns `NONE` when the result fails the Jordan-type parity test. *)
  fun orbit_C_from_rep ((lambda, mu): Hn_rep) : Orbit option =
    let
      val orbit = Cb.from_core_quotient_2 (0, (mu, lambda))
    in
      if is_valid_C orbit then SOME orbit else NONE
    end

  fun orbit_B_from_rep (pair: Hn_rep) : Orbit option =
    let
      val orbit = Cb.from_core_quotient_2 (1, pair)
    in
      if is_valid_B orbit then SOME orbit else NONE
    end

  fun is_special_symbol (s: Symbol) : bool = Cb.is_special_symbol s

  (* Type-B/C “specialness” predicate directly on bipartitions, as in the `.at`
     file (equivalent to specialness of the associated normalized symbol). *)
  fun is_special_rep ((nu0, mu0): Hn_rep) : bool =
    let
      val nu = Cb.strip_to_partition nu0
      val mu = Cb.strip_to_partition mu0
      val d = length mu + 1 - length nu
      val () = if d = 0 then () else raise Fail "not special"
      fun nuAt i = List.nth (nu, i)
      fun muAt i = List.nth (mu, i)
      fun ok i = (nuAt i + 1 >= muAt i) andalso (muAt i >= List.nth (nu, i + 1))
    in
      List.all ok (List.tabulate (length mu, fn i => i))
      handle Fail _ => false
    end

  fun is_special_orbit_C (p: Orbit) : bool = is_special_rep (wrep_C p)
  fun is_special_orbit_B (p: Orbit) : bool = is_special_rep (wrep_B p)

  (* Dimension for the hyperoctahedral irrep indexed by a bipartition. *)
  fun dimension_rep ((p, q): Hn_rep) : IntInf.int =
    let
      val dp = P.dim_rep p
      val dq = P.dim_rep q
      val n = sumInts p + sumInts q
      val k = sumInts q
    in
      dp * dq * IntInf.fromInt (Cb.binom (n, k))
    end
end
