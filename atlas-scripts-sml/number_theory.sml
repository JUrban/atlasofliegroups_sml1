use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/lazy_lists.sml";

(*
  File: atlas-scripts-sml/number_theory.sml

  Purpose
  - Partial SML translation of `atlas-scripts/number_theory.at`.
  - Provides basic number-theoretic utilities used by some Atlas scripts:
      - a lazy stream of primes
      - trial-division factorization
      - Euler phi
      - Bezout coefficients and modular inversion
      - modular exponentiation

  Scope / limitations
  - This is intentionally incremental:
      - The candidate-prime sieve utilities in the `.at` file depend on
        bitset helpers that are not yet ported.
      - The cyclotomic polynomial routines depend on `polynomial.at` and are
        not included here.
  - All computations are on `int` and may overflow for large inputs; internal
    intermediate products use `IntInf` where that is easy to do safely.
*)

structure Number_theory = struct
  structure Stream = Lazy_lists

  (* ---------- basic arithmetic ---------- *)

  fun gcd (a: int, b: int) : int =
    let
      val a = Int.abs a
      val b = Int.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, x mod y)
    in
      if a = 0 then b else loop (a, b)
    end

  fun powInt (x: int, k: int) : int =
    if k < 0 then raise Fail "Number_theory.powInt: negative exponent"
    else
      let
        fun loop (acc, base, e) =
          if e = 0 then acc
          else if e mod 2 = 1 then loop (acc * base, base * base, e div 2)
          else loop (acc, base * base, e div 2)
      in
        loop (1, x, k)
      end

  fun isPrimeTrial (n: int) : bool =
    if n < 2 then false
    else if n = 2 then true
    else if n mod 2 = 0 then false
    else
      let
        fun loop d =
          if IntInf.fromInt d * IntInf.fromInt d > IntInf.fromInt n then true
          else if n mod d = 0 then false
          else loop (d + 2)
      in
        loop 3
      end

  (* ---------- primes (lazy stream) ---------- *)

  fun nextPrimeFromOdd (m0: int) : int =
    let
      val m0 = if m0 <= 3 then 3 else if m0 mod 2 = 0 then m0 + 1 else m0
      fun loop m = if isPrimeTrial m then m else loop (m + 2)
    in
      loop m0
    end

  val primes : Stream.inf_list =
    let
      fun genOdd m () : Stream.inf_node =
        let
          val p = nextPrimeFromOdd m
        in
          Stream.InfNode (p, Stream.InfList (genOdd (p + 2)))
        end
      val raw = Stream.InfList (fn () => Stream.InfNode (2, Stream.InfList (genOdd 3)))
    in
      Stream.memoize raw
    end

  (* ---------- factorization / divisors / phi ---------- *)

  (* Factorization of nonzero n as [(p,e),...], with p positive primes. *)
  fun factorization (n0: int) : (int * int) list =
    let
      val () = if n0 = 0 then raise Fail "Number_theory.factorization: cannot factor 0" else ()
      val n0 = Int.abs n0
      fun loop (n: int, ps: Stream.inf_list, acc: (int * int) list) =
        if n = 1 then
          List.rev acc
        else
          let
            val Stream.InfNode (p, psTail) = Stream.force ps
            val p2 = IntInf.fromInt p * IntInf.fromInt p
          in
            if p2 > IntInf.fromInt n then
              List.rev ((n, 1) :: acc)
            else
              let
                fun countPow (m, c) =
                  if m mod p <> 0 then (m, c) else countPow (m div p, c + 1)
                val (n', c) = countPow (n, 0)
              in
                if c = 0 then loop (n, psTail, acc) else loop (n', psTail, (p, c) :: acc)
              end
          end
    in
      loop (n0, primes, [])
    end

  (* Find prime factors up to `limit` and return (factors, quotient).
     The quotient may be 1 or composite, matching `easy_factors` in `.at`. *)
  fun easy_factors (n0: int, limit: int) : (int * int) list * int =
    let
      val () = if n0 = 0 then raise Fail "Number_theory.easy_factors: cannot factor 0" else ()
      val () = if limit < 2 then raise Fail "Number_theory.easy_factors: limit < 2" else ()
      val n0 = Int.abs n0
      fun loop (n: int, ps: Stream.inf_list, acc: (int * int) list) =
        if n = 1 then
          (List.rev acc, 1)
        else
          let
            val Stream.InfNode (p, psTail) = Stream.force ps
          in
            if p > limit orelse IntInf.fromInt p * IntInf.fromInt p > IntInf.fromInt n then
              (* Mirror `.at` behavior: if remainder is prime and <=limit we include it. *)
              if n = 1 then (List.rev acc, 1)
              else if p <= limit andalso isPrimeTrial n then (List.rev ((n, 1) :: acc), 1)
              else (List.rev acc, n)
            else
              let
                fun countPow (m, c) =
                  if m mod p <> 0 then (m, c) else countPow (m div p, c + 1)
                val (n', c) = countPow (n, 0)
              in
                if c = 0 then loop (n, psTail, acc) else loop (n', psTail, (p, c) :: acc)
              end
          end
    in
      loop (n0, primes, [])
    end

  (* Prime divisors of n (no multiplicity). *)
  fun prime_divisors (n: int) : int list =
    List.map #1 (factorization n)

  (* All divisors (unsorted). *)
  fun divisors (n: int) : int list =
    let
      fun extend (ds: int list, p: int, c: int) : int list =
        let
          fun powLoop (0, pow, acc) = acc
            | powLoop (k, pow, acc) =
                powLoop (k - 1, pow * p, acc @ List.map (fn d => pow * p * d) ds)
        in
          powLoop (c, 1, ds)
        end
    in
      List.foldl (fn ((p, c), acc) => extend (acc, p, c)) [1] (factorization n)
    end

  fun invertibles_modulo (n: int) : int list =
    let
      val n = Int.abs n
      val () = if n = 0 then raise Fail "Number_theory.invertibles_modulo: modulus 0" else ()
    in
      List.filter (fn i => gcd (i, n) = 1) (List.tabulate (n, fn i => i))
    end

  (* Euler totient function for n > 0. *)
  fun phi (n: int) : int =
    if n <= 0 then raise Fail "Number_theory.phi: expects n>0"
    else
      let
        fun step ((p, c), acc) =
          acc * (p - 1) * powInt (p, c - 1)
      in
        List.foldl step 1 (factorization n)
      end

  (* ---------- Bezout / modular arithmetic ---------- *)

  (* (d,s) where d = gcd(a,b) and s*a ≡ d (mod b), with 0 <= s < b when b>0. *)
  fun gcd_Bezout_coef (a0: int, b0: int) : int * int =
    if b0 <= 0 then raise Fail "Number_theory.gcd_Bezout_coef: expects b>0"
    else
      let
        val a0 = IntInf.fromInt a0
        val b0 = IntInf.fromInt b0
        fun loop (s0, s1, r0, r1) =
          if r0 = 0 then (r1, s1)
          else
            let
              val (q, r) = IntInf.divMod (r1, r0)
              val s = s1 - q * s0
            in
              loop (s, s0, r, r0)
            end

        val (d, s) = loop (1, 0, a0, b0)
        val s = if s < 0 then s + b0 else s
      in
        (IntInf.toInt d, IntInf.toInt s)
      end

  (* (d,s,t) with d = s*a + t*b. *)
  fun Bezout (a: int, b: int) : int * int * int =
    let
      val (d, s) = gcd_Bezout_coef (a, b)
      val aI = IntInf.fromInt a
      val bI = IntInf.fromInt b
      val sI = IntInf.fromInt s
      val (q, r) = IntInf.divMod (sI * aI, bI)
      val () = if IntInf.toInt r = d then () else raise Fail "Number_theory.Bezout: internal mismatch"
      val t = IntInf.toInt (~q)
    in
      (d, s, t)
    end

  fun inverse_mod (a: int, n: int) : int =
    let
      val () = if n <= 0 then raise Fail "Number_theory.inverse_mod: expects n>0" else ()
      val a' = ((a mod n) + n) mod n
      val (d, s) = gcd_Bezout_coef (a', n)
    in
      if d <> 1 then raise Fail "Number_theory.inverse_mod: not invertible"
      else s
    end

  fun power_mod (x0: int, k0: int, n: int) : int =
    if n = 0 then raise Fail "Number_theory.power_mod: modulus 0"
    else
      let
        val nPos = Int.abs n
        val (x, k) =
          if k0 < 0 then (inverse_mod (x0, nPos), ~k0) else (x0, k0)
        val x = ((x mod nPos) + nPos) mod nPos
        fun mulMod (a: int, b: int) : int =
          IntInf.toInt (IntInf.mod (IntInf.fromInt a * IntInf.fromInt b, IntInf.fromInt nPos))
        fun loop (acc, base, e) =
          if e = 0 then acc
          else if e mod 2 = 1 then loop (mulMod (acc, base), mulMod (base, base), e div 2)
          else loop (acc, mulMod (base, base), e div 2)
      in
        loop (1 mod nPos, x, k)
      end

  fun test_Fermat (x: int, p: int) : bool =
    power_mod (x, p, p) = (x mod p + p) mod p

  fun is_prime (n: int) : bool = isPrimeTrial n

  (* ---------- primitive root heuristics (ported directly from `.at`) ---------- *)

  type phi_data = {n: int, phi_n: int, prime_factors_phi: int list}

  fun phi_data_of (n: int) : phi_data =
    let
      val phi_n = phi n
    in
      {n = n, phi_n = phi_n, prime_factors_phi = prime_divisors phi_n}
    end

  fun prime_phi_data (p: int) : phi_data =
    {n = p, phi_n = p - 1, prime_factors_phi = prime_divisors (p - 1)}

  fun is_multiplicative_generator_data (i: int, d: phi_data) : bool =
    let
      val n = #n d
      val phi_n = #phi_n d
      val factors = #prime_factors_phi d
    in
      List.all (fn p => power_mod (i, phi_n div p, n) <> 1) factors
    end

  fun is_multiplicative_generator (i: int, n: int) : bool =
    is_multiplicative_generator_data (i, phi_data_of n)

  fun search_probable_generator_data (d: phi_data) : int =
    let
      val n = #n d
      fun loop i =
        if i >= n then 1
        else if gcd (i, n) = 1 andalso is_multiplicative_generator_data (i, d) then i
        else loop (i + 1)
    in
      if n <= 2 then 1 else loop 2
    end

  fun search_probable_generator (n: int) : int =
    let
      val d = if test_Fermat (2, n) then prime_phi_data n else phi_data_of n
    in
      search_probable_generator_data d
    end
end
