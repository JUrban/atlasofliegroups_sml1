use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/W_orbit.sml

  Purpose
  - Partial SML translation of `atlas-scripts/W_orbit.at`.
  - Provides orbit-generation utilities for Weyl group actions on integral
    weights/coweights, focusing on the “generate from a dominant element”
    algorithms that many `.at` scripts use when they want:
      - orbit elements
      - short witness words (in simple generators)
      - optional accumulated action matrices for a representation of W

  Current scope (incremental port)
  - Implemented:
      - `simple_actor` (“lowering only” simple reflection action)
      - `generate_from_dom` (orbit generation with witness words)
      - `generate_action_from_dom` (as above, also tracking action matrices)
      - `from_dominant_vec` (deterministic dominant factoring for integral vecs)
      - `W_orbit_dom` / `W_orbit` (orbit as row-matrix of vectors)
      - `from_dominant_vec_gens` / `generate_from` / `stabiliser_quotient`:
        parabolic-subgroup analogues parameterized by generator lists
      - coweight analogues for the above core operations
      - a few convenience helpers (`allSimples`, `act_word_rtl`)
  - Not yet implemented:
      - iterators over parabolic subgroups and full W

  Terminology and conventions
  - Vectors are represented as integer lists `int list`, of length `rank(rd)`.
  - Witness words are lists of simple indices (0-based), interpreted in the
    `.at` convention:
      “apply right-to-left to transform the start vector into the target”.
    Concretely, `act_word_rtl(rd, w, v)` applies the generators in `rev w`.

  Design notes
  - The original `W_orbit.at` relies on interpreter built-ins (`Weyl_orbit`,
    `Weyl_orbit_ws`). Here we reimplement the level-by-level generation logic
    in pure SML so that translated scripts do not depend on the `.at`
    interpreter.
*)

structure WOrbit = struct
  type vec = int list
  type word = int list
  type mat = IntMatrix.mat
  type rootdatum = RootDatum.t
  type ratvec = Lattice.ratvec

  (* Lightweight “iterator” interface, mirroring the `.at` convention. *)
  type 'a iterator = {peek: unit -> 'a option, advance: unit -> unit}

  (* ---------- vector utilities ---------- *)

  fun vecZipWith f (xs: int list, ys: int list) : int list =
    let
      fun loop ([], [], acc) = List.rev acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', f (a, b) :: acc)
        | loop _ = raise Fail "WOrbit.vecZipWith: length mismatch"
    in
      loop (xs, ys, [])
    end

  fun vecSub (xs: vec, ys: vec) : vec = vecZipWith (fn (a, b) => a - b) (xs, ys)

  fun vecScale (k: int, xs: vec) : vec = List.map (fn a => k * a) xs

  fun allSimples (rd: rootdatum) : int list =
    List.tabulate (RootDatum.semisimpleRank rd, fn i => i)

  (* Standard basis vector `e_i` in Z^n. *)
  fun identity_row (n: int, i: int) : vec =
    if i < 0 orelse i >= n then raise Fail "WOrbit.identity_row: oob"
    else List.tabulate (n, fn j => if i = j then 1 else 0)

  (* Simple root i as a vector in X^*, in the same coordinate convention as
     `RootDatum.simpleRootsCols`. *)
  fun simpleRoot (rd: rootdatum, i: int) : vec =
    List.nth (RootDatum.simpleRootsCols rd, i)

  (* Simple coroot i as a vector in X_*, in the same coordinate convention as
     `RootDatum.simpleCorootsCols`. *)
  fun simpleCoroot (rd: rootdatum, i: int) : vec =
    List.nth (RootDatum.simpleCorootsCols rd, i)

  (* Reflection of a weight `x` through the simple root `i`:
       s_i(x) = x - <x, alpha_i^vee> * alpha_i
     where pairing is implemented by integer dot product in the Atlas
     coordinate conventions. *)
  fun reflectSimpleWeight (rd: rootdatum, i: int) (x: vec) : vec =
    let
      val alpha = simpleRoot (rd, i)
      val av = simpleCoroot (rd, i)
      val m = RootDatum.dot (x, av)
    in
      vecSub (x, vecScale (m, alpha))
    end

  (* Reflection of a coweight `x` through the simple coroot `i`:
       s_i(x) = x - <alpha_i, x> * alpha_i^vee.
  *)
  fun reflectSimpleCoweight (rd: rootdatum, i: int) (x: vec) : vec =
    let
      val alpha = simpleRoot (rd, i)
      val av = simpleCoroot (rd, i)
      val m = RootDatum.dot (alpha, x)
    in
      vecSub (x, vecScale (m, av))
    end

  (* Apply a word in the `.at` right-to-left convention. *)
  fun act_word_rtl_with (reflect: rootdatum * int -> vec -> vec) (rd: rootdatum, w: word, start: vec) : vec =
    List.foldl (fn (i, v) => reflect (rd, i) v) start (List.rev w)

  fun act_word_rtl (rd: rootdatum, w: word, start: vec) : vec =
    act_word_rtl_with reflectSimpleWeight (rd, w, start)

  fun act_word_rtl_coweight (rd: rootdatum, w: word, start: vec) : vec =
    act_word_rtl_with reflectSimpleCoweight (rd, w, start)

  (* ---------- orbit generation ---------- *)

  (* Build a row-major matrix from columns. *)
  fun matFromColumns (nRows: int, cols: vec list) : mat =
    if nRows < 0 then
      raise Fail "WOrbit.matFromColumns: negative row count"
    else
      (case cols of
         [] => List.tabulate (nRows, fn _ => [])
       | c0 :: cs =>
           let
             val () = if length c0 = nRows then () else raise Fail "WOrbit.matFromColumns: column length mismatch"
             val () = if List.all (fn c => length c = nRows) cs then () else raise Fail "WOrbit.matFromColumns: ragged columns"
             fun row i = List.map (fn c => List.nth (c, i)) cols
           in
             List.tabulate (nRows, row)
           end)

  (* Convert a rational solution (BigRat list) to an integer numerator vector
     by clearing denominators and reducing by a common gcd.

     Intended use: build “test vectors” for stabilizers. Scaling a weight by a
     nonzero integer does not change stabilizers, so using the numerator is safe.
  *)
  fun bigratListToNumerVec (qs: LatticeAT.BigRat.t list) : vec =
    let
      val qs = List.map LatticeAT.BigRat.normalize qs
      fun lcm (a: IntInf.int, b: IntInf.int) : IntInf.int =
        let
          val g = LatticeAT.BigRat.gcd (a, b)
          val prod = IntInf.abs (a * b)
        in
          if g = 0 then 0 else IntInf.div (prod, g)
        end
      val denL = List.foldl (fn (q, acc) => lcm (acc, #den q)) 1 qs
      val numsInf = List.map (fn q => #num q * IntInf.div (denL, #den q)) qs
      fun gcdInf (a: IntInf.int, b: IntInf.int) : IntInf.int = LatticeAT.BigRat.gcd (a, b)
      val g = List.foldl gcdInf denL (List.map IntInf.abs numsInf)
      val numsRed = List.map (fn n => if g = 0 then n else IntInf.div (n, g)) numsInf
      fun toInt n = IntInf.toInt n handle _ => raise Fail "WOrbit: overflow converting rational solution to int vector"
    in
      List.map toInt numsRed
    end

  (* `required_solution` analogue for `A*x=b` over Q, returning a numerator
     vector for one rational solution.

     Raises `Fail` if no solution exists.
  *)
  fun required_solution_numer (a: mat, b: vec) : vec =
    (case LatticeAT.solve_ratvec (a, {den = 1, nums = b}) of
       NONE => raise Fail "WOrbit.required_solution_numer: no solution"
     | SOME qs => bigratListToNumerVec qs)

  (* Dominance test for integral weights: <x, alpha_i^vee> >= 0 for all simple i. *)
  fun is_dominant (rd: rootdatum, x: vec) : bool =
    let
      val ssr = RootDatum.semisimpleRank rd
      fun ok i = RootDatum.dot (x, simpleCoroot (rd, i)) >= 0
    in
      List.all ok (List.tabulate (ssr, fn i => i))
    end

  (* Deterministically factor an integral weight into a dominant representative.

     Returns `(witnessWord, x_dom)` where:
       - `x_dom` is dominant
       - `act_word_rtl(rd, witnessWord, x_dom) = x_original`

     This is the integral-weight analogue of `.at` `from_dominant` / C++
     `RootDatum::factor_dominant`, but implemented in pure SML using the
     simple reflection formulas.

     Notes
     - This implementation is deterministic but not guaranteed to return a
       *minimal-length* witness word (the C++ version typically is).
  *)
  fun from_dominant_vec (rd: rootdatum, x_original: vec) : word * vec =
    let
      val ssr = RootDatum.semisimpleRank rd

      fun firstNegative (x: vec) : int option =
        let
          fun loop i =
            if i >= ssr then NONE
            else if RootDatum.dot (x, simpleCoroot (rd, i)) < 0 then SOME i
            else loop (i + 1)
        in
          loop 0
        end

      fun loop (x: vec, appliedRev: int list) : word * vec =
        (case firstNegative x of
           NONE => (List.rev appliedRev, x)
         | SOME i => loop (reflectSimpleWeight (rd, i) x, i :: appliedRev))
    in
      loop (x_original, [])
    end

  (* Parabolic-subgroup variant: make `x` dominant with respect to the
     generators `gens` (a list of simple indices).

     Returns `(witnessWord, x_dom)` where:
       - `x_dom` is `gens`-dominant: <x_dom, alpha_i^vee> >= 0 for all i in gens
       - `act_word_rtl(rd, witnessWord, x_dom) = x_original`
  *)
  fun from_dominant_vec_gens (rd: rootdatum, gens: int list, x_original: vec) : word * vec =
    let
      fun firstNegativeInGens (x: vec) : int option =
        let
          fun loop [] = NONE
            | loop (i :: is) =
                if RootDatum.dot (x, simpleCoroot (rd, i)) < 0 then SOME i else loop is
        in
          loop gens
        end

      fun loop (x: vec, appliedRev: int list) : word * vec =
        (case firstNegativeInGens x of
           NONE => (List.rev appliedRev, x)
         | SOME i => loop (reflectSimpleWeight (rd, i) x, i :: appliedRev))
    in
      loop (x_original, [])
    end

  fun from_dominant_vec_gens_coweight (rd: rootdatum, gens: int list, x_original: vec) : word * vec =
    let
      fun firstNegativeInGens (x: vec) : int option =
        let
          fun loop [] = NONE
            | loop (i :: is) =
                if RootDatum.dot (simpleRoot (rd, i), x) < 0 then SOME i else loop is
        in
          loop gens
        end

      fun loop (x: vec, appliedRev: int list) : word * vec =
        (case firstNegativeInGens x of
           NONE => (List.rev appliedRev, x)
         | SOME i => loop (reflectSimpleCoweight (rd, i) x, i :: appliedRev))
    in
      loop (x_original, [])
    end

  (* "lowering only" standard simple reflection action on weights.

     Mirrors `simple_actor` in `W_orbit.at`: if the coroot evaluation is
     positive, return the reflected weight; otherwise return `NONE`.
  *)
  fun simple_actor (rd: rootdatum, i: int) : vec -> vec option =
    let
      val av = simpleCoroot (rd, i)
    in
      fn x =>
        if RootDatum.dot (x, av) > 0 then
          SOME (reflectSimpleWeight (rd, i) x)
        else
          NONE
    end

  fun simple_actor_coweight (rd: rootdatum, i: int) : vec -> vec option =
    let
      val alpha = simpleRoot (rd, i)
    in
      fn x =>
        if RootDatum.dot (alpha, x) > 0 then
          SOME (reflectSimpleCoweight (rd, i) x)
        else
          NONE
    end

  (* Core generator: produce orbit from a dominant start, partitioned into
     levels, and return pairs `(b, w)` where `w` witnesses `start -> b`.

     This follows `generate_from_dom` in `W_orbit.at`, with one difference:
     we carry the *simple generator index* in the word, not the local index in
     the actor list. This is more directly usable for later SML code.
  *)
  fun generate_from_dom (actors: (vec -> vec option) list, actorLabels: int list, start: vec) : (vec * word) list =
    let
      val () =
        if length actors = length actorLabels then
          ()
        else
          raise Fail "WOrbit.generate_from_dom: actors/labels length mismatch"

      fun absent (level: (vec * word) list, target: vec) : bool =
        not (Option.isSome (Basic.binary_search_in_by (level, #1, Sort.rlex_leq) target))

      fun newLevel (cur: (vec * word) list, prev: (vec * word) list) : (vec * word) list =
        let
          fun stepOne (a, w) =
            let
              fun one (f, lab) =
                (case f a of
                   NONE => []
                 | SOME b =>
                     if absent (prev, b) andalso absent (cur, b) then [(b, lab :: w)] else [])
            in
              List.concat (List.map one (ListPair.zip (actors, actorLabels)))
            end
        in
          List.concat (List.map stepOne cur)
        end

      fun loop (stack: (vec * word) list list) : (vec * word) list list =
        (case stack of
           cur :: prev :: _ =>
             let
               val nl = Basic.sort_u_by (#1, Sort.rlex_leq) (newLevel (cur, prev))
             in
               if null nl then stack else loop (nl :: stack)
             end
         | _ => raise Fail "WOrbit.generate_from_dom: internal: stack underflow")

      val stack0 = [[(start, [])], []]
      val stack = loop stack0
    in
      List.concat (List.rev stack)
    end

  (* Convenience wrapper: use simple reflections indexed by `gens` as generators. *)
  fun generate_from_dom_simples (rd: rootdatum, gens: int list, start: vec) : (vec * word) list =
    generate_from_dom (List.map (fn i => simple_actor (rd, i)) gens, gens, start)

  fun generate_from_dom_simples_coweight (rd: rootdatum, gens: int list, start: vec) : (vec * word) list =
    generate_from_dom (List.map (fn i => simple_actor_coweight (rd, i)) gens, gens, start)

  (* Parabolic-subgroup analogue of `.at` `generate_from`:
     given any `v`, first make it `gens`-dominant; then generate orbit of the
     dominant representative; finally adjust witness words so that each word
     maps the original `v` to the target.

     Returned pairs `(b,w)` satisfy `act_word_rtl(rd,w,v)=b`.
  *)
  fun generate_from (rd: rootdatum, gens: int list, v: vec) : (vec * word) list =
    let
      val (chamber, domv) = from_dominant_vec_gens (rd, gens, v)
      val to_dom = List.rev chamber (* inverse word; s_i^{-1}=s_i *)
      val orbit = generate_from_dom_simples (rd, gens, domv)
      fun adjust (b, w_dom) = (b, w_dom @ to_dom)
    in
      List.map adjust orbit
    end

  fun generate_from_coweight (rd: rootdatum, gens: int list, v: vec) : (vec * word) list =
    let
      val (chamber, domv) = from_dominant_vec_gens_coweight (rd, gens, v)
      val to_dom = List.rev chamber
      val orbit = generate_from_dom_simples_coweight (rd, gens, domv)
      fun adjust (b, w_dom) = (b, w_dom @ to_dom)
    in
      List.map adjust orbit
    end

  (* “Minimal coset representatives” for the stabilizer of `v` in the subgroup
     generated by `gens`, represented as words. This mirrors the
     `stabiliser_quotient` functions in `W_orbit.at`, but returns words rather
     than Atlas `WeylElt` values.
  *)
  fun stabiliser_quotient_of_dom (rd: rootdatum, gens: int list, v_dom: vec) : word list =
    List.map #2 (generate_from_dom_simples (rd, gens, v_dom))

  fun stabiliser_quotient (rd: rootdatum, gens: int list, v: vec) : word list =
    List.map #2 (generate_from (rd, gens, v))

  fun stabiliser_quotient_of_dom_coweight (rd: rootdatum, gens: int list, v_dom: vec) : word list =
    List.map #2 (generate_from_dom_simples_coweight (rd, gens, v_dom))

  fun stabiliser_quotient_coweight (rd: rootdatum, gens: int list, v: vec) : word list =
    List.map #2 (generate_from_coweight (rd, gens, v))

  (* Orbit of a dominant integral weight under the full Weyl group, returned as
     a matrix of row vectors (matching the `.at` convention for `mat`). *)
  fun W_orbit_dom (rd: rootdatum, start_dom: vec) : mat =
    List.map #1 (generate_from_dom_simples (rd, allSimples rd, start_dom))

  (* Orbit of an arbitrary integral weight under the full Weyl group.

     Implementation: compute a dominant representative and then generate the
     orbit from that dominant representative (the orbit set is unchanged).
  *)
  fun W_orbit (rd: rootdatum, x: vec) : mat =
    let
      val (_, x_dom) = from_dominant_vec (rd, x)
    in
      W_orbit_dom (rd, x_dom)
    end

  fun W_orbit_gens_dom (rd: rootdatum, gens: int list, start_dom: vec) : mat =
    List.map #1 (generate_from_dom_simples (rd, gens, start_dom))

  fun W_orbit_gens (rd: rootdatum, gens: int list, x: vec) : mat =
    let
      val (_, x_dom) = from_dominant_vec_gens (rd, gens, x)
    in
      W_orbit_gens_dom (rd, gens, x_dom)
    end

  fun W_orbit_coweight (rd: rootdatum, x: vec) : mat =
    let
      val (_, x_dom) = from_dominant_vec_gens_coweight (rd, allSimples rd, x)
    in
      List.map #1 (generate_from_dom_simples_coweight (rd, allSimples rd, x_dom))
    end

  fun W_orbit_gens_coweight (rd: rootdatum, gens: int list, x: vec) : mat =
    let
      val (_, x_dom) = from_dominant_vec_gens_coweight (rd, gens, x)
    in
      List.map #1 (generate_from_dom_simples_coweight (rd, gens, x_dom))
    end

  (* Variant tracking action matrices.

     Each returned triple `(b, w, act)` satisfies:
        - `b` is the orbit weight
       - `w` witnesses `start -> b` (right-to-left)
       - `act` is the product of representation matrices for the word, in the
         same right-to-left convention:
           act = M_{w_last} * ... * M_{w_first}
     where `M_i` is the matrix for generator `i`.
  *)
  fun generate_action_from_dom
    ( actors: (vec -> vec option) list
    , actorLabels: int list
    , start: vec
    , dim: int
    , gens_rep: mat list
    ) : (vec * word * mat) list =
    let
      val () =
        if length actors = length actorLabels andalso length actors = length gens_rep then
          ()
        else
          raise Fail "WOrbit.generate_action_from_dom: arity mismatch"

      fun absent (level: (vec * word * mat) list, target: vec) : bool =
        not (Option.isSome (Basic.binary_search_in_by (level, (fn (v, _, _) => v), Sort.rlex_leq) target))

      fun newLevel (cur: (vec * word * mat) list, prev: (vec * word * mat) list) : (vec * word * mat) list =
        let
          fun stepOne (a, w, act) =
            let
              fun one ((f, lab), genMat) =
                (case f a of
                   NONE => []
                 | SOME b =>
                     if absent (prev, b) andalso absent (cur, b) then
                       [(b, lab :: w, IntMatrix.matMul (genMat, act))]
                     else
                       [])
            in
              List.concat (List.map one (ListPair.zip (ListPair.zip (actors, actorLabels), gens_rep)))
            end
        in
          List.concat (List.map stepOne cur)
        end

      fun loop (stack: (vec * word * mat) list list) : (vec * word * mat) list list =
        (case stack of
           cur :: prev :: _ =>
             let
               val nl = Basic.sort_u_by ((fn (v, _, _) => v), Sort.rlex_leq) (newLevel (cur, prev))
             in
               if null nl then stack else loop (nl :: stack)
             end
         | _ => raise Fail "WOrbit.generate_action_from_dom: internal: stack underflow")

      val stack0 = [[(start, [], MatrixAT.id_mat dim)], []]
      val stack = loop stack0
    in
      List.concat (List.rev stack)
    end

  fun generate_action_from_dom_simples
    ( rd: rootdatum
    , gens: int list
    , start: vec
    , dim: int
    , gens_rep: mat list
    ) : (vec * word * mat) list =
    generate_action_from_dom
      (List.map (fn i => simple_actor (rd, i)) gens, gens, start, dim, gens_rep)

  (* ---------- Weyl group iterators (word-based) ---------- *)

  (* Iterator over the Weyl subgroup generated by `gens`, following the tower
     construction in `W_orbit.at`’s `W_parabolic_iterator`.

     The iterator yields words (lists of simple indices) representing the Weyl
     group elements.
  *)
  fun W_parabolic_iterator (rd: rootdatum, gens: int list) : word iterator =
    let
      val ng = length gens
      val r = RootDatum.rank rd

      fun prefix k = List.take (gens, k + 1)

      fun buildA (para: int list) : mat =
        let
          val corootRows = List.map (fn i => simpleCoroot (rd, i)) para
          val rootCols = List.map (fn i => simpleRoot (rd, i)) para
          val rootMat = matFromColumns (r, rootCols) (* r x |para|, columns are roots *)
          val cok = IntMatrix.cokernel rootMat (* (r-|para|) x r, row constraints for root span *)
          val a = corootRows @ cok
          val (ar, ac) = IntMatrix.matShape a
          val () =
            if ar = r andalso ac = r then
              ()
            else
              raise Fail "WOrbit.W_parabolic_iterator: A is not square (unexpected root dependence?)"
        in
          a
        end

      (* Subquotient stack for the tower: each entry is a list of words. *)
      val stack : word list list =
        List.tabulate
          ( ng
          , fn k =>
              let
                val para = prefix k
                val a = buildA para
                val rhs = identity_row (r, k)
                val v = required_solution_numer (a, rhs)
              in
                stabiliser_quotient (rd, para, v)
              end
          )

      val m = Array.fromList (List.map length stack)
      val state = Array.array (ng, 0)

      fun done () : bool = ng = 0 orelse Array.sub (state, 0) = Array.sub (m, 0)

      fun pick (i: int) : word =
        let
          val n = Array.sub (state, i)
          val ws = List.nth (stack, i)
        in
          List.nth (ws, n)
        end

      fun currentWord () : word =
        let
          val wsDesc = List.tabulate (ng, fn j => pick (ng - 1 - j))
        in
          List.concat wsDesc
        end

      fun peek () = if done () then NONE else SOME (currentWord ())

      fun advance () =
        if done () then
          ()
        else if ng = 1 then
          Array.update (state, 0, Array.sub (state, 0) + 1)
        else
          let
            fun loop i =
              if i <= 0 then
                Array.update (state, 0, Array.sub (state, 0) + 1)
              else
                let
                  val ni = Array.sub (state, i) + 1
                in
                  if ni < Array.sub (m, i) then
                    Array.update (state, i, ni)
                  else
                    (Array.update (state, i, 0); loop (i - 1))
                end
          in
            loop (ng - 1)
          end
    in
      {peek = peek, advance = advance}
    end

  fun W_iterator (rd: rootdatum) : word iterator =
    W_parabolic_iterator (rd, allSimples rd)

  (* Compute the representation matrix of a word, given generator matrices for
     a specific generator list `gens` (same order as `gens_rep`).

     The matrix returned satisfies: `mat * v = act_word_rtl(rd,word,v)` when
     `gens_rep` are the matrices for the simple reflections acting on the same
     vector space as `v`.
  *)
  fun action_matrix_of_word (gens: int list, gens_rep: mat list, dim: int) (w: word) : mat =
    let
      fun indexOf i =
        let
          fun loop ([], _) = NONE
            | loop (j :: js, k) = if j = i then SOME k else loop (js, k + 1)
        in
          loop (gens, 0)
        end
      fun matOf i =
        (case indexOf i of
           NONE => raise Fail "WOrbit.action_matrix_of_word: generator index not in gens"
         | SOME k => List.nth (gens_rep, k))
      fun step (i, acc) = IntMatrix.matMul (matOf i, acc)
    in
      List.foldl step (MatrixAT.id_mat dim) w
    end

  (* Iterator variant that also returns accumulated representation matrices.

     Input:
       - `gens_rep` is a list of `dim x dim` integer matrices, aligned with `gens`
         (i.e. `gens_rep[j]` is the matrix for the generator `gens[j]`).

     Output elements are `(word, act)` where `act` is the matrix for the word.
  *)
  fun W_parabolic_iterator_with_action
    ( rd: rootdatum
    , gens: int list
    , dim: int
    , gens_rep: mat list
    ) : (word * mat) iterator =
    let
      val ng = length gens
      val () = if length gens_rep = ng then () else raise Fail "WOrbit.W_parabolic_iterator_with_action: gens_rep arity"
      val r = RootDatum.rank rd

      fun prefix k = List.take (gens, k + 1)
      fun prefixMats k = List.take (gens_rep, k + 1)

      fun buildA (para: int list) : mat =
        let
          val corootRows = List.map (fn i => simpleCoroot (rd, i)) para
          val rootCols = List.map (fn i => simpleRoot (rd, i)) para
          val rootMat = matFromColumns (r, rootCols)
          val cok = IntMatrix.cokernel rootMat
          val a = corootRows @ cok
          val (ar, ac) = IntMatrix.matShape a
          val () =
            if ar = r andalso ac = r then
              ()
            else
              raise Fail "WOrbit.W_parabolic_iterator_with_action: A is not square"
        in
          a
        end

      fun stabiliser_quotient_with_action (para: int list, mats: mat list, v: vec) : (word * mat) list =
        let
          val (chamber, domv) = from_dominant_vec_gens (rd, para, v)
          val to_dom = List.rev chamber
          val act_to_dom = action_matrix_of_word (para, mats, dim) to_dom

          val orbit = generate_action_from_dom_simples (rd, para, domv, dim, mats)
          fun term (_, w_dom, act_dom) =
            let
              val w = w_dom @ to_dom
              val act = IntMatrix.matMul (act_dom, act_to_dom)
            in
              (w, act)
            end
        in
          List.map term orbit
        end

      val stack : (word * mat) list list =
        List.tabulate
          ( ng
          , fn k =>
              let
                val para = prefix k
                val mats = prefixMats k
                val a = buildA para
                val rhs = identity_row (r, k)
                val v = required_solution_numer (a, rhs)
              in
                stabiliser_quotient_with_action (para, mats, v)
              end
          )

      val m = Array.fromList (List.map length stack)
      val state = Array.array (ng, 0)

      fun done () : bool = ng = 0 orelse Array.sub (state, 0) = Array.sub (m, 0)

      fun pick (i: int) : word * mat =
        let
          val n = Array.sub (state, i)
          val ws = List.nth (stack, i)
        in
          List.nth (ws, n)
        end

      fun current () : word * mat =
        let
          val partsDesc = List.tabulate (ng, fn j => pick (ng - 1 - j))
          val w = List.concat (List.map #1 partsDesc)
          val matsDesc = List.map #2 partsDesc
          val act = List.foldr (fn (mm, acc) => IntMatrix.matMul (mm, acc)) (MatrixAT.id_mat dim) matsDesc
        in
          (w, act)
        end

      fun peek () = if done () then NONE else SOME (current ())

      fun advance () =
        if done () then
          ()
        else if ng = 1 then
          Array.update (state, 0, Array.sub (state, 0) + 1)
        else
          let
            fun loop i =
              if i <= 0 then
                Array.update (state, 0, Array.sub (state, 0) + 1)
              else
                let
                  val ni = Array.sub (state, i) + 1
                in
                  if ni < Array.sub (m, i) then
                    Array.update (state, i, ni)
                  else
                    (Array.update (state, i, 0); loop (i - 1))
                end
          in
            loop (ng - 1)
          end
    in
      {peek = peek, advance = advance}
    end
end
