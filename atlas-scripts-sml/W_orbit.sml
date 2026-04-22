use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/IntMatrix.sml";
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
      - a few convenience helpers (`allSimples`, `act_word_rtl`)
  - Not yet implemented:
      - `stabiliser_quotient` / `Weyl_orbit_ws` analogues
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
  fun reflectSimple (rd: rootdatum, i: int) (x: vec) : vec =
    let
      val alpha = simpleRoot (rd, i)
      val av = simpleCoroot (rd, i)
      val m = RootDatum.dot (x, av)
    in
      vecSub (x, vecScale (m, alpha))
    end

  (* Apply a word in the `.at` right-to-left convention. *)
  fun act_word_rtl (rd: rootdatum, w: word, start: vec) : vec =
    List.foldl (fn (i, v) => reflectSimple (rd, i) v) start (List.rev w)

  (* ---------- orbit generation ---------- *)

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
          SOME (reflectSimple (rd, i) x)
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
end
