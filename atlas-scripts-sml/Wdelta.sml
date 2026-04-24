use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/weylgroup_at.sml";

(*
  File: atlas-scripts-sml/Wdelta.sml

  Purpose
  - Partial SML translation of `atlas-scripts/Wdelta.at`.
  - Provides utilities related to the fixed-point Weyl group `W^delta`:
      - construct the simple generators of `W^delta` (as elements of the full Weyl
        group `W`) using the same algorithmic pattern as the `.at` script
      - convert a word in `W^delta` generators into the corresponding element of `W`

  Scope / limitations
  - The `.at` version is written in terms of Atlas interpreter types like
    `InnerClass` and `WeylElt`. The SML port works at the level of root data
    and integer matrices:
      - Weyl elements are represented by their action matrices on `X^*`
        (row-major `IntMatrix.mat`).
      - Words are `int list` of simple generator indices.
  - Computing the projection matrix `P = projection_to_K_matrix(ic)` and the
    delta-fixed root datum `rd_delta` from an `InnerClass` requires additional
    ports of `K.at`-style infrastructure; this file assumes `P` and `rd_delta`
    are supplied explicitly.
*)

structure Wdelta = struct
  type rootdatum = RootDatum.t
  type vec = int list
  type mat = IntMatrix.mat
  type word = int list

  fun fail where' msg = raise Fail ("Wdelta." ^ where' ^ ": " ^ msg)

  fun matShape (m: mat) =
    let
      val (r, c) = IntMatrix.matShape m
    in
      if r <> c then fail "matShape" "expected square matrix" else r
    end

  fun idW (rd: rootdatum) : mat =
    IntMatrix.identity (RootDatum.rank rd)

  fun reflMat (rd: rootdatum, s: int) : mat =
    WeylgroupAT.reflection_matrix_simple (rd, s)

  fun rightMulSimple (rd: rootdatum, w: mat, s: int) : mat =
    IntMatrix.matMul (w, reflMat (rd, s))

  fun leftMulSimple (rd: rootdatum, s: int, w: mat) : mat =
    IntMatrix.matMul (reflMat (rd, s), w)

  fun isIdentity (rd: rootdatum, w: mat) : bool = (w = idW rd)

  (* Coxeter length via inversion count: #{ alpha>0 | w(alpha) < 0 }. *)
  fun weylLength (rd: rootdatum, w: mat) : int =
    let
      val pos = RootDatum.posRootsCols rd
      fun loop ([], acc) = acc
        | loop (alpha :: rest, acc) =
            let
              val image = IntMatrix.matVecMul (w, alpha)
              val pos' = WeylgroupAT.is_positive_root (rd, image)
            in
              loop (rest, if pos' then acc else acc + 1)
            end
    in
      loop (pos, 0)
    end

  fun act (w: mat, v: vec) : vec = IntMatrix.matVecMul (w, v)

  (*
    Port of `.at` `Wdelta_generators(ic)` but parameterized explicitly by:
    - `rd`: root datum of the full Weyl group `W`
    - `rd_delta`: root datum of the delta-fixed system
    - `P`: projection matrix sending roots of `rd` to roots of `rd_delta`

    Returns: a list of `ssr_delta` matrices in `W(rd)` representing the simple
    generators of `W^delta` (in the ordering of `rd_delta` simple roots).
  *)
  fun Wdelta_generatorsWith (rd: rootdatum, rd_delta: rootdatum, P: mat) : mat list =
    let
      val ssr = RootDatum.semisimpleRank rd
      val ssr_delta = RootDatum.semisimpleRank rd_delta
      val gens = Array.tabulate (ssr_delta, fn _ => idW rd)

      fun updateAt (j: int, w: mat) : unit =
        if j < 0 orelse j >= ssr_delta then
          fail "Wdelta_generatorsWith" "projected root index out of range"
        else
          Array.update (gens, j, w)

      fun getAt (j: int) : mat =
        if j < 0 orelse j >= ssr_delta then
          fail "Wdelta_generatorsWith" "projected root index out of range"
        else
          Array.sub (gens, j)

      fun simpleRoots rd = RootDatum.simpleRootsCols rd

      fun loopSimples (i: int, betas: vec list) : unit =
        (case betas of
           [] => ()
         | beta :: rest =>
             let
               val betaProj = IntMatrix.matVecMul (P, beta)
               val idx = RootDatum.rootIndex (rd_delta, betaProj)
               val npr_delta = RootDatum.numPosRoots rd_delta
               val () =
                 if idx = npr_delta then
                   fail "Wdelta_generatorsWith" "projection did not yield a root of rd_delta"
                 else
                   ()

               val j = idx (* should be >=0 for a simple root; we keep as-is *)
               val gen_j = getAt j
             in
               if isIdentity (rd, gen_j) then
                 updateAt (j, rightMulSimple (rd, gen_j, i))
               else
                 let
                   val () =
                     if weylLength (rd, gen_j) = 1 then
                       ()
                     else
                       fail "Wdelta_generatorsWith" "expected a simple reflection at this stage"
                   val fixesBeta = (act (gen_j, beta) = beta)
                   val newGen =
                     if fixesBeta then
                       rightMulSimple (rd, gen_j, i)
                     else
                       rightMulSimple (rd, leftMulSimple (rd, i, gen_j), i)
                 in
                   updateAt (j, newGen)
                 end;
               loopSimples (i + 1, rest)
             end)

      val betas = simpleRoots rd
      val () =
        if List.length betas = ssr then
          ()
        else
          fail "Wdelta_generatorsWith" "simpleRootsCols length mismatch"
    in
      loopSimples (0, betas);
      Array.foldr (op ::) [] gens
    end

  (* Convert a word in `W^delta` generators (indices into `gens`) into an element of `W(rd)`
     represented by an action matrix. *)
  fun convert_from_W_K_with (rd: rootdatum, gens: mat list, w: word) : mat =
    let
      val gensA = Array.fromList gens
      val n = Array.length gensA

      fun step (s: int, acc: mat) : mat =
        if s < 0 orelse s >= n then
          fail "convert_from_W_K_with" "generator index out of range"
        else
          IntMatrix.matMul (acc, Array.sub (gensA, s))
    in
      List.foldl step (idW rd) w
    end
end
