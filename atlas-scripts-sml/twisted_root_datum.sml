use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/sort.sml";

(* Minimal SML analogue of `atlas-scripts/twisted_root_datum.at`:
   currently only implements `cyclic_twist`. *)
structure TwistedRootDatum = struct
  type mat = IntMatrix.mat
  type rootdatum = RootDatum.t
  type t = {rd: rootdatum, delta: mat}

  fun dot (xs: int list, ys: int list) : int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "TwistedRootDatum.dot: length mismatch"
    in
      loop (xs, ys, 0)
    end

  fun vecScale (v: int list, k: int) : int list =
    List.map (fn x => x * k) v

  fun vecAdd (a: int list, b: int list) : int list =
    ListPair.mapEq (op +) (a, b)

  fun vecZero n = List.tabulate (n, fn _ => 0)

  fun matFromColumns (cols: int list list) : mat =
    (case cols of
       [] => []
     | c0 :: cs =>
         let
           val n = length c0
           val () = if List.all (fn c => length c = n) cs then () else raise Fail "TwistedRootDatum.matFromColumns: ragged"
           fun row i = List.map (fn c => List.nth (c, i)) cols
         in
          List.tabulate (n, row)
         end)

  fun rootdatum_from_positive (posRootsCols: int list list, posCorootsCols: int list list) : rootdatum =
    let
      val () =
        if length posRootsCols = length posCorootsCols then ()
        else raise Fail "TwistedRootDatum.rootdatum_from_positive: mismatched roots/coroots"
      val rootCor = ListPair.zipEq (posRootsCols, posCorootsCols)
      fun isSum alpha =
        List.exists
          (fn beta => List.exists (fn gamma => vecAdd (beta, gamma) = alpha) posRootsCols)
          posRootsCols
      fun isSimple alpha = not (isSum alpha)
      val simplePairs = List.filter (fn (r, _) => isSimple r) rootCor
      val simpleRoots = List.map #1 simplePairs
      val simpleCoroots = List.map #2 simplePairs
    in
      RootDatum.newFromSimpleMats (matFromColumns simpleRoots, matFromColumns simpleCoroots, false)
    end

  fun direct_product_rootdatum (rd1: rootdatum, rd2: rootdatum) : rootdatum =
    let
      val r1 = RootDatum.simpleRootsMat rd1
      val cr1 = RootDatum.simpleCorootsMat rd1
      val r2 = RootDatum.simpleRootsMat rd2
      val cr2 = RootDatum.simpleCorootsMat rd2
      val r = MatrixAT.block_matrix (r1, r2)
      val cr = MatrixAT.block_matrix (cr1, cr2)
    in
      RootDatum.newFromSimpleMats (r, cr, false)
    end

  fun mul (a: t, b: t) : t =
    let
      val rd = direct_product_rootdatum (#rd a, #rd b)
      val delta = MatrixAT.block_matrix (#delta a, #delta b)
    in
      {rd = rd, delta = delta}
    end

  fun is_distinguished (rd: rootdatum, delta: mat) : bool =
    let
      val simple = RootDatum.simpleRootsCols rd
      fun contains v = List.exists (fn w => w = v) simple
      fun ok alpha = contains (IntMatrix.matVecMul (delta, alpha))
    in
      List.all ok simple
    end

  fun order_twist (trd: t) : int =
    let
      val rd = #rd trd
      val delta = #delta trd
      val () = if is_distinguished (rd, delta) then () else raise Fail "TwistedRootDatum.order_twist: not distinguished"
      val simple = RootDatum.simpleRootsCols rd
      fun orbitSize alpha =
        let
          val maxIter = MatrixAT.order delta
          fun loop (k, v) =
            if v = alpha then k
            else if k >= maxIter then raise Fail "TwistedRootDatum.order_twist: exceeded delta order"
            else loop (k + 1, IntMatrix.matVecMul (delta, v))
          val v1 = IntMatrix.matVecMul (delta, alpha)
        in
          if v1 = alpha then 1 else loop (2, IntMatrix.matVecMul (delta, v1))
        end
    in
      List.foldl Int.max 1 (List.map orbitSize simple)
    end

  fun block_diag_repeat (m: mat, r: int) : mat =
    if r < 0 then
      raise Fail "TwistedRootDatum.block_diag_repeat: negative repeat"
    else if r = 0 then
      MatrixAT.null (0, 0)
    else
      let
        fun loop (1, acc) = acc
          | loop (k, acc) = loop (k - 1, MatrixAT.block_matrix (acc, m))
      in
        loop (r, m)
      end

  (* Construct automorphism delta of (rd,rd,...,rd) (r copies):
     (x_1,...,x_r) -> (tau(x_r),x_1,...,x_{r-1})
     delta^r = tau (on first factor). *)
  fun cyclic_twist (rd: rootdatum, tau: mat, r: int) : t =
    if r <= 0 then
      raise Fail "TwistedRootDatum.cyclic_twist: r must be positive"
    else
      let
        val n = RootDatum.rank rd

        val simple_roots = RootDatum.simpleRootsMat rd
        val simple_coroots = RootDatum.simpleCorootsMat rd

        val roots_r = block_diag_repeat (simple_roots, r)
        val coroots_r = block_diag_repeat (simple_coroots, r)

        val rd_r = RootDatum.newFromSimpleMats (roots_r, coroots_r, false)

        val cyclePi = (r - 1) :: List.tabulate (r - 1, fn i => i)
        val cycle = MatrixAT.permutation_matrix cyclePi
        val basic_twist = MatrixAT.Kronecker_product (cycle, MatrixAT.id_mat n)
        val diag = MatrixAT.block_matrix (MatrixAT.id_mat (n * (r - 1)), tau)
        val delta = IntMatrix.matMul (diag, basic_twist)
      in
        {rd = rd_r, delta = delta}
      end

  fun cyclic_twist_id (rd: rootdatum, r: int) : t =
    cyclic_twist (rd, MatrixAT.id_mat (RootDatum.rank rd), r)

  (* Not yet ported from `atlas-scripts/twisted_root_datum.at`. *)
  fun pre_folded (trd: t) : mat * mat =
    let
      val rd = #rd trd
      val delta = #delta trd
      val () = if is_distinguished (rd, delta) then () else raise Fail "TwistedRootDatum.pre_folded: not distinguished"

      val tMat = IntMatrix.eigenLattice (IntMatrix.transpose delta, 1) (* inclusion X_*(T)->X_*(H) *)
      val (_, tDim) = IntMatrix.matShape tMat
      val tStar = IntMatrix.transpose tMat (* restrict weights X^*(H)->X^*(T) *)

      fun restrict (root: int list) : int list =
        IntMatrix.matVecMul (tStar, root)

      val rootsNonreduced = Sort.sort_u_rlex (List.map restrict (RootDatum.posRootsCols rd))
      val locate = Basic.binary_search_in (rootsNonreduced, Sort.rlex_leq)
      val roots =
        List.filter (fn alpha => not (Option.isSome (locate (vecScale (alpha, 2))))) rootsNonreduced

      val rootsAndCoroots = ListPair.zipEq (RootDatum.rootsCols rd, RootDatum.corootsCols rd)

      fun pullback alpha =
        List.filter (fn (r, _) => restrict r = alpha) rootsAndCoroots

      fun corestrictCoroot alpha =
        let
          val pb = pullback alpha
          val () = if null pb then raise Fail "TwistedRootDatum.pre_folded: empty pullback" else ()
          val (r0, _) = hd pb
          val v =
            List.foldl
              (fn ((_, cor), acc) => vecAdd (acc, cor))
              (vecZero (RootDatum.rank rd))
              pb
          val denom = dot (v, r0)
          val () = if denom = 0 then raise Fail "TwistedRootDatum.pre_folded: zero pairing" else ()
          val w : Lattice.ratvec = {den = denom, nums = vecScale (v, 2)}
        in
          case Lattice.vec_solve (tMat, w) of
            NONE => raise Fail "TwistedRootDatum.pre_folded: corestrict solve failed"
          | SOME coords => coords
        end

      val coroots = List.map corestrictCoroot roots
    in
      (matFromColumns roots, matFromColumns coroots)
    end

  fun matColumns (m: mat) : int list list =
    let
      val (_, nCols) = IntMatrix.matShape m
      fun col j = List.map (fn row => List.nth (row, j)) m
    in
      List.tabulate (nCols, col)
    end

  fun folded (trd: t) : rootdatum * mat =
    let
      val (rootsMat, corootsMat) = pre_folded trd
      val rd = #rd trd
      val delta = #delta trd
      val tMat = IntMatrix.eigenLattice (IntMatrix.transpose delta, 1)

      val roots = matColumns rootsMat
      val coroots = matColumns corootsMat
      val () = if length roots = length coroots then () else raise Fail "TwistedRootDatum.folded: mismatched pre_folded"

      val foldedRd = rootdatum_from_positive (roots, coroots)
    in
      (foldedRd, tMat)
    end

  fun inverse_image_simple_factor (trd: t, foldedFactor: rootdatum, tMat: mat) : rootdatum =
    let
      val rd = #rd trd
      val tStar = IntMatrix.transpose tMat
      fun restrict v = IntMatrix.matVecMul (tStar, v)

      val foldedPos = Sort.sort_u_rlex (RootDatum.posRootsCols foldedFactor)
      val locate = Basic.binary_search_in (foldedPos, Sort.rlex_leq)

      val posRoots = RootDatum.posRootsCols rd
      val posCoroots = RootDatum.posCorootsCols rd
      val () = if length posRoots = length posCoroots then () else raise Fail "TwistedRootDatum.inverse_image_simple_factor: mismatch"

      fun keep (root: int list) : bool =
        Option.isSome (locate (restrict root))

      fun filterPairs ([], [], accR, accC) = (List.rev accR, List.rev accC)
        | filterPairs (r :: rs, c :: cs, accR, accC) =
            if keep r then filterPairs (rs, cs, r :: accR, c :: accC)
            else filterPairs (rs, cs, accR, accC)
        | filterPairs _ = raise Fail "TwistedRootDatum.inverse_image_simple_factor: mismatch"

      val (roots', coroots') = filterPairs (posRoots, posCoroots, [], [])
    in
      rootdatum_from_positive (roots', coroots')
    end

  fun affine_root_of_factor (trd: t, foldedFactor: rootdatum, tMat: mat) : int list =
    let
      val inv = inverse_image_simple_factor (trd, foldedFactor, tMat)
      val orderUp = order_twist {rd = inv, delta = #delta trd}
      val nf = RootDatum.numberSimpleFactors inv
      val () = RootDatum.free inv
      val () = if nf > 0 then () else raise Fail "TwistedRootDatum.affine_root_of_factor: zero factors"
      val ord = orderUp div nf
      val () = if ord <= 3 then () else raise Fail "TwistedRootDatum.affine_root_of_factor: order > 3"
    in
      if ord = 1 then RootDatum.highestRoot foldedFactor else RootDatum.highestShortRoot foldedFactor
    end
end
