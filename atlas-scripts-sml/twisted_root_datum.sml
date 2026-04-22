use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(* Minimal SML analogue of `atlas-scripts/twisted_root_datum.at`:
   currently only implements `cyclic_twist`. *)
structure TwistedRootDatum = struct
  type mat = IntMatrix.mat
  type rootdatum = RootDatum.t
  type t = {rd: rootdatum, delta: mat}

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
  fun pre_folded (_: t) : mat * mat =
    raise Fail "TwistedRootDatum.pre_folded: unimplemented"

  fun folded (_: t) : rootdatum * mat =
    raise Fail "TwistedRootDatum.folded: unimplemented"
end
