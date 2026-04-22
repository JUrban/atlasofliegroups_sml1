use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(* Minimal SML analogue of `atlas-scripts/twisted_root_datum.at`:
   currently only implements `cyclic_twist`. *)
structure TwistedRootDatum = struct
  type mat = IntMatrix.mat
  type rootdatum = RootDatum.t
  type t = {rd: rootdatum, delta: mat}

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
end

