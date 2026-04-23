use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(*
  File: atlas-scripts-sml/test_braid.sml

  Purpose
  - SML translation of `atlas-scripts/test_braid.at`.
  - Given a root datum `rd` and a list of operator matrices (one per simple
    root), test all Coxeter braid relations.

  Atlas correspondence
  - The `.at` version defines `m(rd,i,j)=Cartan[i,j]*Cartan[j,i]` and checks:
      m=0: s_i s_j = s_j s_i
      m=1: s_i s_j s_i = s_j s_i s_j
      m=2: s_i s_j s_i s_j = s_j s_i s_j s_i
      m=3: (s_i s_j)^3 = (s_j s_i)^3

  Representation
  - We use `IntMatrix.mat` (row-major) for matrices. All operators must be
    square and of the same size.
*)

structure Test_braid = struct
  type mat = IntMatrix.mat

  fun matProd (id: mat, ms: mat list) : mat =
    List.foldl (fn (m, acc) => IntMatrix.matMul (acc, m)) id ms

  fun m (rd: RootDatum.t, i: int, j: int) : int =
    let
      val c = RootDatum.cartanMatrix rd
      val a = List.nth (List.nth (c, i), j)
      val b = List.nth (List.nth (c, j), i)
    in
      a * b
    end

  fun test_braid (rd: RootDatum.t, ops: mat list) : bool * (int * int * bool) list =
    let
      val n = RootDatum.semisimpleRank rd
      val () = if length ops = n then () else raise Fail "Test_braid.test_braid: ops length mismatch"

      val (sz, sz2) = IntMatrix.matShape (hd ops)
      val () = if sz = sz2 then () else raise Fail "Test_braid.test_braid: op not square"
      val () =
        if List.all (fn a => let val (r, c) = IntMatrix.matShape a in r = sz andalso c = sz end) ops then
          ()
        else
          raise Fail "Test_braid.test_braid: operator shapes mismatch"

      val id = IntMatrix.identity sz
      fun atOp i = List.nth (ops, i)

      fun check (i: int, j: int) : bool =
        if i = j then
          matProd (id, [atOp i, atOp j]) = id
        else
          let
            val mij = m (rd, i, j)
          in
            if mij = 0 then
              matProd (id, [atOp i, atOp j]) = matProd (id, [atOp j, atOp i])
            else if mij = 1 then
              matProd (id, [atOp i, atOp j, atOp i]) = matProd (id, [atOp j, atOp i, atOp j])
            else if mij = 2 then
              matProd (id, [atOp i, atOp j, atOp i, atOp j]) = matProd (id, [atOp j, atOp i, atOp j, atOp i])
            else if mij = 3 then
              matProd (id, [atOp i, atOp j, atOp i, atOp j, atOp i, atOp j])
              = matProd (id, [atOp j, atOp i, atOp j, atOp i, atOp j, atOp i])
            else
              raise Fail "Test_braid.test_braid: m>3"
          end

      val results =
        List.concat
          (List.tabulate
             ( n
             , fn i =>
                 List.tabulate (n, fn j =>
                   let
                     val tf = check (i, j)
                   in
                     (i, j, tf)
                   end)
             ))
      val allOk = List.all (fn (_, _, tf) => tf) results
    in
      if not allOk then
        (TextIO.print "test_braid: failed\n"; ())
      else
        ();
      (allOk, results)
    end
end
