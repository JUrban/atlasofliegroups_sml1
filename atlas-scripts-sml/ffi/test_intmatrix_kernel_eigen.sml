use "atlas-scripts-sml/IntMatrix.sml";

structure TestIntMatrixKernelEigen = struct
  fun dot (xs: int list, ys: int list) =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "test_intmatrix: dot mismatch"
    in
      loop (xs, ys, 0)
    end

  fun matVecMul (a: IntMatrix.mat, x: int list) : int list =
    List.map (fn row => dot (row, x)) a

  fun shape (m: IntMatrix.mat) : int * int =
    case m of
      [] => (0, 0)
    | r :: rs =>
        let
          val c = length r
          val () = if List.all (fn r2 => length r2 = c) rs then () else raise Fail "test_intmatrix: ragged"
        in
          (length m, c)
        end

  fun run () =
    let
      val a = [[1, 0], [0, 0]]
      val k = IntMatrix.kernel a
      val (kr, kc) = shape k
      val () = if kr = 2 andalso kc = 1 then () else raise Fail "test_intmatrix: unexpected kernel shape"
      val col0 = List.map (fn row => List.nth (row, 0)) k
      val ax0 = matVecMul (a, col0)
      val () = if ax0 = [0, 0] then () else raise Fail "test_intmatrix: kernel vector not in kernel"

      val id2 = [[1, 0], [0, 1]]
      val e1 = IntMatrix.eigenLattice (id2, 1)
      val e2 = IntMatrix.eigenLattice (id2, ~1)
      val () = if shape e1 = (2, 2) then () else raise Fail "test_intmatrix: eigen +1 unexpected"
      val () = if shape e2 = (2, 0) then () else raise Fail "test_intmatrix: eigen -1 unexpected"
    in
      TextIO.print "ok\n"
    end
end

val () = TestIntMatrixKernelEigen.run ();
