use "atlas-scripts-sml/IntMatrix.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

fun isDiagonal (m: IntMatrix.mat) : bool =
  let
    val (n, k) = IntMatrix.matShape m
    fun entry i j = List.nth (List.nth (m, i), j)
    fun ok i j =
      if i = j then true else entry i j = 0
  in
    List.all (fn i => List.all (fn j => ok i j) (List.tabulate (k, fn x => x))) (List.tabulate (n, fn x => x))
  end

val a = [[2, 4], [0, 6], [0, 0]]; (* 3x2 *)
val (ds, row, col) = IntMatrix.diagonalize a;
val d = IntMatrix.matMul (row, IntMatrix.matMul (a, col));
val _ = assert "diagonalized is diagonal" (isDiagonal d);
val _ = assert "diag length matches n_cols" (length ds = 2);

val _ = print "ok\n";

