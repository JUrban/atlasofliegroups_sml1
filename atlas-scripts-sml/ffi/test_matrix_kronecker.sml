use "atlas-scripts-sml/MatrixAT.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val a = [[1, 2], [3, 4]];
val b = [[0, 5], [6, 7]];
val k = MatrixAT.kronecker_product (a, b);

val expected =
  [ [0, 5, 0, 10]
  , [6, 7, 12, 14]
  , [0, 15, 0, 20]
  , [18, 21, 24, 28]
  ];

val _ = assert "kronecker_product matches expected" (k = expected);
val _ = assert "Kronecker_product alias matches" (MatrixAT.Kronecker_product (a, b) = expected);

val _ = print "ok\n";

