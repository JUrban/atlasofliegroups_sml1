use "atlas-scripts-sml/Lattice.sml";

val a = [[1, 1], [0, 2]];
val b = [3, 4];
val x = Lattice.solve (a, b);
val () =
  case x of
    NONE => print "no solution\n"
  | SOME xs => print ("x=" ^ String.concatWith "," (List.map Int.toString xs) ^ "\n");

val u = {den = 1, nums = b};
val y = Lattice.vec_solve (a, u);
val () =
  case y of
    NONE => print "no vec_solve\n"
  | SOME ys => print ("vec_solve=" ^ String.concatWith "," (List.map Int.toString ys) ^ "\n");

val u2 = {den = 2, nums = [6, 8]};
val y2 = Lattice.vec_solve (a, u2);
val () =
  case y2 of
    NONE => print "no vec_solve u2\n"
  | SOME ys => print ("vec_solve_u2=" ^ String.concatWith "," (List.map Int.toString ys) ^ "\n");
