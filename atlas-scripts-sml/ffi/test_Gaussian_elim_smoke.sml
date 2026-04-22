use "atlas-scripts-sml/Gaussian_elim.sml";

fun r (n: int) = BigRat.fromInt n;
fun eqRat (a, b) = BigRat.equal (a, b);

fun eqVec (xs, ys) =
  length xs = length ys andalso List.all eqRat (ListPair.zipEq (xs, ys));

fun eqMatCols (a: Gaussian_elim.mat, b: Gaussian_elim.mat) =
  length a = length b andalso List.all eqVec (ListPair.zipEq (a, b));

(* Identity solve. *)
val id2 : Gaussian_elim.mat = [[r 1, r 0], [r 0, r 1]];
val b : Gaussian_elim.vec = [r 2, r 3];
val x = Gaussian_elim.a_solution (id2, b);
val () = if eqVec (x, [r 2, r 3]) then () else raise Fail "identity solve failed";

(* Inverse + det for an upper triangular 2x2 matrix. *)
val a : Gaussian_elim.mat = [[r 1, r 0], [r 1, r 1]]; (* columns: (1,0), (1,1) *)
val detA = Gaussian_elim.det a;
val () = if eqRat (detA, r 1) then () else raise Fail "det mismatch";

val invA = Gaussian_elim.inverse a;
val expectedInv : Gaussian_elim.mat = [[r 1, r 0], [r ~1, r 1]];
val () = if eqMatCols (invA, expectedInv) then () else raise Fail "inverse mismatch";

(* No-solution case: [1 0; 0 0] * x = [0;1]. *)
val sing : Gaussian_elim.mat = [[r 1, r 0], [r 0, r 0]];
val bbad : Gaussian_elim.vec = [r 0, r 1];
val sol = Gaussian_elim.full_solve (sing, bbad);
val () = (case sol of NONE => () | SOME _ => raise Fail "expected no solution");

val () = print "OK\n";

