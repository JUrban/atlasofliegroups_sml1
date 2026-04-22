use "atlas-scripts-sml/W_orbit.sml";
use "atlas-scripts-sml/Coordinates.sml";

(* Smoke test for `W_parabolic_iterator_with_action`:
   - build the reflection representation matrices for A2 on the weight lattice
   - verify the iterator yields 6 elements
   - verify matrix action agrees with word action on a regular weight. *)

val rd = RootDatum.newSimple (#"A", 2, false);
val rho = Coordinates.parseRatWeightText (RootDatum.rhoText rd);
val () = if #den rho = 1 then () else raise Fail "expected integral rho for A2";
val start = #nums rho;
val dim = 2;

fun basis (n: int) : int list list =
  List.tabulate (n, fn i => List.tabulate (n, fn j => if i = j then 1 else 0));

fun matFromColumns (nRows: int, cols: int list list) : IntMatrix.mat =
  let
    fun row i = List.map (fn c => List.nth (c, i)) cols
  in
    List.tabulate (nRows, row)
  end;

(* Reflection matrix for simple reflection i on weights, in the ambient basis. *)
fun reflMat (i: int) : IntMatrix.mat =
  let
    val cols = List.map (fn e => WOrbit.act_word_rtl (rd, [i], e)) (basis dim)
  in
    matFromColumns (dim, cols)
  end;

val gens = [0, 1];
val gens_rep = [reflMat 0, reflMat 1];

val it = WOrbit.W_parabolic_iterator_with_action (rd, gens, dim, gens_rep);

fun collect (acc: (WOrbit.word * IntMatrix.mat) list) =
  case #peek it () of
    NONE => List.rev acc
  | SOME x => (#advance it (); collect (x :: acc));

val xs = collect [];
val () = if length xs = 6 then () else raise Fail ("iterator size mismatch: " ^ Int.toString (length xs));

val () =
  List.app
    (fn (w, act) =>
      let
        val imgWord = WOrbit.act_word_rtl (rd, w, start)
        val imgMat = IntMatrix.matVecMul (act, start)
      in
        if imgWord = imgMat then () else raise Fail "action matrix mismatch"
      end)
    xs;

val () = RootDatum.free rd;
val () = print "OK\n";

