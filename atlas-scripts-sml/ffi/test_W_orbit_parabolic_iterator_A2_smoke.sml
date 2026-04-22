use "atlas-scripts-sml/W_orbit.sml";
use "atlas-scripts-sml/Coordinates.sml";

(* Smoke test for `W_parabolic_iterator`:
   For A2, |W|=6 and iterating over gens=[0,1] should yield 6 distinct elements. *)

val rd = RootDatum.newSimple (#"A", 2, false);
val rho = Coordinates.parseRatWeightText (RootDatum.rhoText rd);
val () = if #den rho = 1 then () else raise Fail "expected integral rho for A2";
val start = #nums rho;

val it = WOrbit.W_parabolic_iterator (rd, [0, 1]);

fun collect (acc: WOrbit.word list) : WOrbit.word list =
  case #peek it () of
    NONE => List.rev acc
  | SOME w => (#advance it (); collect (w :: acc));

val ws = collect [];
val () = if length ws = 6 then () else raise Fail ("iterator size mismatch: " ^ Int.toString (length ws));

(* Check distinctness by action on a regular weight. *)
val images = List.map (fn w => WOrbit.act_word_rtl (rd, w, start)) ws;
val uniq = Basic.sort_u Sort.rlex_leq images;
val () = if length uniq = 6 then () else raise Fail "iterator produced duplicates (by action)";

val () = RootDatum.free rd;
val () = print "OK\n";
