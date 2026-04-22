use "atlas-scripts-sml/TWOHEIGHTS.sml";

(* In type A2, sum_{alpha>0} ht(alpha) = 1 + 1 + 2 = 4, so HT(rho) = 4. *)

val rd = RootDatum.newSimple (#"A", 2, false);
val rhoTxt = RootDatum.rhoText rd;
val rhoP = Coordinates.parseRatWeightText rhoTxt;
val rho : Lattice.ratvec = Lattice.ratvecNormalize {den = #den rhoP, nums = #nums rhoP};

val () =
  case TWOHEIGHTS.HT_int (rd, rho) of
    SOME 4 => ()
  | SOME n => raise Fail ("unexpected HT(rho) in A2: " ^ Int.toString n)
  | NONE => raise Fail "expected integral HT(rho) in A2";

val () = RootDatum.free rd;
val () = print "OK\n";

