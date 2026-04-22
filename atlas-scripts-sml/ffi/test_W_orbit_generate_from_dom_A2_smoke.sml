use "atlas-scripts-sml/W_orbit.sml";
use "atlas-scripts-sml/Coordinates.sml";

(* Smoke test: for A2, rho is regular, so its W-orbit has size |W| = 6. *)

val rd = RootDatum.newSimple (#"A", 2, false);
val rho = Coordinates.parseRatWeightText (RootDatum.rhoText rd);
val () = if #den rho = 1 then () else raise Fail "expected integral rho for A2";
val start = #nums rho;

val orbit = WOrbit.generate_from_dom_simples (rd, WOrbit.allSimples rd, start);

val n = length orbit;
val () = if n = 6 then () else raise Fail ("A2 orbit size mismatch: " ^ Int.toString n);

val uniq = Basic.sort_u_by (#1, Sort.rlex_leq) orbit;
val () = if length uniq = n then () else raise Fail "orbit has duplicates";

val () =
  List.app
    (fn (v, w) =>
      let
        val v' = WOrbit.act_word_rtl (rd, w, start)
      in
        if v' = v then () else raise Fail "witness word check failed"
      end)
    orbit;

val () = RootDatum.free rd;
val () = print "OK\n";

