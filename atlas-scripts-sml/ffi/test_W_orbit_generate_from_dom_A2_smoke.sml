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

(* from_dominant + W_orbit should work for a non-dominant orbit element too. *)
val v = WOrbit.act_word_rtl (rd, [0], start);
val (witness, dom) = WOrbit.from_dominant_vec (rd, v);
val () = if WOrbit.is_dominant (rd, dom) then () else raise Fail "from_dominant produced non-dominant";
val () = if dom = start then () else raise Fail "A2: expected dom=rho";
val () =
  if WOrbit.act_word_rtl (rd, witness, dom) = v then
    ()
  else
    raise Fail "from_dominant witness failed";

val orb2 = WOrbit.W_orbit (rd, v);
val () = if length orb2 = 6 then () else raise Fail "W_orbit size mismatch on non-dominant input";

val () = RootDatum.free rd;
val () = print "OK\n";
