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

(* Parabolic-subgroup variants (gens = [0]) should give orbit size 2. *)
val gens = [0];
val orbG = WOrbit.generate_from_dom_simples (rd, gens, start);
val () = if length orbG = 2 then () else raise Fail "A2 gens=[0] orbit size mismatch";
val () =
  List.app
    (fn (b, w) =>
      if WOrbit.act_word_rtl (rd, w, start) = b then () else raise Fail "gens witness failed")
    orbG;

val reps = WOrbit.stabiliser_quotient_of_dom (rd, gens, start);
val () = if length reps = 2 then () else raise Fail "stabiliser_quotient_of_dom size mismatch";
val () =
  List.app
    (fn w =>
      let
        val b = WOrbit.act_word_rtl (rd, w, start)
      in
        if List.exists (fn (b2, _) => b2 = b) orbG then () else raise Fail "stabiliser_quotient rep not in orbit"
      end)
    reps;

val orbFrom = WOrbit.generate_from (rd, gens, v);
val () = if length orbFrom = 2 then () else raise Fail "generate_from size mismatch";
val () =
  List.app
    (fn (b, w) =>
      if WOrbit.act_word_rtl (rd, w, v) = b then () else raise Fail "generate_from witness failed")
    orbFrom;

(* Coweight variant (A2 is simply laced, so sizes should match). *)
val orbCo = WOrbit.W_orbit_coweight (rd, start);
val () = if length orbCo = 6 then () else raise Fail "W_orbit_coweight size mismatch";

val () = RootDatum.free rd;
val () = print "OK\n";
