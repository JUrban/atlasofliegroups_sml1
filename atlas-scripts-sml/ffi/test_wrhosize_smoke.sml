use "atlas-scripts-sml/wrhosize.sml";

(* Smoke test: run `WRhoSize.counts` for type A1 with a tiny `d`. *)

val rd = RootDatum.newSimple (#"A", 1, false);
val () = WRhoSize.counts (rd, 1);
val () = RootDatum.free rd;

