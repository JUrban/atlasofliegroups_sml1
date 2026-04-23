use "atlas-scripts-sml/misc.sml";

(*
  Smoke test for the root-system helpers in `atlas-scripts-sml/misc.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val rd = RootDatum.newSimple (#"A", 1, false);

val simps = RootDatum.simpleRootsCols rd;
val () = assert "A1 has one simple root" (length simps = 1);
val alpha = hd simps;
val negAlpha = List.map (fn x => ~x) alpha;

val () = assert "alpha is a root" (Misc.is_root (rd, alpha));
val () = assert "negAlpha is a root" (Misc.is_root (rd, negAlpha));
val () = assert "alpha is positive" (Misc.is_posroot (rd, alpha));
val () = assert "negAlpha is not positive" (not (Misc.is_posroot (rd, negAlpha)));

val () = assert "sgn(alpha)=1" (Misc.sgn (rd, alpha) = 1);
val () = assert "sgn(-alpha)=-1" (Misc.sgn (rd, negAlpha) = ~1);

val () = assert "height(alpha)=1" (Misc.height (rd, alpha) = 1);
val () = assert "height(-alpha)=1" (Misc.height (rd, negAlpha) = 1);

val () = assert "posroot_as_sum_of_simple(alpha)=[alpha]"
               (Misc.posroot_as_sum_of_simple (rd, alpha) = [alpha]);
val () = assert "root_as_sum_of_simple(-alpha)=[-alpha]"
               (Misc.root_as_sum_of_simple (rd, negAlpha) = [negAlpha]);

val () = assert "simple_root_summand(alpha)=alpha"
               (Misc.simple_root_summand (rd, alpha) = alpha);
val () = assert "simple_root_summand(-alpha)=-alpha"
               (Misc.simple_root_summand (rd, negAlpha) = negAlpha);

val (p, q) = Misc.root_string (rd, alpha, alpha);
val () = assert "root_string(alpha,alpha)=(0,0)" (p = 0 andalso q = 0);

val () = assert "all_root_index(alpha) present" (Misc.all_root_index (rd, alpha) >= 0);
val () = assert "simple_root_index(alpha)=0" (Misc.simple_root_index (rd, alpha) = 0);
val () = assert "pm_simple_root_index(-alpha)=0" (Misc.pm_simple_root_index (rd, negAlpha) = 0);

val () = RootDatum.free rd;

val _ = print "ok\n";

