use "atlas-scripts-sml/lazy_lists.sml";

(*
  File: atlas-scripts-sml/ffi/test_lazy_lists_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/lazy_lists.sml`.
  - Checks basic stream operations and that `memoize` prevents repeated thunk
    evaluation for the same node.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val s = Lazy_lists.series (fn i => i * i);
val () = assert "initial squares" (Lazy_lists.initial 5 s = [0, 1, 4, 9, 16]);
val () = assert "shift/constant_term" (Lazy_lists.constant_term (Lazy_lists.shift (3, s)) = 9);
val () = assert "coefficient" (Lazy_lists.coefficient (4, s) = 16);

(* memoize smoke: first node should be computed once *)
val counter = ref 0;
fun thunk () =
  let
    val () = counter := !counter + 1
  in
    Lazy_lists.InfNode (42, Lazy_lists.InfList thunk)
  end;

val m = Lazy_lists.memoize (Lazy_lists.InfList thunk);
val Lazy_lists.InfNode (h1, _) = Lazy_lists.force m;
val Lazy_lists.InfNode (h2, _) = Lazy_lists.force m;
val () = assert "memoized head" (h1 = 42 andalso h2 = 42);
val () = assert "memoize computes once" (!counter = 1);
