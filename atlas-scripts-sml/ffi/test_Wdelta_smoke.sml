(* Smoke test for `atlas-scripts-sml/Wdelta.sml`.

   Exercises the trivial delta=id case where `W^delta = W`, so the constructed
   generators should coincide with the usual simple reflections. *)

use "atlas-scripts-sml/Wdelta.sml";

val rd = RootDatum.newSimple (#"A", 2, false);
val P = IntMatrix.identity (RootDatum.rank rd);

val gens = Wdelta.Wdelta_generatorsWith (rd, rd, P);
val ssr = RootDatum.semisimpleRank rd;

val () =
  if length gens = ssr then
    ()
  else
    raise Fail "Wdelta smoke: wrong number of generators";

val s0 = WeylgroupAT.reflection_matrix_simple (rd, 0);
val s1 = WeylgroupAT.reflection_matrix_simple (rd, 1);

val () =
  if List.nth (gens, 0) = s0 andalso List.nth (gens, 1) = s1 then
    ()
  else
    raise Fail "Wdelta smoke: generator matrices mismatch in delta=id case";

val word = [0, 1, 0];
val wMat = Wdelta.convert_from_W_K_with (rd, gens, word);
val wExpected = IntMatrix.matMul (s0, IntMatrix.matMul (s1, s0));

val () =
  if wMat = wExpected then
    ()
  else
    raise Fail "Wdelta smoke: conversion produced wrong matrix";

val () = RootDatum.free rd;

val () = print "test_Wdelta_smoke: ok\n";
