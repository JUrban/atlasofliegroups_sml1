use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_lambdas_fold.sml";
use "atlas-scripts-sml/F4_FPP_lambdas.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

fun parseInts s =
  let
    fun toInt tok =
      case Int.fromString tok of
        SOME n => n
      | NONE => raise Fail ("bad int token: " ^ tok)
  in
    List.map toInt (String.tokens Char.isSpace s)
  end

fun key (u: Lattice.ratvec) : int list =
  let
    val u = Lattice.ratvecNormalize u
  in
    #den u :: #nums u
  end

fun noReps us = Basic.sort_u_by (key, Sort.rlex_leq) us

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val kgbSize = AtlasFFI.atlas_group_kgb_size g;

val computed = FPP_lambdas_fold.FPP_lambdas_table g;
val file = F4_FPP_lambdas.load kgbSize;

fun fromFile x =
  noReps
    (List.map
       (fn {numsText, denom} =>
         let
           val nums = parseInts numsText
         in
           Lattice.ratvecNormalize {den = denom, nums = nums}
         end)
       (Array.sub (file, x)));

fun checkX x =
  let
    val c = noReps (Array.sub (computed, x))
    val e = fromFile x
  in
    if c = e then ()
    else raise Fail ("F4 FPP_lambdas mismatch at x=" ^ Int.toString x ^ " computed=" ^ Int.toString (length c) ^ " expected=" ^ Int.toString (length e))
  end;

val () = List.app checkX (List.tabulate (kgbSize, fn i => i));

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

