use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/F4_FPP_barycenters.sml";
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
val computed = noReps (FPP_barycenters_fold.barycenters_all g);
val () = AtlasFFI.atlas_group_free g;

val fileTexts = F4_FPP_barycenters.load ();
val fromFile =
  noReps
    (List.map
       (fn {numsText, denom} =>
         let
           val nums = parseInts numsText
         in
           Lattice.ratvecNormalize {den = denom, nums = nums}
         end)
       fileTexts);

val () =
  if computed = fromFile then ()
  else raise Fail ("barycenter set mismatch: computed=" ^ Int.toString (length computed) ^ " file=" ^ Int.toString (length fromFile));

val () = print "OK\n";

