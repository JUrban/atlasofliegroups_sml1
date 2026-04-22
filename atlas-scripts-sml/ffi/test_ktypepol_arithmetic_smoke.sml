use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for KTypePol arithmetic FFI:
   - clone preserves term data
   - add(pol,pol) doubles coefficients termwise
   - scaleSplit(pol,0,1) (multiply by `s`) swaps (e,s) in each coefficient *)

fun cmpInt (a: int, b: int) : order = Int.compare (a, b);

fun cmpIntList (xs: int list, ys: int list) : order =
  case (xs, ys) of
    ([], []) => EQUAL
  | ([], _) => LESS
  | (_, []) => GREATER
  | (x :: xr, y :: yr) =>
      (case cmpInt (x, y) of
         EQUAL => cmpIntList (xr, yr)
       | ord => ord);

fun cmpTerm (a: KTypePol.term, b: KTypePol.term) : order =
  case cmpInt (#x a, #x b) of
    EQUAL =>
      (case cmpInt (#height a, #height b) of
         EQUAL => cmpIntList (#lambdaRho a, #lambdaRho b)
       | ord => ord)
  | ord => ord;

fun sortTerms (ts: KTypePol.term list) : KTypePol.term list =
  let
    fun insert (t, []) = [t]
      | insert (t, u :: us) =
          (case cmpTerm (t, u) of
             LESS => t :: u :: us
           | _ => u :: insert (t, us));
  in
    List.foldl (fn (t, acc) => insert (t, acc)) [] ts
  end;

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val nu = {den = 1, nums = [1, 0]};
val p = Representations.minimal_spherical_principal_series (g, nu);

val pol = AtlasFFI.atlas_param_full_deform p;
val () = if pol = Foreign.Memory.null then raise Fail ("full_deform failed: " ^ AtlasFFI.atlas_last_error ()) else ();
val rank = AtlasFFI.atlas_group_rank g;

val ts0 = sortTerms (KTypePol.terms (pol, rank));
val () = if length ts0 > 0 then () else raise Fail "expected nonempty deformation";

val polC = KTypePol.clone pol;
val tsC = sortTerms (KTypePol.terms (polC, rank));
val () = if tsC = ts0 then () else raise Fail "clone changed term list";

val pol2 = KTypePol.add (pol, pol);
val ts2 = sortTerms (KTypePol.terms (pol2, rank));
val () = if length ts2 = length ts0 then () else raise Fail "add(pol,pol) changed term count";
val () =
  if ListPair.allEq (fn (t, u) => #x t = #x u andalso #height t = #height u andalso #lambdaRho t = #lambdaRho u) (ts0, ts2)
  then ()
  else raise Fail "add(pol,pol) changed term support";
val () =
  if ListPair.allEq (fn (t, u) => #e u = 2 * #e t andalso #s u = 2 * #s t) (ts0, ts2)
  then ()
  else raise Fail "add(pol,pol) did not double coefficients";

val polS = KTypePol.scaleSplit (pol, 0, 1);
val tsS = sortTerms (KTypePol.terms (polS, rank));
val () = if length tsS = length ts0 then () else raise Fail "scaleSplit(s) changed term count";
val () =
  if ListPair.allEq (fn (t, u) => #x t = #x u andalso #height t = #height u andalso #lambdaRho t = #lambdaRho u) (ts0, tsS)
  then ()
  else raise Fail "scaleSplit(s) changed term support";
val () =
  if ListPair.allEq (fn (t, u) => #e u = #s t andalso #s u = #e t) (ts0, tsS)
  then ()
  else raise Fail "scaleSplit(s) did not swap (e,s)";

val () = KTypePol.free polS;
val () = KTypePol.free pol2;
val () = KTypePol.free polC;
val () = KTypePol.free pol;
val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";

