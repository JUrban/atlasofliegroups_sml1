use "atlas-scripts-sml/FPP_localDirac.sml";
use "atlas-scripts-sml/FPP_lambdas.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/hash.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test that the cached gamma-bucket lookup used by
   `FPP_localDirac.gammas_for_x_lambda_ctx` matches the naive filter definition. *)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val c = FPP_localDirac.create_ctx g;
val tbl = FPP_lambdas.FPP_lambdas_table g;

val kgbSize = AtlasFFI.atlas_group_kgb_size g;
fun findX x =
  if x >= kgbSize then raise Fail "expected some x with nonempty lambda list"
  else let val ls = Array.sub (tbl, x) in if null ls then findX (x + 1) else (x, hd ls) end;
val (x, lambda) = findX 0;

fun ratvecKey (u: Lattice.ratvec) : int list =
  let
    val u = Lattice.ratvecNormalize u
  in
    #den u :: #nums u
  end;

val cached = FPP_localDirac.gammas_for_x_lambda_ctx (c, x, lambda);

(* Naive definition: filter all barycenters by key match. *)
val rank = AtlasFFI.atlas_group_rank g;
val theta =
  AllParameters.parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x));
val onePlus = Lattice.matAdd (Lattice.identity rank, theta);
val k = ratvecKey (Lattice.matVecMulRatvec onePlus lambda);
val naive =
  List.filter
    (fn gamma => ratvecKey (Lattice.matVecMulRatvec onePlus gamma) = k)
    (#barycenters c);

val cachedKeys = List.map ratvecKey cached;
val naiveKeys = List.map ratvecKey naive;

val h1 = Hash.make_vec_hash_data cachedKeys;
val h2 = Hash.make_vec_hash_data naiveKeys;

val () = if List.all (fn kk => Hash.lookup h1 kk >= 0) naiveKeys then () else raise Fail "naive key missing from cached";
val () = if List.all (fn kk => Hash.lookup h2 kk >= 0) cachedKeys then () else raise Fail "cached key missing from naive";

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

