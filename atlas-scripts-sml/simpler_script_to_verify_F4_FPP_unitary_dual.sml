use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryHash.sml";
use "atlas-scripts-sml/F4_FPP_points.sml";
use "atlas-scripts-sml/F4_FPP_barycenters.sml";
use "atlas-scripts-sml/F4_FPP_lambdas.sml";

val G = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val kgbSize = AtlasFFI.atlas_group_kgb_size G;

val barycenters = F4_FPP_barycenters.load ();
val lambdasByX = F4_FPP_lambdas.load kgbSize;

val big_unitary_hash = BigUnitaryHash.create 4096;
val () = F4_FPP_points.loadInto (G, big_unitary_hash);

val () = TextIO.print ("loaded unitary points: " ^ Int.toString (BigUnitaryHash.size big_unitary_hash) ^ "\n");
val () = TextIO.print ("barycenters: " ^ Int.toString (length barycenters) ^ "\n");

val bad = ref 0;
val checked = ref 0;
val maxChecks = 0;

fun checkOne (x: int, lam: F4_FPP_lambdas.ratvec_text, nu: F4_FPP_barycenters.ratvec_text) =
  let
    val p =
      AtlasFFI.atlas_param_new_from_lambda_nu_text
        (G, x, #numsText lam, #denom lam, #numsText nu, #denom nu)
    val () =
      if p = Foreign.Memory.null then
        raise Fail ("parameter construction failed: " ^ AtlasFFI.atlas_last_error ())
      else
        ()
    val u = AtlasFFI.atlas_param_is_unitary_c_form p
    val ok = (u <> 1) orelse BigUnitaryHash.contains big_unitary_hash p
    val () =
      if ok then ()
      else (bad := !bad + 1; TextIO.print "IT'S ALL WRONG!!!\n")
    val () = checked := !checked + 1
    val () = AtlasFFI.atlas_param_free p
  in
    ()
  end

fun loopX x =
  if x >= kgbSize then ()
  else
    let
      val lambdas = Array.sub (lambdasByX, x)
      fun loopLam [] = ()
        | loopLam (lam :: rest) =
            let
              fun loopNu [] = ()
                | loopNu (nu :: nus) =
                    if maxChecks > 0 andalso !checked >= maxChecks then ()
                    else (checkOne (x, lam, nu); loopNu nus)
            in
              loopNu barycenters;
              loopLam rest
            end
    in
      loopLam lambdas;
      loopX (x + 1)
    end

val () = if maxChecks <= 0 then () else loopX 0;

val () = TextIO.print ("checked=" ^ Int.toString (!checked) ^ " bad=" ^ Int.toString (!bad) ^ "\n");

val () = BigUnitaryHash.freeAll big_unitary_hash;
val () = AtlasFFI.atlas_group_free G;
