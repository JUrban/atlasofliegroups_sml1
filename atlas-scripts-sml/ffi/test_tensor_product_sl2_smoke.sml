use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/tensor_product_sl2.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);

(* Build two arbitrary K-types in the distinguished fiber x0=0. *)
val mu = KType.newFromXAndLambdaRhoText (g, 0, "1 1");
val tau = KType.newFromXAndLambdaRhoText (g, 0, "1 2");

val prod = Tensor_product_sl2.tensor_sl2 (mu, tau);
val () = assert "tensor_sl2 returned handle" (prod <> Foreign.Memory.null);
val () = ignore (KType.lambdaRhoText prod);

val () = KType.free prod;
val () = KType.free tau;
val () = KType.free mu;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK: tensor_product_sl2_smoke\n";

