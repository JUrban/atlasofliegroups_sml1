use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/ParamReduce.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

val p = AtlasFFI.atlas_param_trivial g;
val p2 = AtlasFFI.atlas_param_clone p;

val rs0 = ParamReduce.reduce [];
val _ = assert "reduce([]) empty" (length rs0 = 0);

val rs = ParamReduce.reduce [p, p2];
val _ = print ("reduce([p,p]) count = " ^ Int.toString (length rs) ^ "\n");
val _ = assert "reduce([p,p]) nonempty" (length rs > 0);

val _ = assert "reduce outputs final" (List.all (fn q => AtlasFFI.atlas_param_is_final q = 1) rs);

fun pairwiseDistinct [] = true
  | pairwiseDistinct (x :: xs) =
      List.all (fn y => AtlasFFI.atlas_param_equivalent (x, y) = 0) xs andalso pairwiseDistinct xs;
val _ = assert "reduce outputs inequivalent" (pairwiseDistinct rs);

val _ = ParamReduce.freeAll rs;
val _ = AtlasFFI.atlas_param_free p2;
val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

