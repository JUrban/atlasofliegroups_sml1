use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/parabolics.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/RootDatum.sml";

fun assertTrue (b: bool, msg: string) = if b then () else raise Fail msg;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val tw = Parabolics.twist g;
val r = AtlasFFI.atlas_group_semisimple_rank g;
val () = assertTrue (length tw = r, "twist: bad length");

(* twist is a diagram involution/permutation *)
val () = List.app (fn t => assertTrue (0 <= t andalso t < r, "twist: oob")) tw;
val () =
  List.app
    (fn i =>
      let
        val t = List.nth (tw, i)
        val tt = List.nth (tw, t)
      in
        assertTrue (tt = i, "twist: not an involution")
      end)
    (List.tabulate (r, fn i => i));

(* delta(simple_root_i) = simple_root_{tw[i]} *)
val rd = AtlasFFI.atlas_group_rootdatum_new g;
val () = assertTrue (rd <> Foreign.Memory.null, "twist: rootdatum_new failed");
val delta = IntMatrix.parseMatText (AtlasFFI.atlas_group_distinguished_involution_text g);
val simples = RootDatum.simpleRootsCols rd;
val () = RootDatum.free rd;

val () =
  List.app
    (fn i =>
      let
        val alpha_i = List.nth (simples, i)
        val alpha_t = List.nth (simples, List.nth (tw, i))
        val img = IntMatrix.matVecMul (delta, alpha_i)
      in
        assertTrue (img = alpha_t, "twist: delta(simple_root) mismatch")
      end)
    (List.tabulate (r, fn i => i));

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

