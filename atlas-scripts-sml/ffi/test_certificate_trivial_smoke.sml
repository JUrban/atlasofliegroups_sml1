use "atlas-scripts-sml/certificate.sml";

(*
  File: atlas-scripts-sml/ffi/test_certificate_trivial_smoke.sml

  Purpose
  - Smoke test for `Certificate.nonunitarity_certificate` on the trivial
    representation (should be unitary with empty certificate list).
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val () = assert "group new" (g <> Foreign.Memory.null);

val p = AtlasFFI.atlas_param_trivial g;
val () = assert "param trivial" (p <> Foreign.Memory.null);

val (ok, cert) = Certificate.nonunitarity_certificate p;
val () = assert "trivial is unitary" ok;
val () = assert "empty certificate" (null cert);

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

