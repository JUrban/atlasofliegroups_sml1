use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/representations.sml";

(*
  File: atlas-scripts-sml/ffi/test_representations_discrete_series_smoke.sml

  Purpose
  - Smoke test for the `discrete_series` constructors in `representations.sml`.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_representations_discrete_series_smoke.sml`
*)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val lam = Representations.rho g
val p = Representations.discrete_series (g, lam)
val () = if p = Foreign.Memory.null then raise Fail "discrete_series returned null" else ()

(* Basic sanity: it should at least be a well-formed parameter. *)
val () = ignore (AtlasFFI.atlas_param_x p)
val () = ignore (AtlasFFI.atlas_param_lambda_text p)
val () = ignore (AtlasFFI.atlas_param_nu_text p)

val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g
val () = TextIO.print "OK\n"

