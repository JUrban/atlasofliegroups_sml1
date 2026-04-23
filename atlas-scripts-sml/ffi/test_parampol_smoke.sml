use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/extParamPol.sml";

(*
  Smoke test for the new `Split`, `ParamPol`, and `ExtParamPol` infrastructure.

  Intended properties checked (quickly):
  - `ParamPol.addTermMove` merges equal parameters and frees the duplicate.
  - scaling by `(1-s)` annihilates `(1+s)` (since `s^2=1`) and drops the term.
  - basic `ExtParamPol` construction from `(param,type)` works and frees cleanly.
*)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ();

val xOpen = AtlasFFI.atlas_group_kgb_size g - 1;
val rho = Representations.rho g;
val nu0 = Representations.ratvecZero (AtlasFFI.atlas_group_rank g);

(* Build two equal-but-distinct parameter handles. *)
val p1 = Representations.parameter (g, xOpen, rho, nu0);
val p2 = Representations.parameter (g, xOpen, rho, nu0);

val poly = ParamPol.create ();
val () = ParamPol.addTermMove (poly, Split.one, p1);
val () = ParamPol.addTermMove (poly, Split.s, p2); (* should merge with the first term *)

val () = ParamPol.scaleInPlace (poly, Split.one_minus_s); (* (1-s)*(1+s)=0 *)
val () = if ParamPol.isEmpty poly then () else raise Fail ("ParamPol smoke: expected empty, got: " ^ ParamPol.toString poly);
val () = ParamPol.free poly;

(* ExtParamPol: just check that basic constructors work and clean up. *)
val p3 = Representations.parameter (g, xOpen, rho, nu0);
val ep = ExtParamPol.extParamPol_of_param_type_move (p3, 1);
val () = ExtParamPol.free ep;

val () = AtlasFFI.atlas_group_free g;

