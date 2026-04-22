use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/affine.sml

  Purpose
  - Minimal SML analogue of `atlas-scripts/affine.at`.
  - Provides just enough “affine datum” structure for the folded-FPP routines.
*)
structure Affine = struct
  type vec = int list

  (* Affine datum: one or more affine roots/coroots attached to a root datum. *)
  type affine_datum = {affine_coroots: vec list, rd: RootDatum.t, affine_roots: vec list}

  (* Highest short roots of `rd` (delegates to `RootDatum`). *)
  fun highest_short_roots (rd: RootDatum.t) : vec list =
    RootDatum.highestShortRoots rd

  (* A choice of highest short root. *)
  fun highest_short_root (rd: RootDatum.t) : vec =
    RootDatum.highestShortRoot rd

  (* Swap affine roots and coroots (dualize the affine datum). *)
  fun dual (ad: affine_datum) : affine_datum =
    {affine_coroots = #affine_roots ad, rd = #rd ad, affine_roots = #affine_coroots ad}

  (* Build an affine datum from a chosen affine root `alpha`. *)
  fun affine_datum_from_root (rd: RootDatum.t, alpha: vec) : affine_datum =
    {affine_coroots = [RootDatum.coroot rd alpha], rd = rd, affine_roots = [alpha]}

  (* Default affine datum using the highest root. *)
  fun affine_datum (rd: RootDatum.t) : affine_datum =
    affine_datum_from_root (rd, RootDatum.highestRoot rd)

  (* Convenience constructor from a semisimple Lie type. *)
  fun affine_datum_from_lieType (lt: LieType.t) : affine_datum =
    affine_datum (RootDatum.fromLieType lt)
end
