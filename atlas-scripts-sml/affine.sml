use "atlas-scripts-sml/RootDatum.sml";

(* Minimal SML analogue of `atlas-scripts/affine.at`. *)
structure Affine = struct
  type vec = int list

  type affine_datum = {affine_coroots: vec list, rd: RootDatum.t, affine_roots: vec list}

  fun highest_short_roots (rd: RootDatum.t) : vec list =
    [RootDatum.highestShortRoot rd]

  fun highest_short_root (rd: RootDatum.t) : vec =
    RootDatum.highestShortRoot rd

  fun affine_datum_from_root (rd: RootDatum.t, alpha: vec) : affine_datum =
    {affine_coroots = [RootDatum.coroot rd alpha], rd = rd, affine_roots = [alpha]}

  fun affine_datum (rd: RootDatum.t) : affine_datum =
    affine_datum_from_root (rd, RootDatum.highestRoot rd)
end

