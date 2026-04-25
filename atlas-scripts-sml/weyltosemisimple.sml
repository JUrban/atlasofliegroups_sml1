use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(*
  File: atlas-scripts-sml/weyltosemisimple.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/weyltosemisimple.at`.
  - The `.at` script computes certain torus elements / Kac coordinates for
    “good” Weyl-group representatives by working in a root datum with a twist
    automorphism and then folding to an affine datum.

  Status
  - Not yet ported end-to-end. While the SML codebase includes pieces of the
    needed infrastructure (`TwistedRootDatum`, `Affine`, some folding helpers),
    this specific script’s API surface has not been reconstructed and its
    required Levi/dominance helpers are not currently exposed in the same form.
*)

structure WeylToSemisimple = struct
  type rootdatum = RootDatum.t
  type mat = IntMatrix.mat

  type good_data =
    { rd: rootdatum
    , name: string
    , pairs: (int list * int) list
    , d: int
    , delta: mat
    }

  fun TODO (_: string) : 'a =
    raise Fail "WeylToSemisimple: not yet ported"
end

