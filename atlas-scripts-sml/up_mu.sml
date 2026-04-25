use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/up_mu.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/up_mu.at`.
  - The `.at` script builds a memoized “up-mu” hash table for iterating certain
    K-type raising operations (used in Dirac/best-Dirac computations).

  Status
  - Not yet ported: the algorithm relies on Dirac shifters, anti-finalization,
    K-type hashing across packets, and other heavy machinery not yet available
    in the SML port.
*)

structure Up_mu = struct
  fun TODO (_: string) : 'a =
    raise Fail "Up_mu: not yet ported"
end

