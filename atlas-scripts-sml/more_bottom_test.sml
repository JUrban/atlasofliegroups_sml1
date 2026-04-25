use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/more_bottom_test.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/more_bottom_test.at`.
  - The `.at` script is a large test/driver for “bottom layer” computations and
    related verification pipelines.

  Status
  - Not yet ported: depends on extensive `.at` infrastructure (FPP/Dirac,
    coherent continuation, packets, etc.).
*)

structure More_bottom_test = struct
  fun TODO (_: string) : 'a =
    raise Fail "More_bottom_test: not yet ported"
end

