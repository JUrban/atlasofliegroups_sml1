use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/ellipticExceptional.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/ellipticExceptional.at`.
  - The `.at` script contains exceptional-type elliptic conjugacy-class data.

  Status
  - Not yet ported: depends on exceptional Weyl class-table constructions and
    related data not currently implemented in SML.
*)

structure EllipticExceptional = struct
  fun TODO (_: string) : 'a =
    raise Fail "EllipticExceptional: not yet ported"
end

