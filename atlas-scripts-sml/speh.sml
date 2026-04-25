use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/speh.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/speh.at`.
  - The `.at` script defines constructors for Speh representations for
    `GL(2n,R)` via theta-stable parabolic induction from `GL(n,C)`.

  Status
  - Not yet implemented: the SML port currently lacks the `.at`-level
    theta-induction interface (`theta_induce_irreducible`) and the richer
    parabolic/Levi object model used by `speh.at`.

  Notes
  - This file exists so downstream script translations can `use` it and type
    check, even though the computational content is pending.
*)

structure Speh = struct
  type param = AtlasFFI.param

  fun speh_0 (n: int, k0: int) : param =
    let
      val _ = (n, k0)
    in
    raise Fail "Speh.speh_0: not yet ported"
    end

  fun Speh_0 (n: int, k: int) : param =
    let
      val _ = (n, k)
    in
    raise Fail "Speh.Speh_0: not yet ported"
    end

  fun Speh (n: int, k: int) : param =
    let
      val _ = (n, k)
    in
    raise Fail "Speh.Speh: not yet ported"
    end

  fun speh_long (n: int, k0: int) : string =
    let
      val _ = (n, k0)
    in
    raise Fail "Speh.speh_long: not yet ported"
    end
end
