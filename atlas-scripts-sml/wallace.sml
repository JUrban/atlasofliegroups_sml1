use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/wallace.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/wallace.at`.
  - The `.at` script is an example/driver for truncated induction of the sign
    representation from a Levi Weyl group into a larger symplectic Weyl group.

  Status
  - Not yet ported: this depends on `truncated_induction.at` and on a much
    richer `CharacterTable` API (viewing, inducing sign, truncation) than the
    current SML character-table layer provides.

  Policy
  - We keep the entry points as stubs so other ports can refer to them without
    depending on `.at` scripts.
*)

structure Wallace = struct
  val wallace_verbose : bool ref = ref true

  fun wallace (n: int, k: int) : string =
    let
      val _ = (n, k)
    in
    raise Fail "Wallace.wallace: not yet ported"
    end

  fun grommit (n: int, k: int) : string =
    let
      val _ = (n, k)
    in
    raise Fail "Wallace.grommit: not yet ported"
    end

  fun grommit_with_character_table (ct: unit, k: int) : string =
    let
      val _ = (ct, k)
    in
    raise Fail "Wallace.grommit(CharacterTable,k): not yet ported"
    end
end
