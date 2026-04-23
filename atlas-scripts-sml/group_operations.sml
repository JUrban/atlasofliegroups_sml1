use "atlas-scripts-sml/MatrixAT.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/group_operations.sml

  Purpose
  - Very small SML analogue of the `RootDatum`-level parts of
    `atlas-scripts/group_operations.at`.
  - This module exists to support line-by-line ports of scripts that use:
      - direct products of complex root data (`*(RootDatum,RootDatum)`)
      - the radical / maximal central torus of a complex root datum

  Scope (incremental)
  - Implemented:
      - `mul` (`*`): direct product of root data (block-diagonal on roots/coroots)
      - `radical`: central torus root datum (no roots) of rank `rank-ssrank`
  - Not implemented:
      - `InnerClass` / `RealForm` operations (not yet a first-class SML type)
      - derived/mod-central-torus helpers (Atlas interpreter built-ins)

  Ownership
  - Returned `RootDatum.t` handles are newly allocated; callers must free them.
*)

structure GroupOperations = struct
  type rootdatum = RootDatum.t

  (* Direct product of root data: delegates to `RootDatum.mul`. *)
  fun mul (r: rootdatum, s: rootdatum) : rootdatum =
    RootDatum.mul (r, s)

  infix 7 *
  val op * = mul

  (* Radical (maximal central torus) of a complex group as a separate root datum. *)
  fun radical (rd: rootdatum) : rootdatum =
    let
      val torusRank = RootDatum.rank rd - RootDatum.semisimpleRank rd
      val () = if torusRank >= 0 then () else raise Fail "GroupOperations.radical: negative torus rank"
      val empty = MatrixAT.null (torusRank, 0)
    in
      RootDatum.newFromSimpleMats (empty, empty, false)
    end

  val maximal_central_torus = radical
end

