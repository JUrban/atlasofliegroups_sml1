use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamHash.sml";

(*
  File: atlas-scripts-sml/BigUnitaryCache.sml

  Purpose
  - Cache the result of “is unitary?” tests for parameters.
  - Maintains two `ParamHash` tables: one for known unitary parameters and one
    for known non-unitary parameters.

  Notes
  - This avoids repeated expensive unitary checks during the bottom-layer scan.
  - `ParamHash` clones on insertion, so the cache owns its stored handles.
*)
structure BigUnitaryCache = struct
  type param = AtlasFFI.param

  type t =
    { unitary: ParamHash.t
    , nonunitary: ParamHash.t
    }

  (* Create a new cache with the given initial bucket count. *)
  fun create bucketCount : t =
    { unitary = ParamHash.create bucketCount
    , nonunitary = ParamHash.create bucketCount
    }

  (* Accessor for the unitary table. *)
  fun uhash ({unitary, ...}: t) = unitary
  (* Accessor for the non-unitary table. *)
  fun nuhash ({nonunitary, ...}: t) = nonunitary

  (* Lookup index in the unitary table (`~1` if absent). *)
  fun ulookup (t: t) (p: param) : int =
    ParamHash.lookup (#unitary t) p

  (* Lookup index in the non-unitary table (`~1` if absent). *)
  fun nulookup (t: t) (p: param) : int =
    ParamHash.lookup (#nonunitary t) p

  (* Insert-or-match in the unitary table; returns index. *)
  fun umatch (t: t) (p: param) : int =
    ParamHash.match (#unitary t) p

  (* Insert-or-match in the non-unitary table; returns index. *)
  fun numatch (t: t) (p: param) : int =
    ParamHash.match (#nonunitary t) p

  (* Clear both caches without freeing stored handles. *)
  fun clear (t: t) =
    (ParamHash.clear (#unitary t); ParamHash.clear (#nonunitary t))

  (* Free all cached handles and clear the tables. *)
  fun freeAll (t: t) =
    (ParamHash.freeAll (#unitary t); ParamHash.freeAll (#nonunitary t))

  (* Generic cache wrapper:
       - if `p` is in the unitary table, return true
       - if `p` is in the nonunitary table, return false
       - otherwise compute `tester p`, record it, and return it. *)
  fun check (t: t) (tester: param -> bool) (p: param) : bool =
    if ulookup t p >= 0 then
      true
    else if nulookup t p >= 0 then
      false
    else
      let
        val ok = tester p
        val _ = if ok then umatch t p else numatch t p
      in
        ok
      end

  (* Cached wrapper for `AtlasFFI.atlas_param_is_unitary`. *)
  fun check_unitary (t: t) (p: param) : bool =
    check t (fn q => AtlasFFI.atlas_param_is_unitary q = 1) p
end
