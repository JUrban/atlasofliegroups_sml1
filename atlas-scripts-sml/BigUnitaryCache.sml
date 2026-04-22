use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamHash.sml";

structure BigUnitaryCache = struct
  type param = AtlasFFI.param

  type t =
    { unitary: ParamHash.t
    , nonunitary: ParamHash.t
    }

  fun create bucketCount : t =
    { unitary = ParamHash.create bucketCount
    , nonunitary = ParamHash.create bucketCount
    }

  fun uhash ({unitary, ...}: t) = unitary
  fun nuhash ({nonunitary, ...}: t) = nonunitary

  fun ulookup (t: t) (p: param) : int =
    ParamHash.lookup (#unitary t) p

  fun nulookup (t: t) (p: param) : int =
    ParamHash.lookup (#nonunitary t) p

  fun umatch (t: t) (p: param) : int =
    ParamHash.match (#unitary t) p

  fun numatch (t: t) (p: param) : int =
    ParamHash.match (#nonunitary t) p

  fun clear (t: t) =
    (ParamHash.clear (#unitary t); ParamHash.clear (#nonunitary t))

  fun freeAll (t: t) =
    (ParamHash.freeAll (#unitary t); ParamHash.freeAll (#nonunitary t))

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

  fun check_unitary_c_form (t: t) (p: param) : bool =
    check t (fn q => AtlasFFI.atlas_param_is_unitary_c_form q = 1) p
end

