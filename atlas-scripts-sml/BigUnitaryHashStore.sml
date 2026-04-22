use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryCache.sml";

(*
  File: atlas-scripts-sml/BigUnitaryHashStore.sml

  Purpose
  - Maintain per-group unitary caches keyed by `AtlasFFI.group` handles.
  - The `.at` scripts often treat “current group” as an implicit global; this
    store provides an explicit mapping `group -> BigUnitaryCache`.

  Notes
  - Groups are compared by pointer equality (`g2 = g`), so callers must use
    stable group handles and avoid freeing them while still stored here.
*)
structure BigUnitaryHashStore = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  type entry = {g: group, cache: BigUnitaryCache.t}

  type t = {entries: entry list ref}

  (* Create an empty store. *)
  fun create () : t = {entries = ref []}

  (* Find the cache entry for `g`, creating one if missing. *)
  fun getOrAddEntry (store: t) (g: group) : entry =
    let
      fun find [] = NONE
        | find ({g = g2, cache} :: rest) =
            if g2 = g then SOME {g = g2, cache = cache} else find rest
    in
      case find (!(#entries store)) of
        SOME e => e
      | NONE =>
          let
            val e = {g = g, cache = BigUnitaryCache.create 4096}
            val () = #entries store := e :: !(#entries store)
          in
            e
          end
    end

  (* Stable numbering of groups as they are inserted (used by some scripts). *)
  fun rf_number (store: t) (g: group) : int =
    let
      val _ = getOrAddEntry store g
      fun loop ([], _) = raise Fail "BigUnitaryHashStore.rf_number: internal error"
        | loop ({g = g2, ...} :: rest, i) = if g2 = g then i else loop (rest, i + 1)
    in
      loop (List.rev (!(#entries store)), 0)
    end

  (* Fetch an entry by its insertion number (0-based). *)
  fun entry_by_number (store: t) (j: int) : entry =
    let
      val es = List.rev (!(#entries store))
      fun nth ([], _) = raise Subscript
        | nth (e :: _, 0) = e
        | nth (_ :: rest, n) = nth (rest, n - 1)
    in
      if j < 0 then raise Subscript else nth (es, j)
    end

  (* Access the unitary hash for a given group, creating it if needed. *)
  fun uhash (store: t) (g: group) =
    BigUnitaryCache.uhash (#cache (getOrAddEntry store g))

  (* Access the non-unitary hash for a given group, creating it if needed. *)
  fun nuhash (store: t) (g: group) =
    BigUnitaryCache.nuhash (#cache (getOrAddEntry store g))

  (* Insert-or-match into the unitary cache for entry `j`. *)
  fun long_match (store: t) (p: param, j: int) : int =
    let
      val {cache, ...} = entry_by_number store j
    in
      BigUnitaryCache.umatch cache p
    end

  (* Lookup in the unitary cache for entry `j` (`~1` if absent). *)
  fun ulookup (store: t) (p: param, j: int) : int =
    let
      val {cache, ...} = entry_by_number store j
    in
      BigUnitaryCache.ulookup cache p
    end

  (* Lookup in the non-unitary cache for entry `j` (`~1` if absent). *)
  fun nulookup (store: t) (p: param, j: int) : int =
    let
      val {cache, ...} = entry_by_number store j
    in
      BigUnitaryCache.nulookup cache p
    end

  (* Cached unitary check for entry `j`. *)
  fun check_unitary (store: t) (p: param, j: int) : bool =
    let
      val {cache, ...} = entry_by_number store j
    in
      BigUnitaryCache.check_unitary cache p
    end

  (* Clear all caches without freeing stored handles. *)
  fun clear (store: t) =
    let
      fun loop [] = ()
        | loop ({cache, ...} :: rest) = (BigUnitaryCache.clear cache; loop rest)
    in
      loop (!(#entries store))
    end

  (* Free all cached handles and remove all entries. *)
  fun freeAll (store: t) =
    let
      fun loop [] = ()
        | loop ({cache, ...} :: rest) = (BigUnitaryCache.freeAll cache; loop rest)
    in
      loop (!(#entries store));
      #entries store := []
    end
end
