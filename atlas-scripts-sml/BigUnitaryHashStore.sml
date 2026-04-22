use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryCache.sml";

structure BigUnitaryHashStore = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  type entry = {g: group, cache: BigUnitaryCache.t}

  type t = {entries: entry list ref}

  fun create () : t = {entries = ref []}

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

  fun rf_number (store: t) (g: group) : int =
    let
      val _ = getOrAddEntry store g
      fun loop ([], _) = raise Fail "BigUnitaryHashStore.rf_number: internal error"
        | loop ({g = g2, ...} :: rest, i) = if g2 = g then i else loop (rest, i + 1)
    in
      loop (List.rev (!(#entries store)), 0)
    end

  fun entry_by_number (store: t) (j: int) : entry =
    let
      val es = List.rev (!(#entries store))
      fun nth ([], _) = raise Subscript
        | nth (e :: _, 0) = e
        | nth (_ :: rest, n) = nth (rest, n - 1)
    in
      if j < 0 then raise Subscript else nth (es, j)
    end

  fun uhash (store: t) (g: group) =
    BigUnitaryCache.uhash (#cache (getOrAddEntry store g))

  fun nuhash (store: t) (g: group) =
    BigUnitaryCache.nuhash (#cache (getOrAddEntry store g))

  fun long_match (store: t) (p: param, j: int) : int =
    let
      val {cache, ...} = entry_by_number store j
    in
      BigUnitaryCache.umatch cache p
    end

  fun ulookup (store: t) (p: param, j: int) : int =
    let
      val {cache, ...} = entry_by_number store j
    in
      BigUnitaryCache.ulookup cache p
    end

  fun nulookup (store: t) (p: param, j: int) : int =
    let
      val {cache, ...} = entry_by_number store j
    in
      BigUnitaryCache.nulookup cache p
    end

  fun check_unitary_c_form (store: t) (p: param, j: int) : bool =
    let
      val {cache, ...} = entry_by_number store j
    in
      BigUnitaryCache.check_unitary_c_form cache p
    end

  fun clear (store: t) =
    let
      fun loop [] = ()
        | loop ({cache, ...} :: rest) = (BigUnitaryCache.clear cache; loop rest)
    in
      loop (!(#entries store))
    end

  fun freeAll (store: t) =
    let
      fun loop [] = ()
        | loop ({cache, ...} :: rest) = (BigUnitaryCache.freeAll cache; loop rest)
    in
      loop (!(#entries store));
      #entries store := []
    end
end
