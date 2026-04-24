use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/character_tables.sml";

(*
  File: atlas-scripts-sml/families.sml

  Purpose
  - SML translation of `atlas-scripts/families.at`.
  - Computes “families” of irreducible characters in a `CharacterTable`
    according to the Atlas-provided `special` representative map.

  Atlas correspondence
  - In the `.at` scripts, a `CharacterTable ct` provides:
      - `ct.special(i)` mapping an irrep index `i` to the special index of its family
      - `ct.is_special_representation(i)` for testing if `i` is special
  - This port uses the data stored in `CharacterTables.CharacterTable.t`:
      - `CharacterTables.special(ct,i)` is the family representative index
      - An index `i` is “special” iff `CharacterTables.special(ct,i) = i`

  API
  - `Families.sort_special_first(ct, reps)`:
      given a list of irrep indices in the same family, return a list with the
      unique special element first, followed by the remaining indices sorted.
  - `Families.families(ct)`:
      returns a list of families; each family is returned in the
      `sort_special_first` order.
  - `Families.family(ct, i)`:
      return the family containing index `i`.

  Notes
  - The original `.at` implementation assumes `reps` contains exactly one
    special representation and asserts that; we keep the same behavior.
*)

structure Families = struct
  structure CT = CharacterTables

  type ct = CT.CharacterTable.t

  fun is_special_representation (ct: ct, i: int) : bool =
    CT.special (ct, i) = i

  fun sort_special_first (ct: ct, reps: int list) : int list =
    let
      val specials = List.filter (fn i => is_special_representation (ct, i)) reps
      val () =
        if length specials = 1 then
          ()
        else
          raise Fail ("Families.sort_special_first: expected exactly 1 special, got " ^ Int.toString (length specials))
      val sp = hd specials
      val rest = List.filter (fn i => i <> sp) reps
      val restSorted = Basic.sort (op <=) rest
    in
      sp :: restSorted
    end

  fun families (ct: ct) : int list list =
    let
      val n = CT.n_irreps ct
      val buckets : int list array = Array.array (n, [])

      fun add j =
        let
          val sp = CT.special (ct, j)
          val () =
            if sp < 0 orelse sp >= n then
              raise Fail "Families.families: special index out of range"
            else
              ()
        in
          Array.update (buckets, sp, j :: Array.sub (buckets, sp))
        end

      val () = List.app add (List.tabulate (n, fn i => i))

      fun finalize i =
        let
          val xs = Array.sub (buckets, i)
        in
          if null xs then [] else [sort_special_first (ct, xs)]
        end
    in
      List.concat (List.tabulate (n, finalize))
    end

  fun family (ct: ct, i: int) : int list =
    let
      val sp = CT.special (ct, i)
      val n = CT.n_irreps ct
      val reps = List.filter (fn j => CT.special (ct, j) = sp) (List.tabulate (n, fn j => j))
    in
      sort_special_first (ct, reps)
    end

  fun family_f (ct: ct) : int -> int list = fn i => family (ct, i)
end

