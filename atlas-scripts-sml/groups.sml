use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/groups.sml

  Purpose
  - Small SML-facing group-constructor library inspired by `atlas-scripts/groups.at`.
  - The `.at` file builds many groups via `RootDatum`/`InnerClass`/`RealForm`
    constructors. In SML we currently expose a simpler surface:
      create a real form directly as an `AtlasFFI.group` handle using the C++
      shim constructor `atlas_group_new_simple(type,rank,innerClass,rf)`.

  Scope
  - This module focuses on *simple* groups where the Atlas library provides
    a single (type,rank) datum and a finite list of real forms indexed by `rf`.
  - It currently provides:
      - a generic `simple` constructor + `numRealForms` query
      - named exceptional real forms mirroring the bottom of `groups.at`

  Inner class letters
  - The shim accepts the same user-facing letters as the Atlas interpreter:
      `c` (compact), `e` (equal-rank synonym), `s` (split), `u` (unequal rank)
    and canonicalizes them as the interpreter does (e.g. for `F4`, `s` and `e`
    collapse to the same inner class).

  Ownership
  - Every function returning a `group` allocates a fresh handle.
  - Callers must free it with `AtlasFFI.atlas_group_free`.
*)

structure Groups = struct
  type group = AtlasFFI.group

  fun failFFI (where': string) : 'a =
    raise Fail ("Groups." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun simple (typeLetter: char, rank: int, innerClassLetter: char, rf: int) : group =
    let
      val g = AtlasFFI.atlas_group_new_simple (typeLetter, rank, innerClassLetter, rf)
    in
      if g = Foreign.Memory.null then failFFI "simple" else g
    end

  fun numRealForms (typeLetter: char, rank: int, innerClassLetter: char) : int =
    let
      val g0 = simple (typeLetter, rank, innerClassLetter, 0)
      val n = AtlasFFI.atlas_group_num_real_forms g0
      val () = AtlasFFI.atlas_group_free g0
    in
      if n < 0 then failFFI "numRealForms" else n
    end

  (* Exceptional groups: names follow the `groups.at` conventions. *)

  (* G2: inner class `e`; typically rf=0 (compact), rf=1 (split). *)
  fun G2_c () : group = simple (#"G", 2, #"e", 0)
  fun G2_s () : group = simple (#"G", 2, #"e", 1)

  (* F4: inner class `e` (same as `s`); typically rf=0 (split), rf=1 (B4), rf=2 (compact). *)
  fun F4_s () : group =
    let
      val g = AtlasFFI.atlas_group_new_F4_s ()
    in
      if g = Foreign.Memory.null then failFFI "F4_s" else g
    end

  fun F4_B4 () : group = simple (#"F", 4, #"e", 1)
  fun F4_c () : group = simple (#"F", 4, #"e", 2)

  (* E6: two inner classes `e` and `s` (as in `groups.at`). *)
  fun E6_c () : group = simple (#"E", 6, #"e", 0)
  fun E6_h () : group = simple (#"E", 6, #"e", 1)
  fun E6_q () : group = simple (#"E", 6, #"e", 2)

  fun E6_F4 () : group = simple (#"E", 6, #"s", 0)
  fun E6_s () : group = simple (#"E", 6, #"s", 1)

  (* E7: inner class `e` (as in `groups.at`). *)
  fun E7_c () : group = simple (#"E", 7, #"e", 0)
  fun E7_h () : group = simple (#"E", 7, #"e", 1)
  fun E7_q () : group = simple (#"E", 7, #"e", 2)
  fun E7_s () : group = simple (#"E", 7, #"e", 3)

  (* E8: inner class `e` (as in `groups.at`). *)
  fun E8_c () : group = simple (#"E", 8, #"e", 0)
  fun E8_q () : group = simple (#"E", 8, #"e", 1)
  fun E8_s () : group = simple (#"E", 8, #"e", 2)
end

