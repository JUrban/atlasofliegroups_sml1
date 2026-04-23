use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/groups.sml

  Purpose
  - Small SML-facing group-constructor library inspired by `atlas-scripts/groups.at`.
  - The `.at` file builds many groups via `RootDatum`/`InnerClass`/`RealForm`
    constructors. In SML we currently expose a simpler surface:
      create a real form directly as an `AtlasFFI.group` handle using the C++
      shim constructor `atlas_group_new_simple_outer(type,rank,innerClass,rf)`.

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

  Real-form numbering (important)
  - Atlas has two real-form numberings:
      - "inner" numbers (`RealFormNbr`): internal to the library; inner 0 is
        always the quasisplit form of the chosen inner class.
      - "outer" numbers: the stable script-facing indices used by
        `real_form(ic, rf)` in `.at` scripts; outer 0 is the quasicompact form
        (see `atlas-scripts/basic.at`).
  - This module follows the `.at` convention: `simple(..., rf)` interprets
    `rf` as an *outer* number, i.e. it matches `real_form(inner_class(...), rf)`.

  Ownership
  - Every function returning a `group` allocates a fresh handle.
  - Callers must free it with `AtlasFFI.atlas_group_free`.
*)

structure Groups = struct
  type group = AtlasFFI.group

  fun failFFI (where': string) : 'a =
    raise Fail ("Groups." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun simpleInner (typeLetter: char, rank: int, innerClassLetter: char, rfInner: int) : group =
    let
      val g = AtlasFFI.atlas_group_new_simple (typeLetter, rank, innerClassLetter, rfInner)
    in
      if g = Foreign.Memory.null then failFFI "simpleInner" else g
    end

  fun simple (typeLetter: char, rank: int, innerClassLetter: char, rfOuter: int) : group =
    let
      val g = AtlasFFI.atlas_group_new_simple_outer (typeLetter, rank, innerClassLetter, rfOuter)
    in
      if g = Foreign.Memory.null then failFFI "simple" else g
    end

  fun quasisplit (typeLetter: char, rank: int, innerClassLetter: char) : group =
    (* Quasisplit = inner number 0. *)
    simpleInner (typeLetter, rank, innerClassLetter, 0)

  fun numRealForms (typeLetter: char, rank: int, innerClassLetter: char) : int =
    let
      val g0 = simple (typeLetter, rank, innerClassLetter, 0)
      val n = AtlasFFI.atlas_group_num_real_forms g0
      val () = AtlasFFI.atlas_group_free g0
    in
      if n < 0 then failFFI "numRealForms" else n
    end

  (* Exceptional groups: names follow the `groups.at` conventions. *)

  (* G2: see `atlas-scripts/groups.at`. *)
  fun G2_c () : group = simple (#"G", 2, #"e", 0) (* quasicompact/compact *)
  fun G2_s () : group = quasisplit (#"G", 2, #"e")

  (* F4: see `atlas-scripts/groups.at`. *)
  fun F4_s () : group =
    let
      val g = AtlasFFI.atlas_group_new_F4_s ()
    in
      if g = Foreign.Memory.null then failFFI "F4_s" else g
    end

  fun F4_c () : group = simple (#"F", 4, #"e", 0) (* quasicompact *)
  fun F4_B4 () : group = simple (#"F", 4, #"e", 1)

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
