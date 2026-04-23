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
      - a growing subset of classical simple-group constructors (A/B/C/D)
        compatible with the `groups.at` naming and conventions

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

  datatype isogeny = SC | AD

  fun isogenyChar SC = #"s"
    | isogenyChar AD = #"a"

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

  fun simpleIsogenyOuter (iso: isogeny,
                          typeLetter: char,
                          rank: int,
                          innerClassLetter: char,
                          rfOuter: int)
    : group =
    let
      val g =
        AtlasFFI.atlas_group_new_simple_isogeny_outer
          (typeLetter, rank, innerClassLetter, rfOuter, isogenyChar iso)
    in
      if g = Foreign.Memory.null then failFFI "simpleIsogenyOuter" else g
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

  (*
    Classical constructors (subset)

    These functions intentionally follow the *script-facing* conventions of
    `atlas-scripts/groups.at`:
    - real-form indices are outer indices (what `.at` passes to `real_form`)
    - quasisplit forms use the special inner-number 0 (`quasisplit` helper)

    Limitations
    - This module currently constructs only simple groups via
      `atlas_group_new_simple(_isogeny)_outer`. Reductive groups like `GL(n)`
      and `U(p,q)` are not currently supported here.
    - Some `groups.at` constructors (e.g. `SO(n)` for even `n`) correspond to
      intermediate isogeny types; this shim currently supports only simply
      connected (`SC`) and adjoint (`AD`) for classical types.
  *)

  fun minInt (a: int, b: int) = if a < b then a else b
  fun maxInt (a: int, b: int) = if a > b then a else b

  fun requireNonNeg (name: string, n: int) =
    if n < 0 then raise Fail ("Groups." ^ name ^ ": expected nonnegative int") else ()

  fun requirePos (name: string, n: int) =
    if n <= 0 then raise Fail ("Groups." ^ name ^ ": expected positive int") else ()

  (* Type A: SU(p,q) as a real form of SL(p+q). *)
  fun SU (p: int, q: int) : group =
    let
      val () = requireNonNeg ("SU(p,q)", p)
      val () = requireNonNeg ("SU(p,q)", q)
      val n = p + q
      val () = if n >= 2 then () else raise Fail "Groups.SU: expected p+q>=2"
      val rank = n - 1
      val rf = minInt (p, q)
    in
      simpleIsogenyOuter (SC, #"A", rank, #"c", rf)
    end

  fun PSU (p: int, q: int) : group =
    let
      val () = requireNonNeg ("PSU(p,q)", p)
      val () = requireNonNeg ("PSU(p,q)", q)
      val n = p + q
      val () = if n >= 2 then () else raise Fail "Groups.PSU: expected p+q>=2"
      val rank = n - 1
      val rf = minInt (p, q)
    in
      simpleIsogenyOuter (AD, #"A", rank, #"c", rf)
    end

  (* Split real form SL(n,R) and its adjoint PSL(n,R). *)
  fun SL_R (n: int) : group =
    let
      val () = if n >= 2 then () else raise Fail "Groups.SL_R: expected n>=2"
      val rank = n - 1
    in
      simpleInner (#"A", rank, #"s", 0)
    end

  fun PSL_R (n: int) : group =
    let
      val () = if n >= 2 then () else raise Fail "Groups.PSL_R: expected n>=2"
      val rank = n - 1
      val g = AtlasFFI.atlas_group_new_simple_isogeny (#"A", rank, #"s", 0, #"a")
    in
      if g = Foreign.Memory.null then failFFI "PSL_R" else g
    end

  (* Type C: Sp(n,R) for even n, and Sp(p,q). *)
  fun Sp_R (n: int) : group =
    let
      val () = requirePos ("Sp_R", n)
      val () = if n mod 2 = 0 then () else raise Fail "Groups.Sp_R: expected even n"
      val m = n div 2
      val () = requirePos ("Sp_R", m)
    in
      simpleInner (#"C", m, #"e", 0)
    end

  fun PSp_R (n: int) : group =
    let
      val () = requirePos ("PSp_R", n)
      val () = if n mod 2 = 0 then () else raise Fail "Groups.PSp_R: expected even n"
      val m = n div 2
      val () = requirePos ("PSp_R", m)
    in
      (* adjoint, quasisplit (inner 0) *)
      let
        val g = AtlasFFI.atlas_group_new_simple_isogeny (#"C", m, #"e", 0, #"a")
      in
        if g = Foreign.Memory.null then failFFI "PSp_R" else g
      end
    end

  fun Sp (p: int, q: int) : group =
    let
      val () = requireNonNeg ("Sp(p,q)", p)
      val () = requireNonNeg ("Sp(p,q)", q)
      val m = p + q
      val () = requirePos ("Sp(p,q)", m)
      val rf = minInt (p, q)
    in
      simpleIsogenyOuter (SC, #"C", m, #"e", rf)
    end

  fun PSp (p: int, q: int) : group =
    let
      val () = requireNonNeg ("PSp(p,q)", p)
      val () = requireNonNeg ("PSp(p,q)", q)
      val m = p + q
      val () = requirePos ("PSp(p,q)", m)
      val rf = minInt (p, q)
    in
      simpleIsogenyOuter (AD, #"C", m, #"e", rf)
    end

  (* Type B/D: Spin(p,q) and PSO(p,q) as adjoint form.
     This follows `SO_inner_class` and `SO_real_form_number` from `groups.at`
     for the simple cases (n != 1,4). *)
  fun SO_inner_class (p: int, q: int) : char =
    let
      val n = p + q
      val () = if n = 1 orelse n = 4 then raise Fail "Groups.SO_inner_class: n=1 or n=4 not supported" else ()
    in
      if (p mod 2 = 1 andalso q mod 2 = 1) then
        if (n mod 4 = 0 andalso n > 4) then #"u" else #"s"
      else
        #"e"
    end

  fun SO_real_form_number (p0: int, q0: int) : int =
    let
      val n = p0 + q0
      val (p, q) = if p0 < q0 then (q0, p0) else (p0, q0)
    in
      if n <= 2 then 0
      else if n mod 2 = 1 then q
      else if q mod 2 = 1 then q div 2
      else if q <= n div 4 then q div 2
      else if n mod 4 = 2 then q div 2 + 1
      else q div 2 + 2
    end

  fun Spin (p: int, q: int) : group =
    let
      val () = requireNonNeg ("Spin(p,q)", p)
      val () = requireNonNeg ("Spin(p,q)", q)
      val n = p + q
      val () = if n >= 5 then () else raise Fail "Groups.Spin: expected p+q>=5 (simple types only)"
      val ic = SO_inner_class (maxInt (p, q), minInt (p, q))
      val rf = SO_real_form_number (p, q)
    in
      if n mod 2 = 1 then
        simpleIsogenyOuter (SC, #"B", (n - 1) div 2, ic, rf)
      else
        simpleIsogenyOuter (SC, #"D", n div 2, ic, rf)
    end

  fun PSO (p: int, q: int) : group =
    let
      val () = requireNonNeg ("PSO(p,q)", p)
      val () = requireNonNeg ("PSO(p,q)", q)
      val n = p + q
      val () = if n >= 5 then () else raise Fail "Groups.PSO: expected p+q>=5 (simple types only)"
      val ic = SO_inner_class (maxInt (p, q), minInt (p, q))
      val rf = SO_real_form_number (p, q)
    in
      if n mod 2 = 1 then
        simpleIsogenyOuter (AD, #"B", (n - 1) div 2, ic, rf)
      else
        simpleIsogenyOuter (AD, #"D", n div 2, ic, rf)
    end
end
