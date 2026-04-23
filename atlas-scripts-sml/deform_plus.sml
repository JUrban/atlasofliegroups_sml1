use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/iterate_deform.sml";
use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/deform_plus.sml

  Purpose
  - SML translation of `atlas-scripts/deform_plus.at`.
  - Provides a recursive “deform all the way to nu=0” driver which expands a
    final standard parameter `p` into a (virtual) K-type polynomial at `nu=0`.

  Background (Atlas terminology)
  - The Atlas interpreter exposes a built-in `deform(p)` command whose output
    is a `ParamPol` of “spin-off” terms with coefficients in split-integers
    `a + s*b`. The helper `deformation_terms` is an older script-level
    equivalent.
  - This repository already ports the core deformation primitive as:
      `IterateDeform.deform : Param -> ParamPol.t`
    backed by the C++ shim export `atlas_param_deform`.
  - This file is therefore mostly a control-flow / recursion port.

  API (ported subset)
  - `deformation p`:
      returns `(lower(p), deform(p))` in the `.at` sense.
  - `recursive_deform_plus p`:
      recursively expands `p` and all spun-off terms, returning:
        - `computed`: the set of parameters for which deformation terms were computed
          (as a list of params; caller owns and must free them),
        - `at_nu0`: the accumulated `KTypePol` at `nu=0` (caller must free it).

  Notes / differences vs `.at`
  - The `.at` script uses `p.K_type_pol`. We approximate this by the Atlas
    library routine `atlas_param_full_deform`, which computes the full
    deformation K-type polynomial for a standard representation.
  - `chamber_rep` from `deform_plus.at` is currently left unimplemented; it
    requires `coxeter_number` access that we have not yet exposed in the FFI.

  Ownership
  - All returned `param` handles are newly allocated (cloned) and must be freed
    by the caller with `AtlasFFI.atlas_param_free`.
  - The returned `ktypepol` handle must be freed with `AtlasFFI.atlas_ktypepol_free`
    (or via `KTypePol.free`).
*)

structure DeformPlus = struct
  type param = AtlasFFI.param
  type coef = Split.t
  type poly = ParamPol.t
  type ktypepol = AtlasFFI.ktypepol
  type ratvec = Lattice.ratvec

  val d_verbose = ref false

  fun failFFI (where': string) : 'a =
    raise Fail ("DeformPlus." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun parseRatvecText (s: string) : ratvec =
    let
      val {den, nums} = AllParameters.parseRatWeightText s
    in
      Lattice.ratvecNormalize {den = den, nums = nums}
    end

  fun ratvecToText (u: ratvec) : string =
    let
      val u = Lattice.ratvecNormalize u
      fun intToCText n =
        let
          val s = Int.toString n
        in
          if String.size s > 0 andalso String.sub (s, 0) = #"~" then
            "-" ^ String.extract (s, 1, NONE)
          else
            s
        end
    in
      String.concatWith " " (intToCText (#den u) :: List.map intToCText (#nums u))
    end

  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec =
    Lattice.ratvecSub (u, Lattice.ratvecScale (v, ~1, 1))

  (* Floor of `v/k` as an integer vector (script: `ratvec v \ k`). *)
  fun ratvecFloorDiv (v: ratvec, k: int) : int list =
    let
      val v = Lattice.ratvecNormalize v
      val den = #den v
      val () = if den <= 0 then raise Fail "DeformPlus.ratvecFloorDiv: nonpositive denom" else ()
      val () = if k > 0 then () else raise Fail "DeformPlus.ratvecFloorDiv: expected k>0"
      val dk = den * k
    in
      List.map (fn n => n div dk) (#nums v)
    end

  fun splitToIntParts (w: coef) : int * int =
    let
      fun toInt z =
        (IntInf.toInt z
         handle Overflow => raise Fail "DeformPlus: split coefficient out of int range")
    in
      (toInt (Split.int_part w), toInt (Split.s_part w))
    end

  fun ktypepolScaleSplit (pol: ktypepol, w: coef) : ktypepol =
    let
      val (e, s) = splitToIntParts w
      val q = AtlasFFI.atlas_ktypepol_scale_split (pol, e, s)
    in
      if q = Foreign.Memory.null then failFFI "ktypepolScaleSplit" else q
    end

  fun ktypepolAdd (a: ktypepol, b: ktypepol) : ktypepol =
    let
      val q = AtlasFFI.atlas_ktypepol_add (a, b)
    in
      if q = Foreign.Memory.null then failFFI "ktypepolAdd" else q
    end

  fun paramFullDeform (p: param) : ktypepol =
    let
      val pol = AtlasFFI.atlas_param_full_deform p
    in
      if pol = Foreign.Memory.null then failFFI "paramFullDeform" else pol
    end

  fun assertFinalAndStandard (p: param) : unit =
    let
      val isFinal = AtlasFFI.atlas_param_is_final p = 1
      val isStd = AtlasFFI.atlas_param_is_standard p = 1
    in
      if not isFinal then raise Fail "DeformPlus: improper parameter (not final)" else ();
      if not isStd then raise Fail "DeformPlus: improper parameter (not standard)" else ()
    end

  fun paramScale (p: param, num: int, den: int) : param =
    let
      val q = AtlasFFI.atlas_param_scale (p, num, den)
    in
      if q = Foreign.Memory.null then failFFI "paramScale" else q
    end

  (* `deformation(p) = (lower(p), deform(p))` in `.at`. *)
  fun deformation (p: param) : param * poly =
    (IterateDeform.lower p, IterateDeform.deform p)

  (* A “set” of parameters, represented as a ParamPol with coefficient 1. *)
  fun computedAddClone (computed: poly, p: param) : unit =
    ParamPol.addTermClone (computed, Split.one, p)

  fun computedTakeMove (computed: poly) : param list =
    let
      val ts = ParamPol.terms computed
      val () = computed := []
    in
      List.map (fn (_, p) => p) ts
    end

  (*
    recursive_deform_plus

    Port of the non-verbose branch of `recursive_deform_plus` in `deform_plus.at`.

    WARNING
    - This is potentially expensive on large parameters (it is a recursive
      expansion). Use with care.
  *)
  fun recursive_deform_plus (p0: param) : param list * ktypepol =
    let
      val () = assertFinalAndStandard p0

      val computed = ParamPol.create ()

      fun full_def (sc: coef, p: param) : ktypepol =
        let
          val () =
            if AtlasFFI.atlas_param_is_standard p = 1 then
              ()
            else
              raise Fail "DeformPlus.full_def: non-standard parameter encountered in deformation"

          val () = computedAddClone (computed, p)

          val base = paramFullDeform p
          val at_nu0_ref = ref (ktypepolScaleSplit (base, sc))
          val () = AtlasFFI.atlas_ktypepol_free base

          val acc = ParamPol.create ()
          val rps = IterateDeform.reducibility_points p

          fun addFactor (num, den) =
            let
              val p_def = paramScale (p, num, den)
              val def = IterateDeform.deform p_def
              val () = computedAddClone (computed, p_def)
              val () = ParamPol.addIntoMove (acc, def)
              val () = AtlasFFI.atlas_param_free p_def
            in
              ()
            end

          val () = List.app addFactor rps

          fun processTerm (k, q) =
            let
              val sc2 = Split.mul (sc, k)
              val polQ = full_def (sc2, q)
              val sum = ktypepolAdd (!at_nu0_ref, polQ)
              val () = AtlasFFI.atlas_ktypepol_free (!at_nu0_ref)
              val () = AtlasFFI.atlas_ktypepol_free polQ
              val () = at_nu0_ref := sum
              val () = computedAddClone (computed, q)
            in
              ()
            end

          val () = List.app processTerm (ParamPol.terms acc)
          val () = ParamPol.free acc
        in
          !at_nu0_ref
        end

      val pol = full_def (Split.one, p0)
      val ps = computedTakeMove computed
      val () = ParamPol.free computed
    in
      (ps, pol)
    end

  (*
    chamber_rep

    Port of `deform_plus.at`:
      parameter(p.x, p.lambda, p.nu\1 + p.root_datum.rho/(p.root_datum.coxeter_number+1))

    Notes
    - `p.nu\1` is the coordinate-wise floor of `p.nu` (a `ratvec`) divided by 1,
      producing an integral vector which is then coerced to a `ratvec` of
      denominator 1 when added to `rho/(h+1)`.
    - We build the new parameter using the group handle already associated to `p`.
  *)
  fun chamber_rep (p: param) : param =
    let
      val g = AtlasFFI.atlas_param_group_handle p
      val () = if g = Foreign.Memory.null then failFFI "chamber_rep/group_handle" else ()
      val x = AtlasFFI.atlas_param_x p
      val lambda = parseRatvecText (AtlasFFI.atlas_param_lambda_text p)
      val nu = parseRatvecText (AtlasFFI.atlas_param_nu_text p)

      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () = if rd = Foreign.Memory.null then failFFI "chamber_rep/rootdatum_new" else ()
      val rho = parseRatvecText (RootDatum.rhoText rd)
      val h = RootDatum.coxeterNumber rd
      val () = RootDatum.free rd

      val nuFloor = {den = 1, nums = ratvecFloorDiv (nu, 1)}
      val shift = Lattice.ratvecScale (rho, 1, h + 1)
      val nu2 = Lattice.ratvecNormalize (ratvecAdd (nuFloor, shift))

      fun intsToCText xs =
        let
          fun one n =
            let
              val s = Int.toString n
            in
              if String.size s > 0 andalso String.sub (s, 0) = #"~" then
                "-" ^ String.extract (s, 1, NONE)
              else
                s
            end
        in
          String.concatWith " " (List.map one xs)
        end

      val p0 =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          ( g
          , x
          , intsToCText (#nums lambda)
          , #den lambda
          , intsToCText (#nums nu2)
          , #den nu2
          )
      val () = if p0 = Foreign.Memory.null then failFFI "chamber_rep/param_new" else ()
      val p1 = AtlasFFI.atlas_param_normalise p0
      val () = AtlasFFI.atlas_param_free p0
      val () = if p1 = Foreign.Memory.null then failFFI "chamber_rep/normalise" else ()
    in
      p1
    end
end
