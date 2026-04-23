use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/iterate_deform.sml";
use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/deform.sml

  Purpose
  - Compatibility / bridge layer for the Atlas script `atlas-scripts/deform.at`.
  - The original `.at` file is largely historical documentation: most modern
    Atlas setups expose deformation computations as built-ins (`deform`,
    `full_deform`, etc.).
  - In this SML port we implement a small subset of the *public surface* that
    other scripts typically depend on, by delegating to:
      - `IterateDeform.deform` (backed by C++ shim `atlas_param_deform`)
      - `AtlasFFI.atlas_param_full_deform` (full deformation to `KTypePol`)

  Provided API (subset)
  - `deformation_terms p`:
      deformation spin-off terms as a `ParamPol.t` with `Split` coefficients.
  - `deformation p`:
      `(lower(p), deformation_terms(p))` matching `deform_plus.at`.
  - `full_deform_param p`:
      `KTypePol` value for the (standard) module attached to `p`.
  - `full_deform poly`:
      linear extension of `full_deform_param` to a `ParamPol.t`.

  Notes
  - Coefficients in `ParamPol` are `Split.t` with `IntInf` components; the
    `KTypePol` FFI uses machine `int`. This module checks for overflow and
    raises on out-of-range coefficients.

  Ownership
  - `deformation_terms` returns a `ParamPol.t` that owns its stored params.
  - `full_deform_param` / `full_deform` return owned `ktypepol` handles; free
    with `AtlasFFI.atlas_ktypepol_free` (or `KTypePol.free`).
*)

structure Deform = struct
  type param = AtlasFFI.param
  type coef = Split.t
  type poly = ParamPol.t
  type ktypepol = AtlasFFI.ktypepol

  fun failFFI (where': string) : 'a =
    raise Fail ("Deform." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun splitToIntParts (w: coef) : int * int =
    let
      fun toInt z =
        (IntInf.toInt z
         handle Overflow => raise Fail "Deform: split coefficient out of int range")
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

  (* Deformation terms (spin-off) as `ParamPol`, mirroring `deformation_terms`. *)
  fun deformation_terms (p: param) : poly = IterateDeform.deform p

  (* `deformation(p) = (lower(p), deform(p))` as in `deform_plus.at`. *)
  fun deformation (p: param) : param * poly = (IterateDeform.lower p, deformation_terms p)

  (* Full deformation of a standard representation to `KTypePol`. *)
  fun full_deform_param (p: param) : ktypepol =
    let
      val pol = AtlasFFI.atlas_param_full_deform p
    in
      if pol = Foreign.Memory.null then failFFI "full_deform_param" else pol
    end

  (* A zero `KTypePol` for the same group as `p`. *)
  fun null_K_module_like (p: param) : ktypepol =
    let
      val pol = full_deform_param p
      val zero = AtlasFFI.atlas_ktypepol_scale_split (pol, 0, 0)
      val () = AtlasFFI.atlas_ktypepol_free pol
    in
      if zero = Foreign.Memory.null then failFFI "null_K_module_like" else zero
    end

  (* Linear extension of `full_deform_param` from `Param` to `ParamPol`. *)
  fun full_deform (pp: poly) : ktypepol =
    (case ParamPol.terms pp of
       [] => raise Fail "Deform.full_deform: empty ParamPol"
     | (c0, p0) :: rest =>
         let
           val accRef = ref (null_K_module_like p0)

           fun addOne (c: coef, p: param) =
             let
               val polP = full_deform_param p
               val scaled = ktypepolScaleSplit (polP, c)
               val sum = ktypepolAdd (!accRef, scaled)
               val () = AtlasFFI.atlas_ktypepol_free polP
               val () = AtlasFFI.atlas_ktypepol_free scaled
               val () = AtlasFFI.atlas_ktypepol_free (!accRef)
               val () = accRef := sum
             in
               ()
             end

           val () = addOne (c0, p0)
           val () = List.app addOne rest
         in
           !accRef
         end)
end

