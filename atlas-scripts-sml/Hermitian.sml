use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/Hermitian.sml

  Purpose
  - Small SML wrapper for hermitian-form related operations used by Atlas
    scripts (notably `test_unitarity.at` / `hermitian.at`), implemented via
    direct C++ FFI calls.

  Scope
  - This is intentionally minimal and currently exposes only what we need for
    unitary testing in the equal-rank path:
      - `hermitian_form_irreducible(p)` returning a `KTypePol`
      - `is_unitary_via_hermitian_form(p)` checking purity of that form

  Atlas correspondence
  - In the `.at` environment, unitarity of a delta-fixed parameter can be
    tested via:
      `hf = hermitian_form_irreducible(p); is_pure(hf)`
  - The C++ shim exports `atlas_param_hermitian_form_irreducible`, which follows
    the same equal-rank construction used internally by
    `AtlasFFI.atlas_param_is_unitary` (see `atlas-scripts-sml/ffi/atlas_smlffi.cpp`).

  Ownership
  - `AtlasFFI.ktypepol` values are owned handles; callers must free them with
    `KTypePol.free` (or `AtlasFFI.atlas_ktypepol_free`).
*)
structure Hermitian = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ktypepol = AtlasFFI.ktypepol

  (* Return `hermitian_form_irreducible(p)` as a `KTypePol`.
     Raises `Fail` if the parameter is not hermitian or if the FFI fails. *)
  fun hermitian_form_irreducible (p: param) : ktypepol =
    let
      val pol = AtlasFFI.atlas_param_hermitian_form_irreducible p
      val () =
        if pol = Foreign.Memory.null then
          raise Fail ("Hermitian.hermitian_form_irreducible: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      pol
    end

  (* Unitarity test computed explicitly via the hermitian form:
       - builds the hermitian form as a KTypePol
       - checks “module purity” (all coefficients integer OR all `s`-multiples)

     This is the same purity predicate used by the current C++ implementation
     of `AtlasFFI.atlas_param_is_unitary` for equal-rank cases. *)
  fun is_unitary_via_hermitian_form (g: group, p: param) : bool =
    if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      false
    else
      let
        val pol = hermitian_form_irreducible p
        val r = AtlasFFI.atlas_group_rank g
        val ok = KTypePol.isPureModule (pol, r)
        val () = KTypePol.free pol
      in
        ok
      end
end

