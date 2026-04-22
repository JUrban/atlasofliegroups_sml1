use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/finite_dimensional.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/ffi/test_finite_dimensional_param_A1_smoke.sml

  Purpose
  - Smoke test for the parameter-facing helpers in `finite_dimensional.sml`:
      - `highest_weight_finite_dimensional_ratvec`
      - `fundamental_weight_coordinates`
      - `dimension_param`

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_finite_dimensional_param_A1_smoke.sml`
*)

fun expectEqInt (a: int, b: int, msg: string) =
  if a = b then () else raise Fail (msg ^ ": expected " ^ Int.toString b ^ " got " ^ Int.toString a)

fun expectEqIntInf (a: IntInf.int, b: IntInf.int, msg: string) =
  if a = b then () else raise Fail (msg ^ ": expected " ^ IntInf.toString b ^ " got " ^ IntInf.toString a)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

(* Highest weight = 3, so dim = 4. *)
val lam = Lattice.ratvecNormalize {den = 1, nums = [3]}
val p = Representations.finite_dimensional (g, lam)
val () = if p = Foreign.Memory.null then raise Fail "finite_dimensional returned null" else ()

val hwt = FiniteDimensional.highest_weight_finite_dimensional_ratvec (g, p)
val () = expectEqInt (#den hwt, 1, "hwt.den")
val () = expectEqInt (hd (#nums hwt), 3, "hwt")

val coords = FiniteDimensional.fundamental_weight_coordinates (g, p)
val () = expectEqInt (hd coords, 3, "fundamental_weight_coordinates")

val dim = FiniteDimensional.dimension_param (g, p)
val () = expectEqIntInf (dim, 4, "dimension_param")

val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g

val () = TextIO.print "OK\n"

