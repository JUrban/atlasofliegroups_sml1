use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/finite_dimensional.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/ffi/test_finite_dimensional_dimension_A1_smoke.sml

  Purpose
  - Smoke test for `FiniteDimensional.dimension` against the known A1 formula:
      dim(V_n) = n + 1
    when `n` is given in fundamental-weight coordinates.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_finite_dimensional_dimension_A1_smoke.sml`
*)

fun expectEq (a: IntInf.int, b: IntInf.int, msg: string) =
  if a = b then () else raise Fail (msg ^ ": expected " ^ IntInf.toString b ^ " got " ^ IntInf.toString a)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val rd = AtlasFFI.atlas_group_rootdatum_new g
val () = if rd = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

fun dimN n =
  let
    val lam = Lattice.ratvecNormalize {den = 1, nums = [n]}
  in
    FiniteDimensional.dimension (rd, lam)
  end

val () = expectEq (dimN 0, 1, "dim(0)")
val () = expectEq (dimN 1, 2, "dim(1)")
val () = expectEq (dimN 2, 3, "dim(2)")
val () = expectEq (dimN 5, 6, "dim(5)")

val () = AtlasFFI.atlas_rootdatum_free rd
val () = AtlasFFI.atlas_group_free g

val () = TextIO.print "OK\n"

