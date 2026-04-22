use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamBlocks.sml";
use "atlas-scripts-sml/representations.sml";

(*
  File: atlas-scripts-sml/ffi/test_param_block_trivial_F4_s.sml

  Purpose
  - Smoke test for the `block(p)` FFI wrapper (`atlas_param_block_survivors`).

  What it checks
  - For `p = trivial(F4_s)`, the block survivor list is nonempty.
  - `start_pos` is in range and points to a parameter equal to `p`.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_param_block_trivial_F4_s.sml`
*)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val p = Representations.trivial g
val () = if AtlasFFI.atlas_param_is_standard p = 1 then () else raise Fail "trivial param not standard?"

val ((terms, startPos)) = ParamBlocks.block_survivors p
val n = length terms
val () = if n > 0 then () else raise Fail "block survivors empty"

val () =
  if startPos >= 0 andalso startPos < n then
    ()
  else
    raise Fail ("start_pos out of range: " ^ Int.toString startPos ^ " n=" ^ Int.toString n)

val (q, _) = List.nth (terms, startPos)
val () = if AtlasFFI.atlas_param_equal (p, q) = 1 then () else raise Fail "start_pos param not equal to p"

val () = ParamBlocks.freeTerms terms
val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g

val () = TextIO.print "OK\n"

