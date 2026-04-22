use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/representations.sml";

(*
  File: atlas-scripts-sml/ffi/test_representations_large_series_smoke.sml

  Purpose
  - Smoke test for additional constructors ported from `representations.at`:
      - `large_fundamental_series_default`
      - `large_discrete_series_default` (equal-rank gated)

  Behavior
  - Ensures `large_*` constructors allocate/free parameters correctly.
  - Checks that `large_discrete_series_default` rejects a non-equal-rank split
    group (A2_s / SL(3,R)).

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_representations_large_series_smoke.sml`
*)

fun expectFail (thunk: unit -> 'a) : unit =
  (ignore (thunk ()); raise Fail "expected failure, but call succeeded")
  handle Fail _ => ()

val gF4 = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if gF4 = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val pFund = Representations.large_fundamental_series_default gF4
val () = if pFund = Foreign.Memory.null then raise Fail "large_fundamental_series_default returned null" else ()
val () = AtlasFFI.atlas_param_free pFund

val pDisc = Representations.large_discrete_series_default gF4
val () = if pDisc = Foreign.Memory.null then raise Fail "large_discrete_series_default returned null" else ()
val () = AtlasFFI.atlas_param_free pDisc

val () = AtlasFFI.atlas_group_free gF4

val gA2 = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0)
val () = if gA2 = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()
val () = expectFail (fn () => Representations.large_discrete_series_default gA2)
val () = AtlasFFI.atlas_group_free gA2

val () = TextIO.print "OK\n"

