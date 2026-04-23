use "atlas-scripts-sml/partitions.sml";

(*
  File: atlas-scripts-sml/ffi/test_partitions_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/partitions.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

open Partitions;

val () = assert "transpose" (transpose [3, 1] = [2, 1, 1]);
val () = assert "hook_length (2,1) @ (0,0)" (hook_length ([2, 1], 0, 0) = 3);
val () = assert "dim_rep [3]" (dim_rep [3] = 1);
val () = assert "dim_rep [1,1,1]" (dim_rep [1, 1, 1] = 1);
val () = assert "dim_rep [2,1]" (dim_rep [2, 1] = 2);

val c = compositions_le (2, 2);
val () = assert "compositions count" (length c = 6);
val () = assert "compositions include [2,0]" (List.exists (fn xs => xs = [2, 0]) c);
val () = assert "compositions include [0,2]" (List.exists (fn xs => xs = [0, 2]) c);

