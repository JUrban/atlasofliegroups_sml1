use "atlas-scripts-sml/test_unitarity.sml";

(*
  File: atlas-scripts-sml/test_unitarity_main.sml

  Purpose
  - Runnable entrypoint (Poly/ML) for checking the spherical-unitary point table
    for split F4 using the SML-side Atlas bindings.

  Usage
  - Default (tests first 3 points, quiet):
      `poly -q < atlas-scripts-sml/test_unitarity_main.sml`
  - Override point count and verbosity using command line args:
      `poly -q atlas-scripts-sml/test_unitarity_main.sml -- 10 verbose`
    where the first arg is an integer max count and the optional second arg is
    the literal string `verbose`.
*)

val args = CommandLine.arguments ();

fun firstIntOr (xs: string list, default: int) : int =
  case xs of
    [] => default
  | x :: rest =>
      (case Int.fromString x of
         SOME n => n
       | NONE => firstIntOr (rest, default));

val maxN = firstIntOr (args, 3);
val verbose = List.exists (fn s => s = "verbose") args;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val ok =
  TestUnitarity.test_minimal_spherical_unitary_points
    (g, Unitary.F4_spherical_unitary, verbose, SOME maxN);
val () = AtlasFFI.atlas_group_free g;

val () = TextIO.print ("passed=" ^ (if ok then "true" else "false") ^ "\n");
