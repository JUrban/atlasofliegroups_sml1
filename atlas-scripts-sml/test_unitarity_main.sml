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
    where the first integer argument is the max count and the optional argument
    `verbose` enables per-parameter output.

  - Select the table by passing one of `F4`, `D4`, `E7` anywhere in the args:
      `poly -q < atlas-scripts-sml/test_unitarity_main.sml`                  (defaults to F4)
      `poly -q < atlas-scripts-sml/test_unitarity_main.sml -- E7 100 verbose`
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

fun pickTable (xs: string list) : string =
  if List.exists (fn s => s = "E7") xs then "E7"
  else if List.exists (fn s => s = "D4") xs then "D4"
  else "F4";

val table = pickTable args;

val (typeLetter, rank, nus) =
  case table of
    "F4" => (#"F", 4, Unitary.F4_spherical_unitary)
  | "D4" => (#"D", 4, Unitary.D4_spherical_unitary)
  | "E7" => (#"E", 7, Unitary.E7_spherical_unitary)
  | _ => raise Fail "unreachable";

val g = AtlasFFI.atlas_group_new_simple (typeLetter, rank, #"s", 0);
val ok =
  TestUnitarity.test_minimal_spherical_unitary_points
    (g, nus, verbose, SOME maxN);
val () = AtlasFFI.atlas_group_free g;

val () =
  TextIO.print
    ("table=" ^ table ^ " tested=" ^ Int.toString maxN ^ " passed=" ^ (if ok then "true" else "false") ^ "\n");
