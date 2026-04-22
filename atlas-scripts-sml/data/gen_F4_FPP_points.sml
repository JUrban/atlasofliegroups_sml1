use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/F4_FPP_points_compute.sml";
use "atlas-scripts-sml/FPPFlags.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/data/gen_F4_FPP_points.sml

  Purpose
  - Pure-SML generator for the fixture `atlas-scripts-sml/data/F4_FPP_points.txt`.
  - This removes the last “core” dependency on Atlas `.at` scripts/data in the
    F4 FPP workflow: we can now regenerate the point set directly from the
    folded-FPP SML computations and the Atlas C++ library via FFI.

  What this file regenerates
  - `atlas-scripts-sml/data/F4_FPP_points.txt`
    Rows have the same encoding as the historical fixture:
      x lamDen l1 l2 l3 l4 nuDen n1 n2 n3 n4
    where `lambda = [l1,l2,l3,l4]/lamDen` and `nu = [n1,n2,n3,n4]/nuDen`.

  Usage
  - From the repo root:
      `poly -q < atlas-scripts-sml/data/gen_F4_FPP_points.sml`

  Output policy
  - By default, writes `F4_FPP_points.txt.new` to avoid noisy diffs.
  - To overwrite the tracked fixture, set `OVERWRITE=1`.

  Determinism
  - The computed parameter set is turned into a stable key `(x, lambda, nu)` and
    sorted lexicographically before writing.
*)

structure GenF4FPPPoints = struct
  type param = AtlasFFI.param
  type row = int list

  (* Read an environment flag as a boolean. *)
  fun envFlag (name: string) : bool =
    (case OS.Process.getEnv name of
       SOME "1" => true
     | SOME "true" => true
     | SOME "yes" => true
     | _ => false)

  (* Convert SML `~` negatives to C-style `-` negatives for stable file output. *)
  fun intToCText (n: int) : string =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  (* Parse whitespace-separated integers, accepting C-style negatives. *)
  fun parseInts (s: string) : int list =
    let
      fun toInt tok =
        (case Int.fromString tok of
           SOME n => n
         | NONE => raise Fail ("GenF4FPPPoints: bad int token: " ^ tok))
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Extract `[den, a, b, c, d]` from Atlas' `*_text` representation. *)
  fun parseRatvecText (who: string, text: string) : int list =
    let
      val xs = parseInts text
    in
      case xs of
        [_, _, _, _, _] => xs
      | _ => raise Fail ("GenF4FPPPoints: expected 5 ints for " ^ who ^ ", got: " ^ text)
    end

  (* Stable key for sorting parameters: `[x, lamDen, l1..l4, nuDen, n1..n4]`. *)
  fun rowOfParam (p: param) : row =
    let
      val x = AtlasFFI.atlas_param_x p
      val lam = parseRatvecText ("lambda", AtlasFFI.atlas_param_lambda_text p)
      val nu = parseRatvecText ("nu", AtlasFFI.atlas_param_nu_text p)
    in
      x :: lam @ nu
    end

  (* Sort rows lexicographically (all rows have the same length). *)
  fun sortRows (rows: row list) : row list =
    Basic.sort Sort.rlex_leq rows

  (* Write the point fixture file. *)
  fun writePoints (path: string, rows: row list) : unit =
    let
      val out = TextIO.openOut path
      fun emit ints =
        let
          val line = String.concatWith " " (List.map intToCText ints) ^ "\n"
        in
          TextIO.output (out, line)
        end
    in
      (List.app emit rows; TextIO.closeOut out) handle e => (TextIO.closeOut out; raise e)
    end

  fun main () : unit =
    let
      val overwrite = envFlag "OVERWRITE"

      val () = FPPFlags.final_verbose := false
      val () = FPPFlags.Dirac_flag := true

      val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
      val () =
        if g = Foreign.Memory.null then
          raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()

      val h = ParamHash.create 4096
      val () = F4_FPP_points_compute.computeAllIntoParamHash (g, h)
      val rows = sortRows (List.map rowOfParam (ParamHash.list h))

      val () = ParamHash.freeAll h
      val () = AtlasFFI.atlas_group_free g

      val base = "atlas-scripts-sml/data/F4_FPP_points.txt"
      val path = if overwrite then base else base ^ ".new"
      val () = writePoints (path, rows)

      val () =
        TextIO.print
          ("Wrote:\n"
           ^ "  " ^ path ^ " (" ^ Int.toString (length rows) ^ " rows)\n")
    in
      ()
    end
end

val _ = GenF4FPPPoints.main ();
