use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/FPP_lambdas_fold.sml";
use "atlas-scripts-sml/FPP_vertices_fold.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/data/gen_F4_FPP_aux_data.sml

  Purpose
  - Pure-SML replacement for `atlas-scripts-sml/data/gen_F4_FPP_aux_data.at`.
  - Regenerates the fixture files:
      - `atlas-scripts-sml/data/F4_FPP_barycenters.txt`
      - `atlas-scripts-sml/data/F4_FPP_lambdas.txt`
    using the SML folded-FPP implementations (`FPP_barycenters_fold`,
    `FPP_lambdas_fold`) and the Atlas C++ library via FFI.

  Usage
  - From the repo root:
      `poly -q < atlas-scripts-sml/data/gen_F4_FPP_aux_data.sml`

  Notes
  - The output is deterministic: we normalize, sort, and deduplicate using the
    same key order used in the regression tests.
  - This script writes files in-place; it is intended for maintainers who want
    to refresh fixtures after changing the folded-FPP algorithms.
*)

structure GenF4FPPAuxData = struct
  type ratvec = Lattice.ratvec

  fun envFlag (name: string) : bool =
    (case OS.Process.getEnv name of
       SOME "1" => true
     | SOME "true" => true
     | SOME "yes" => true
     | _ => false)

  fun key (u: ratvec) : int list =
    let
      val u = Lattice.ratvecNormalize u
    in
      #den u :: #nums u
    end

  fun noReps (us: ratvec list) : ratvec list =
    Basic.sort_u_by (key, Sort.rlex_leq) us

  fun writeBarycenters (path: string, barys: ratvec list) : unit =
    let
      val out = TextIO.openOut path
      fun line u =
        let
          val u = Lattice.ratvecNormalize u
          val nums = #nums u
        in
          case nums of
            [a, b, c, d] =>
              TextIO.output
                (out, Int.toString (#den u) ^ " " ^ Int.toString a ^ " " ^ Int.toString b ^ " " ^ Int.toString c ^ " " ^ Int.toString d ^ "\n")
          | _ => raise Fail "writeBarycenters: expected length 4"
        end
    in
      (List.app line barys; TextIO.closeOut out) handle e => (TextIO.closeOut out; raise e)
    end

  fun writeVertices (path: string, verts: ratvec list) : unit =
    let
      val out = TextIO.openOut path
      fun line u =
        let
          val u = Lattice.ratvecNormalize u
          val nums = #nums u
        in
          case nums of
            [a, b, c, d] =>
              TextIO.output
                (out, Int.toString (#den u) ^ " " ^ Int.toString a ^ " " ^ Int.toString b ^ " " ^ Int.toString c ^ " " ^ Int.toString d ^ "\n")
          | _ => raise Fail "writeVertices: expected length 4"
        end
    in
      (List.app line verts; TextIO.closeOut out) handle e => (TextIO.closeOut out; raise e)
    end

  fun writeLambdas (path: string, lambdasByX: ratvec list array) : unit =
    let
      val out = TextIO.openOut path
      val kgbSize = Array.length lambdasByX
      fun writeX x =
        let
          val lams = noReps (Array.sub (lambdasByX, x))
          fun line u =
            let
              val u = Lattice.ratvecNormalize u
              val nums = #nums u
            in
              case nums of
                [a, b, c, d] =>
                  TextIO.output
                    ( out
                    , Int.toString x ^ " " ^ Int.toString (#den u) ^ " " ^ Int.toString a ^ " " ^ Int.toString b ^ " " ^ Int.toString c ^ " " ^ Int.toString d
                      ^ "\n"
                    )
              | _ => raise Fail "writeLambdas: expected length 4"
            end
        in
          List.app line lams
        end
    in
      (List.app writeX (List.tabulate (kgbSize, fn i => i)); TextIO.closeOut out) handle e => (TextIO.closeOut out; raise e)
    end

  fun main () : unit =
    let
      val overwrite = envFlag "OVERWRITE"
      val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
      val () =
        if g = Foreign.Memory.null then
          raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()

      val barys = noReps (FPP_barycenters_fold.barycenters_all g)
      val verts = noReps (FPP_vertices_fold.vertices g)
      val lambdasByX = FPP_lambdas_fold.FPP_lambdas_table g

      val () = AtlasFFI.atlas_group_free g

      val baryBase = "atlas-scripts-sml/data/F4_FPP_barycenters.txt"
      val vertBase = "atlas-scripts-sml/data/F4_FPP_vertices.txt"
      val lamBase = "atlas-scripts-sml/data/F4_FPP_lambdas.txt"
      val baryPath = if overwrite then baryBase else baryBase ^ ".new"
      val vertPath = if overwrite then vertBase else vertBase ^ ".new"
      val lamPath = if overwrite then lamBase else lamBase ^ ".new"
      val () = writeBarycenters (baryPath, barys)
      val () = writeVertices (vertPath, verts)
      val () = writeLambdas (lamPath, lambdasByX)

      val () =
        TextIO.print
          ("Wrote:\n"
           ^ "  " ^ baryPath ^ " (" ^ Int.toString (length barys) ^ " rows)\n"
           ^ "  " ^ vertPath ^ " (" ^ Int.toString (length verts) ^ " rows)\n"
           ^ "  " ^ lamPath ^ " (" ^ Int.toString (Array.length lambdasByX) ^ " x-buckets)\n")
    in
      ()
    end
end

val _ = GenF4FPPAuxData.main ();
