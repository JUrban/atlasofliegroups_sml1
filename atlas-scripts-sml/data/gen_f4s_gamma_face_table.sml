use "atlas-scripts-sml/FPPFaceKey.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  Script: atlas-scripts-sml/data/gen_f4s_gamma_face_table.sml

  Purpose
  - Generate `atlas-scripts-sml/data/f4s_gamma_face_table.txt`, a precomputed
    mapping from folded-FPP barycenter keys to global face keys for `F4_s`.

  Usage
  - Run from repo root:
      `poly -q < atlas-scripts-sml/data/gen_f4s_gamma_face_table.sml`
*)

fun ratvecKey (u: Lattice.ratvec) : int list =
  let
    val u = Lattice.ratvecNormalize u
  in
    #den u :: #nums u
  end;

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val faceCtx = FPPFaceKey.create g;
val barycenters = FPP_barycenters_fold.barycenters_all g;

fun entry gamma =
  case FPPFaceKey.faceKeyOfGamma (faceCtx, gamma) of
    NONE => raise Fail "missing face key for barycenter"
  | SOME fk => (ratvecKey gamma, fk);

val entries = List.map entry barycenters;
val entriesSorted = Basic.sort_by (fn (k, _) => k, Sort.rlex_leq) entries;

val outPath = "atlas-scripts-sml/data/f4s_gamma_face_table.txt";
val outs = TextIO.openOut outPath;

fun intsToLine xs = String.concatWith " " (List.map Int.toString xs);
fun writeOne (key, fk) =
  let
    val line = intsToLine key ^ " | " ^ Int.toString (length fk) ^ " " ^ intsToLine fk ^ "\n"
  in
    TextIO.output (outs, line)
  end;

val () = List.app writeOne entriesSorted;
val () = TextIO.closeOut outs;
val () = AtlasFFI.atlas_group_free g;

val () = print ("Wrote " ^ Int.toString (length entriesSorted) ^ " lines to " ^ outPath ^ "\n");

