use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/FPPFlags.sml";
use "atlas-scripts-sml/F4_s_unipdata.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/ParamHash.sml";

(*
  File: atlas-scripts-sml/FPP_unipotents.sml

  Purpose
  - SML replacement for the `.at` helper `unipotents_to_hash(G,Uhash)` used by
    the FPP/local-Dirac scripts.

  Atlas correspondence
  - In `.at`, `unipotents_to_hash` inserts `unipotent_representations(G)` into
    a global/unitary hash (guarded by the flag `unip_flag`).

  Implementation status
  - We currently provide an `F4_s`-specific implementation backed by the
    translated data table `atlas-scripts-sml/F4_s_unipdata.sml`.
  - For other groups, this module is a no-op (for now).

  Ownership
  - This module allocates temporary parameters during insertion and frees them.
  - `ParamHash` owns clones of inserted final parameters.
*)

structure FPP_unipotents = struct
  type group = AtlasFFI.group
  type ratvec = Lattice.ratvec

  fun parseLeadingInt (s: string) : int option =
    case String.tokens Char.isSpace s of
      [] => NONE
    | tok :: _ => Int.fromString tok

  fun looksLikeF4s (g: group) : bool =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
      val isSplit = AtlasFFI.atlas_group_is_split g = 1
      val nPosRoots =
        (case parseLeadingInt (AtlasFFI.atlas_group_posroots_text g) of
           SOME n => n
         | NONE => ~1)
    in
      isSplit andalso rank = 4 andalso kgbSize = 229 andalso nPosRoots = 24
    end

  fun addFinalsToParamHash (out: ParamHash.t, p: AtlasFFI.param) : unit =
    let
      val finals = ParamFinals.finals p
      fun addOne (q, mult) =
        if mult = 0 then
          AtlasFFI.atlas_param_free q
        else
          (ignore (ParamHash.match out q); AtlasFFI.atlas_param_free q)
      val () = List.app addOne finals
    in
      ()
    end

  (* Insert unipotent unitary parameters for supported groups, ignoring flags. *)
  fun insert_unipotents_to_paramhash (g: group, out: ParamHash.t) : unit =
    if not (looksLikeF4s g) then
      ()
    else
      let
        fun addEntry (x, lambda: ratvec, nu: ratvec) =
          let
            val p0 = Representations.parameter (g, x, lambda, nu)
            val p1 = AtlasFFI.atlas_param_normalise p0
            val () = AtlasFFI.atlas_param_free p0
            val () =
              if p1 = Foreign.Memory.null then
                raise Fail ("FPP_unipotents: normalise failed: " ^ AtlasFFI.atlas_last_error ())
              else
                ()
            val () = addFinalsToParamHash (out, p1)
            val () = AtlasFFI.atlas_param_free p1
          in
            ()
          end
      in
        List.app addEntry F4_s_unipdata.data
      end

  (*
    Insert known unipotent unitary parameters into `out` (as final terms).

    This mirrors the `.at` behavior at the level of “what ends up in the hash”,
    but we do not attempt to reconstruct `unipotent_representations(G)` for
    general groups yet.
  *)
  fun unipotents_to_paramhash (g: group, out: ParamHash.t) : unit =
    if not (!FPPFlags.unip_flag) then
      ()
    else
      insert_unipotents_to_paramhash (g, out)
end
