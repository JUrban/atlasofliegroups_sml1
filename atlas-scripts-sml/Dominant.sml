use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/Dominant.sml

  Purpose
  - Wrapper around the Atlas operation “make a rational weight dominant” for a
    given real form/group, exposed through the SML FFI.

  Notes
  - Atlas expects `-` for negatives; Poly/ML prints negatives as `~n`.
    `toCText` performs this translation.
*)
structure Dominant = struct
  (* Translate SML `~` negatives to C-style `-` negatives. *)
  fun toCText (s: string) : string =
    String.translate (fn #"~" => "-" | c => str c) s

  (* Call into Atlas to make a rational weight dominant; returns Atlas text. *)
  fun makeDominantText (g: AtlasFFI.group) (ratweightText: string) : string =
    let
      val out = AtlasFFI.atlas_group_make_dominant_ratweight_text (g, toCText ratweightText)
    in
      case Int.fromString (hd (String.tokens Char.isSpace out)) of
        SOME ~1 => raise Fail ("Dominant.makeDominantText failed: " ^ AtlasFFI.atlas_last_error ())
      | _ => out
    end
end
