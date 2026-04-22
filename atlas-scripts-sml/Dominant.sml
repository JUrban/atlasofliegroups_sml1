use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure Dominant = struct
  fun toCText (s: string) : string =
    String.translate (fn #"~" => "-" | c => str c) s

  fun makeDominantText (g: AtlasFFI.group) (ratweightText: string) : string =
    let
      val out = AtlasFFI.atlas_group_make_dominant_ratweight_text (g, toCText ratweightText)
    in
      case Int.fromString (hd (String.tokens Char.isSpace out)) of
        SOME ~1 => raise Fail ("Dominant.makeDominantText failed: " ^ AtlasFFI.atlas_last_error ())
      | _ => out
    end
end
