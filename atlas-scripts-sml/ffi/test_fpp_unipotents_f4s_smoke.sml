use "atlas-scripts-sml/FPP_unipotents.sml";

(*
  Smoke test: inserting `F4_s` unipotents populates a `ParamHash`.
*)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ();

val h = ParamHash.create 512;
val () = FPPFlags.unip_flag := true;
val () = FPP_unipotents.unipotents_to_paramhash (g, h);

val n = ParamHash.size h;
val () = if n > 0 then () else raise Fail "expected some unipotent params";

val () = ParamHash.freeAll h;
val () = AtlasFFI.atlas_group_free g;

val () = TextIO.print "OK\n";

