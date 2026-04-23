(* Smoke test for `atlas-scripts-sml/generate_groups.sml`. *)
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/generate_groups.sml";

val () =
  let
    val gs = GenerateGroups.all_simple (GenerateGroups.SC, 1)
    val () = print ("all_simple(SC,1) count = " ^ Int.toString (length gs) ^ "\n")
    val () = List.app AtlasFFI.atlas_group_free gs

    val gs2 = GenerateGroups.all_simple (GenerateGroups.AD, 1)
    val () = print ("all_simple(AD,1) count = " ^ Int.toString (length gs2) ^ "\n")
    val () = List.app AtlasFFI.atlas_group_free gs2
  in
    ()
  end;

