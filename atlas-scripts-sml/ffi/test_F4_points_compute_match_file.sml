use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/F4_FPP_points.sml";
use "atlas-scripts-sml/F4_FPP_points_compute.sml";
use "atlas-scripts-sml/FPPFlags.sml";

val () =
  let
    val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
    val () = FPPFlags.Dirac_flag := true

    val fromFile = ParamHash.create 4096
    val () = F4_FPP_points.loadIntoParamHash (g, fromFile)

    val computedAll = ParamHash.create 4096
    val () = F4_FPP_points_compute.computeAllIntoParamHash (g, computedAll)

    fun requireSubset (a: ParamHash.t, b: ParamHash.t, name: string) =
      let
        val ps = ParamHash.list a
        val missing = List.filter (fn p => not (ParamHash.contains b p)) ps
      in
        if null missing then
          ()
        else
          raise Fail (name ^ ": missing=" ^ Int.toString (length missing))
      end

    val () =
      if ParamHash.size fromFile = 1864 then
        ()
      else
        raise Fail ("unexpected file size: " ^ Int.toString (ParamHash.size fromFile))

    val () = requireSubset (fromFile, computedAll, "file_not_subset_of_computed")
    val () = requireSubset (computedAll, fromFile, "computed_not_subset_of_file")
    val () =
      if ParamHash.size computedAll = ParamHash.size fromFile then
        ()
      else
        raise Fail ("size mismatch: computed=" ^ Int.toString (ParamHash.size computedAll) ^ " file="
                    ^ Int.toString (ParamHash.size fromFile))
    val () = TextIO.print ("computed size=" ^ Int.toString (ParamHash.size computedAll) ^ "\n")

    val () = ParamHash.freeAll computedAll
    val () = ParamHash.freeAll fromFile
    val () = AtlasFFI.atlas_group_free g
  in
    TextIO.print "OK\n"
  end;
