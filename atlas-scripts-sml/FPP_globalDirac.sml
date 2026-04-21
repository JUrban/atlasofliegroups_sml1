use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryHash.sml";
use "atlas-scripts-sml/FPPFlags.sml";

structure FPP_globalDirac = struct
  fun verify_unitary_dual (hash: BigUnitaryHash.t) : unit =
    let
      val ps = BigUnitaryHash.list hash
      fun checkOne p =
        let
          val q = AtlasFFI.atlas_param_contragredient p
          val () =
            if q = Foreign.Memory.null then
              raise Fail ("contragredient failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val ok = BigUnitaryHash.contains hash q
          val () = AtlasFFI.atlas_param_free q
        in
          ok
        end

      val missing = List.filter (fn p => not (checkOne p)) ps
    in
      if null missing then
        if !FPPFlags.final_verbose then TextIO.print "unitary_dual check: OK\n" else ()
      else
        raise Fail ("unitary_dual check: missing contragredients: " ^ Int.toString (length missing))
    end

  fun FPP_unitary_hash_bottom_layer (_: AtlasFFI.group, hash: BigUnitaryHash.t) : unit =
    let
      val () =
        if !FPPFlags.final_verbose then
          TextIO.print "FPP_unitary_hash_bottom_layer: starting\n"
        else
          ()
      val () = verify_unitary_dual hash
      val () =
        if !FPPFlags.final_verbose then
          TextIO.print "FPP_unitary_hash_bottom_layer: TODO (more checks)\n"
        else
          ()
    in
      ()
    end
end
