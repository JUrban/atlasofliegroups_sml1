use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryHash.sml";
use "atlas-scripts-sml/F4_FPP_lambdas.sml";
use "atlas-scripts-sml/FPPFlags.sml";

structure FPP_globalDirac = struct
  fun verify_all_standard_final (hash: BigUnitaryHash.t) : unit =
    let
      val ps = BigUnitaryHash.list hash
      val nonStandard = List.filter (fn p => AtlasFFI.atlas_param_is_standard p <> 1) ps
      val nonFinal = List.filter (fn p => AtlasFFI.atlas_param_is_final p <> 1) ps
    in
      if null nonStandard andalso null nonFinal then
        if !FPPFlags.final_verbose then TextIO.print "standard/final check: OK\n" else ()
      else
        raise Fail
          ("standard/final check: nonStandard="
           ^ Int.toString (length nonStandard)
           ^ " nonFinal="
           ^ Int.toString (length nonFinal))
    end

  fun verify_F4_points_lambdas_ok (g: AtlasFFI.group, hash: BigUnitaryHash.t) : unit =
    let
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
    in
      if kgbSize <> 229 then
        ()
      else
        let
          fun rvText ({numsText, denom}: {numsText: string, denom: int}) = Int.toString denom ^ " " ^ numsText

          val lambdasByX = F4_FPP_lambdas.load kgbSize
          val lambdaTextsByX =
            Array.tabulate
              ( kgbSize
              , fn x => List.map rvText (Array.sub (lambdasByX, x))
              )

          val ps = BigUnitaryHash.list hash

          fun checkOne p =
            let
              val x = AtlasFFI.atlas_param_x p
              val lam = AtlasFFI.atlas_param_lambda_text p
              val () =
                if x < 0 orelse x >= kgbSize then
                  raise Fail ("F4 aux check: x out of range: " ^ Int.toString x)
                else
                  ()
              val okLam = List.exists (fn s => s = lam) (Array.sub (lambdaTextsByX, x))
            in
              if okLam then
                ()
              else
                raise Fail
                  ("F4 aux check failed: x="
                   ^ Int.toString x
                   ^ " lambda not in FPP_lambdas table: "
                   ^ lam)
            end

          val () = List.app checkOne ps
        in
          if !FPPFlags.final_verbose then TextIO.print "F4 lambda-table check: OK\n" else ()
        end
    end

  fun verify_all_twist_equivalent (hash: BigUnitaryHash.t) : unit =
    let
      val ps = BigUnitaryHash.list hash
      fun badOne p =
        let
          val q = AtlasFFI.atlas_param_twist p
          val () =
            if q = Foreign.Memory.null then
              raise Fail ("twist failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val eq = AtlasFFI.atlas_param_equivalent (p, q) = 1
          val () = AtlasFFI.atlas_param_free q
        in
          not eq
        end
      val bad = List.filter badOne ps
    in
      if null bad then
        if !FPPFlags.final_verbose then TextIO.print "twist-equivalence check: OK\n" else ()
      else
        raise Fail ("twist-equivalence check: failed for " ^ Int.toString (length bad) ^ " params")
    end

  fun verify_all_hermitian (hash: BigUnitaryHash.t) : unit =
    let
      val ps = BigUnitaryHash.list hash
      val bad = List.filter (fn p => AtlasFFI.atlas_param_is_hermitian p <> 1) ps
    in
      if null bad then
        if !FPPFlags.final_verbose then TextIO.print "hermitian check: OK\n" else ()
      else
        raise Fail ("hermitian check: non-hermitian params: " ^ Int.toString (length bad))
    end

  fun verify_all_unitary_c_form (hash: BigUnitaryHash.t) : unit =
    let
      val ps = BigUnitaryHash.list hash
      val total = length ps

      fun loop ([], checked, bad) = (checked, bad)
        | loop (p :: rest, checked, bad) =
            let
              val u = AtlasFFI.atlas_param_is_unitary_c_form p
              val bad' = if u = 1 then bad else bad + 1
              val checked' = checked + 1
              val () =
                if !FPPFlags.final_verbose andalso checked' mod 100 = 0 then
                  TextIO.print
                    ("unitary(c-form) progress: " ^ Int.toString checked' ^ "/" ^ Int.toString total
                     ^ " bad=" ^ Int.toString bad' ^ "\n")
                else
                  ()
            in
              loop (rest, checked', bad')
            end

      val (_, bad) = loop (ps, 0, 0)
    in
      if bad = 0 then
        if !FPPFlags.final_verbose then TextIO.print "unitary(c-form) check: OK\n" else ()
      else
        raise Fail ("unitary(c-form) check: non-unitary params: " ^ Int.toString bad)
    end

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

  fun FPP_unitary_hash_bottom_layer (g: AtlasFFI.group, hash: BigUnitaryHash.t) : unit =
    let
      val () =
        if !FPPFlags.final_verbose then
          TextIO.print "FPP_unitary_hash_bottom_layer: starting\n"
        else
          ()
      val () = verify_all_standard_final hash
      val () = verify_F4_points_lambdas_ok (g, hash)
      val () = verify_all_twist_equivalent hash
      val () = verify_all_hermitian hash
      val () =
        if !FPPFlags.Dirac_flag then
          verify_all_unitary_c_form hash
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
