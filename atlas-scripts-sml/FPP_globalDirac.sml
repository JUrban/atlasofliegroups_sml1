use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryHash.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/FPP_lambdas_fold.sml";
use "atlas-scripts-sml/FPPFlags.sml";
use "atlas-scripts-sml/ParamHash.sml";

(*
  File: atlas-scripts-sml/FPP_globalDirac.sml

  Purpose
  - Port/adapter of the “global Dirac” verification pipeline used by the F4
    unitary facet computation scripts.
  - Provides checks used by `VerifyF4FPP.run()` and the translated verifier:
      - standard/final sanity checks
      - lambda-table consistency checks (F4-specific)
      - twist-equivalence checks
      - hermitian/unitary checks (via Atlas FFI)

  Notes
  - The original `.at` scripts use global flags to control verbosity and the
    scope of checks; this module reads `FPPFlags`.
*)
structure FPP_globalDirac = struct
  type param = AtlasFFI.param

  type param_set = {list: unit -> param list, contains: param -> bool}

  (* View a `BigUnitaryHash` as a generic `param_set`. *)
  fun param_set_of_big_unitary_hash (h: BigUnitaryHash.t) : param_set =
    {list = fn () => BigUnitaryHash.list h, contains = fn p => BigUnitaryHash.contains h p}

  (* View a `ParamHash` as a generic `param_set`. *)
  fun param_set_of_param_hash (h: ParamHash.t) : param_set =
    {list = fn () => ParamHash.list h, contains = fn p => ParamHash.contains h p}

  (* Verify that every parameter in `hash` is standard and final. *)
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

  (* F4-specific consistency check: each parameter’s `lambda` must come from the
     precomputed `FPP_lambdas_fold` table for its `x`. *)
  fun verify_F4_points_lambdas_ok (g: AtlasFFI.group, hash: BigUnitaryHash.t) : unit =
    let
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
    in
      if kgbSize <> 229 then
        ()
      else
        let
          fun intToCText n =
            let
              val s = Int.toString n
            in
              if String.size s > 0 andalso String.sub (s, 0) = #"~" then
                "-" ^ String.extract (s, 1, NONE)
              else
                s
            end

          fun ratvecText ({den, nums}: {den: int, nums: int list}) : string =
            String.concatWith " " (intToCText den :: List.map intToCText nums)

          val lambdasByX = FPP_lambdas_fold.FPP_lambdas_table g
          val lambdaTextsByX = Array.tabulate (kgbSize, fn x => List.map ratvecText (Array.sub (lambdasByX, x)))

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
          if !FPPFlags.final_verbose then
            TextIO.print "F4 lambda-table check: OK\n"
          else
            ()
        end
    end

  (* Same as `verify_F4_points_lambdas_ok` but for a generic `param_set`. *)
  fun verify_F4_points_lambdas_ok_set (g: AtlasFFI.group, set: param_set) : unit =
    let
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
    in
      if kgbSize <> 229 then
        ()
      else
        let
          fun intToCText n =
            let
              val s = Int.toString n
            in
              if String.size s > 0 andalso String.sub (s, 0) = #"~" then
                "-" ^ String.extract (s, 1, NONE)
              else
                s
            end

          fun ratvecText ({den, nums}: {den: int, nums: int list}) : string =
            String.concatWith " " (intToCText den :: List.map intToCText nums)

          val lambdasByX = FPP_lambdas_fold.FPP_lambdas_table g
          val lambdaTextsByX = Array.tabulate (kgbSize, fn x => List.map ratvecText (Array.sub (lambdasByX, x)))

          val ps = #list set ()

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
          if !FPPFlags.final_verbose then
            TextIO.print "F4 lambda-table check: OK\n"
          else
            ()
        end
    end

  (* Verify that `p` is equivalent to its Atlas twist `twist(p)`. *)
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

  (* Set-based version of `verify_all_standard_final`. *)
  fun verify_all_standard_final_set (set: param_set) : unit =
    let
      val ps = #list set ()
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

  (* Set-based version of `verify_all_twist_equivalent`. *)
  fun verify_all_twist_equivalent_set (set: param_set) : unit =
    let
      val ps = #list set ()
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

  (* Verify that all parameters in the set are hermitian. *)
  fun verify_all_hermitian_set (set: param_set) : unit =
    let
      val ps = #list set ()
      val bad = List.filter (fn p => AtlasFFI.atlas_param_is_hermitian p <> 1) ps
    in
      if null bad then
        if !FPPFlags.final_verbose then TextIO.print "hermitian check: OK\n" else ()
      else
        raise Fail ("hermitian check: non-hermitian params: " ^ Int.toString (length bad))
    end

  (* Verify that all parameters in the set are unitary, with optional progress prints. *)
  fun verify_all_unitary_set (set: param_set) : unit =
    let
      val ps = #list set ()
      val total = length ps

      fun loop ([], checked, bad) = (checked, bad)
        | loop (p :: rest, checked, bad) =
            let
              val u = AtlasFFI.atlas_param_is_unitary p
              val bad' = if u = 1 then bad else bad + 1
              val checked' = checked + 1
              val () =
                if !FPPFlags.final_verbose andalso checked' mod 100 = 0 then
                  TextIO.print
                    ("unitary progress: " ^ Int.toString checked' ^ "/" ^ Int.toString total
                     ^ " bad=" ^ Int.toString bad' ^ "\n")
                else
                  ()
            in
              loop (rest, checked', bad')
            end

      val (_, bad) = loop (ps, 0, 0)
    in
      if bad = 0 then
        if !FPPFlags.final_verbose then TextIO.print "unitary check: OK\n" else ()
      else
        raise Fail ("unitary check: non-unitary params: " ^ Int.toString bad)
    end

  (* Verify closure under contragredient within the set (unitary dual symmetry). *)
  fun verify_unitary_dual_set (set: param_set) : unit =
    let
      val ps = #list set ()
      fun checkOne p =
        let
          val q = AtlasFFI.atlas_param_contragredient p
          val () =
            if q = Foreign.Memory.null then
              raise Fail ("contragredient failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val ok = #contains set q
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

  (* Hash-based version of `verify_all_hermitian_set`. *)
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

  (* Hash-based version of `verify_all_unitary_set`. *)
  fun verify_all_unitary (hash: BigUnitaryHash.t) : unit =
    let
      val ps = BigUnitaryHash.list hash
      val total = length ps

      fun loop ([], checked, bad) = (checked, bad)
        | loop (p :: rest, checked, bad) =
            let
              val u = AtlasFFI.atlas_param_is_unitary p
              val bad' = if u = 1 then bad else bad + 1
              val checked' = checked + 1
              val () =
                if !FPPFlags.final_verbose andalso checked' mod 100 = 0 then
                  TextIO.print
                    ("unitary progress: " ^ Int.toString checked' ^ "/" ^ Int.toString total
                     ^ " bad=" ^ Int.toString bad' ^ "\n")
                else
                  ()
            in
              loop (rest, checked', bad')
            end

      val (_, bad) = loop (ps, 0, 0)
    in
      if bad = 0 then
        if !FPPFlags.final_verbose then TextIO.print "unitary check: OK\n" else ()
      else
        raise Fail ("unitary check: non-unitary params: " ^ Int.toString bad)
    end

  (* Hash-based version of `verify_unitary_dual_set`. *)
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

  (* Run the full bottom-layer verification pipeline for a generic parameter set. *)
  fun FPP_unitary_hash_bottom_layer_set (g: AtlasFFI.group, set: param_set) : unit =
    let
      val () =
        if !FPPFlags.final_verbose then
          TextIO.print "FPP_unitary_hash_bottom_layer: starting\n"
        else
          ()
      val () = verify_all_standard_final_set set
      val () = verify_F4_points_lambdas_ok_set (g, set)
      val () = verify_all_twist_equivalent_set set
      val () = verify_all_hermitian_set set
      val () =
        if !FPPFlags.Dirac_flag then
          verify_all_unitary_set set
        else
          ()
      val () = verify_unitary_dual_set set
      val () =
        if !FPPFlags.final_verbose then
          TextIO.print "FPP_unitary_hash_bottom_layer: done\n"
        else
          ()
    in
      ()
    end

  (* Convenience wrapper for `BigUnitaryHash` inputs. *)
  fun FPP_unitary_hash_bottom_layer (g: AtlasFFI.group, hash: BigUnitaryHash.t) : unit =
    FPP_unitary_hash_bottom_layer_set (g, param_set_of_big_unitary_hash hash)

  (* Convenience wrapper for `ParamHash` inputs; for compact groups, seeds the set
     with parameters at infinitesimal character `rho`. *)
  fun FPP_unitary_hash_bottom_layer_param_hash (g: AtlasFFI.group, hash: ParamHash.t) : unit =
    if AtlasFFI.atlas_group_is_compact g = 1 then
      let
        val rho = AllParameters.parseRatWeightText (AtlasFFI.atlas_group_rho_text g)
        val ps = AllParameters.all_parameters_gamma (g, rho)

        fun addOne p =
          (ignore (ParamHash.match hash p);
           AtlasFFI.atlas_param_free p)
        val () = List.app addOne ps
      in
        ()
      end
    else
      FPP_unitary_hash_bottom_layer_set (g, param_set_of_param_hash hash)
end
