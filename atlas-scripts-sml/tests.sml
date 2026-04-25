use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/groups.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Hermitian.sml";
use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/tests.sml

  Purpose
  - Incremental SML translation of `atlas-scripts/tests.at`.
  - The original `.at` script is a small test driver that:
      - defines some example groups,
      - runs the `test_unitarity.at` suite, and
      - runs a small “other” suite that inspects hermitian forms in the blocks
        of the trivial representation and of the spherical representation at
        infinitesimal character `rho/2`.

  What is implemented here
  - `groups1/groups2/groups3`:
      translated as lists of `AtlasFFI.group` handles, but **restricted** to
      constructors currently available in `atlas-scripts-sml/groups.sml`.
      (The `.at` list mentions complex groups like `SL(2,C)`; those are not
      currently constructed by this shim, so they are omitted.)
  - `test_other`:
      a functional analogue of the `.at` loop:
        - compute `block_of(trivial(G))`,
        - compute `block_of(scale(trivial(G),1/2))`,
        - for hermitian parameters, compute and print the purity of the
          irreducible hermitian form (equal-rank path).

  What is NOT implemented yet
  - The `.at` function `test_unitarity(groups)` calls:
      `print_test_spherical_unipotent`,
      `print_test_all_real_induced_one_dimensional`,
      `print_test_Aq`,
    from `test_unitarity.at`. The SML port `atlas-scripts-sml/test_unitarity.sml`
    is intentionally partial and does not provide these entry points, so
    `test_unitarity` here is currently a stub.

  Ownership / resource management
  - Group handles in `groups1/2/3` are freshly allocated and must be freed
    by callers (use `Tests.freeGroups`).
  - Parameters obtained from `Representations.block_of` are freshly cloned and
    must be freed (use `Tests.freeParams`).
  - Hermitian-form polynomials returned from `Hermitian.hermitian_form_irreducible`
    must be freed with `KTypePol.free`.
*)

structure Tests = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun freeParams (ps: param list) : unit = List.app AtlasFFI.atlas_param_free ps
  fun freeGroups (gs: group list) : unit = List.app AtlasFFI.atlas_group_free gs

  (* A reduced version of the `.at` group lists (only what `Groups` can build). *)
  val groups1 : group list =
    [Groups.SL_R 2, Groups.SL_R 3, Groups.SL_R 4]

  val groups2 : group list =
    [Groups.SL_R 2, Groups.SL_R 3, Groups.SL_R 4]

  val groups3 : group list =
    groups2 @ [Groups.PSO (5, 5)]

  (* `.at`-style helper: print one param with a short summary line. *)
  fun printParamSummary (p: param) : unit =
    let
      val x = AtlasFFI.atlas_param_x p
      val ht = AtlasFFI.atlas_param_height p
      val herm = AtlasFFI.atlas_param_is_hermitian p
      val fin = AtlasFFI.atlas_param_is_final p
      val std = AtlasFFI.atlas_param_is_standard p
    in
      TextIO.print
        ("param: x=" ^ Int.toString x
         ^ " ht=" ^ Int.toString ht
         ^ " standard=" ^ Int.toString std
         ^ " final=" ^ Int.toString fin
         ^ " hermitian=" ^ Int.toString herm
         ^ "\n")
    end

  (* Print a summary of the irreducible hermitian form (purity triple). *)
  fun printHermitianFormSummary (g: group, p: param) : unit =
    if AtlasFFI.atlas_param_is_hermitian p <> 1 then
      ()
    else
      let
        val pol = Hermitian.hermitian_form_irreducible p
        val r = AtlasFFI.atlas_group_rank g
        val purity = KTypePol.purityString (pol, r)
        val () = KTypePol.free pol
      in
        TextIO.print ("  hermitian_form_irreducible purity = " ^ purity ^ "\n")
      end

  (* Port of the `.at` `test_other([G])` loop. *)
  fun test_other (gs: group list) : unit =
    let
      fun oneGroup g =
        let
          val p0 = Representations.trivial g
          val psTriv = Representations.block_of p0
          val () = AtlasFFI.atlas_param_free p0

          val pHalf0 = Representations.trivial g
          val pHalf = AtlasFFI.atlas_param_scale (pHalf0, 1, 2)
          val () = AtlasFFI.atlas_param_free pHalf0
          val psHalf =
            if pHalf = Foreign.Memory.null then
              raise Fail ("Tests.test_other: scale(trivial,1/2) failed: " ^ AtlasFFI.atlas_last_error ())
            else
              let
                val qs = Representations.block_of pHalf
                val () = AtlasFFI.atlas_param_free pHalf
              in
                qs
              end

          fun inspect label ps =
            (TextIO.print (label ^ " (" ^ Int.toString (length ps) ^ " survivors)\n");
             List.app
               (fn p =>
                  (printParamSummary p;
                   printHermitianFormSummary (g, p)))
               ps)
        in
          TextIO.print "Testing block of trivial representation\n";
          inspect "block(trivial)" psTriv;
          TextIO.print "Testing block of spherical representation at rho/2\n";
          inspect "block(trivial*1/2)" psHalf;
          freeParams psHalf;
          freeParams psTriv
        end
    in
      List.app oneGroup gs
    end

  fun test_other1 (g: group) : unit = test_other [g]

  (* Stub for the `test_unitarity.at` suite. *)
  fun test_unitarity (_: group list) : unit =
    TextIO.print "Tests.test_unitarity: not yet ported (depends on test_unitarity.at entry points)\n"
end

