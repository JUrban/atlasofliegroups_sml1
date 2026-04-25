(*  File: atlas-scripts-sml/all.sml

    Purpose
    - SML analogue of `atlas-scripts/all.at`.
    - In the `.at` world, `all.at` is a convenience “loader” that pulls in a
      recommended collection of library scripts.
    - Here we provide a similar loader for the SML ports under
      `atlas-scripts-sml/`.

    Important difference
    - Many of the `.at` files referenced by `all.at` have not yet been ported to
      SML (and some depend on Atlas interpreter features not yet exposed via
      the Poly/ML FFI). This loader therefore:
        - Loads the SML equivalents that currently exist.
        - Skips the rest (documented below).

    Usage
    - `poly -q < atlas-scripts-sml/all.sml`
*)

fun tryUse (path: string) : unit =
  (use path
   handle e =>
     (print ("[all.sml] WARNING: failed to load " ^ path ^ "\n");
      raise e))

(* Core utilities already ported. *)
val () = tryUse "atlas-scripts-sml/basic.sml"
val () = tryUse "atlas-scripts-sml/std_decs.sml"
val () = tryUse "atlas-scripts-sml/generics.sml"
val () = tryUse "atlas-scripts-sml/partitions.sml"
val () = tryUse "atlas-scripts-sml/combinatorics.sml"
val () = tryUse "atlas-scripts-sml/elliptic.sml"
val () = tryUse "atlas-scripts-sml/number_theory.sml"
val () = tryUse "atlas-scripts-sml/misc.sml"
val () = tryUse "atlas-scripts-sml/structure_constants.sml"
val () = tryUse "atlas-scripts-sml/lazy_lists.sml"
val () = tryUse "atlas-scripts-sml/exp-generating-series.sml"
val () = tryUse "atlas-scripts-sml/sort.sml"
val () = tryUse "atlas-scripts-sml/tabulate.sml"
val () = tryUse "atlas-scripts-sml/poset.sml"
val () = tryUse "atlas-scripts-sml/StringListData.sml"
val () = tryUse "atlas-scripts-sml/generate_groups.sml"
val () = tryUse "atlas-scripts-sml/groups.sml"
val () = tryUse "atlas-scripts-sml/groups_at.sml"
val () = tryUse "atlas-scripts-sml/center.sml"
val () = tryUse "atlas-scripts-sml/lietypes.sml"
val () = tryUse "atlas-scripts-sml/isomorphism.sml"
val () = tryUse "atlas-scripts-sml/group_operations.sml"
val () = tryUse "atlas-scripts-sml/A1.sml"

(* Matrix / lattice helpers. *)
val () = tryUse "atlas-scripts-sml/matrix.sml"
val () = tryUse "atlas-scripts-sml/ratmat.sml"
val () = tryUse "atlas-scripts-sml/Gaussian_elim.sml"
val () = tryUse "atlas-scripts-sml/Smith.sml"
val () = tryUse "atlas-scripts-sml/lattice_aux.sml"
val () = tryUse "atlas-scripts-sml/lattice.sml"
val () = tryUse "atlas-scripts-sml/twist.sml"

(* Polynomial utilities. *)
val () = tryUse "atlas-scripts-sml/polynomial.sml"
val () = tryUse "atlas-scripts-sml/inverse.sml"
val () = tryUse "atlas-scripts-sml/laurentPolynomial.sml"
val () = tryUse "atlas-scripts-sml/Split.sml"
val () = tryUse "atlas-scripts-sml/ParamPol.sml"
val () = tryUse "atlas-scripts-sml/extParamPol.sml"
val () = tryUse "atlas-scripts-sml/deform.sml"
val () = tryUse "atlas-scripts-sml/iterate_deform.sml"
val () = tryUse "atlas-scripts-sml/deform_plus.sml"
val () = tryUse "atlas-scripts-sml/KL_polynomial_matrices.sml"
val () = tryUse "atlas-scripts-sml/extended_misc.sml"
val () = tryUse "atlas-scripts-sml/bigMatrices.sml"
val () = tryUse "atlas-scripts-sml/GKfast.sml"
val () = tryUse "atlas-scripts-sml/generic_degrees.sml"
val () = tryUse "atlas-scripts-sml/GK_dimension.sml"
val () = tryUse "atlas-scripts-sml/Dirac.sml"
val () = tryUse "atlas-scripts-sml/DiracCoh.sml"
val () = tryUse "atlas-scripts-sml/K_Nilpotent.sml"
val () = tryUse "atlas-scripts-sml/Levi_subgroups.sml"
val () = tryUse "atlas-scripts-sml/character_table_E6.sml"
val () = tryUse "atlas-scripts-sml/character_table_E7.sml"
val () = tryUse "atlas-scripts-sml/character_table_E8.sml"
val () = tryUse "atlas-scripts-sml/class_tables.sml"
val () = tryUse "atlas-scripts-sml/character_tables.sml"
val () = tryUse "atlas-scripts-sml/classical_character_tables.sml"
val () = tryUse "atlas-scripts-sml/character_tables_reductive.sml"
val () = tryUse "atlas-scripts-sml/character_table_reps.sml"
val () = tryUse "atlas-scripts-sml/character_table_F.sml"
val () = tryUse "atlas-scripts-sml/character_table_G.sml"
val () = tryUse "atlas-scripts-sml/e8_gap.sml"

(* Geometry / polytope helpers used by FPP code. *)
val () = tryUse "atlas-scripts-sml/aff_cube.sml"
val () = tryUse "atlas-scripts-sml/chopping_facets.sml"
val () = tryUse "atlas-scripts-sml/chopping_facets_fast.sml"

(* KGB / Weyl-group transport and Bruhat order (KGB part). *)
val () = tryUse "atlas-scripts-sml/WeylWord.sml"
val () = tryUse "atlas-scripts-sml/Weylgroup.sml"
val () = tryUse "atlas-scripts-sml/weylgroup_at.sml"
val () = tryUse "atlas-scripts-sml/WeylElt.sml"
val () = tryUse "atlas-scripts-sml/Wdelta.sml"
val () = tryUse "atlas-scripts-sml/bruhat.sml"
val () = tryUse "atlas-scripts-sml/conjugacy_class_partial_order.sml"
val () = tryUse "atlas-scripts-sml/W_order.sml"
val () = tryUse "atlas-scripts-sml/standardize.sml"
val () = tryUse "atlas-scripts-sml/K_norm.sml"
val () = tryUse "atlas-scripts-sml/genuine.sml"
val () = tryUse "atlas-scripts-sml/K.sml"
val () = tryUse "atlas-scripts-sml/K_types.sml"
val () = tryUse "atlas-scripts-sml/K_types_plus.sml"
val () = tryUse "atlas-scripts-sml/K_type_formula_generalized.sml"
val () = tryUse "atlas-scripts-sml/K_type_formula.sml"

(* FFI-facing representation utilities (partial ports). *)
val () = tryUse "atlas-scripts-sml/representations.sml"
val () = tryUse "atlas-scripts-sml/parameters.sml"
val () = tryUse "atlas-scripts-sml/translate.sml"
val () = tryUse "atlas-scripts-sml/convert_c_form.sml"
val () = tryUse "atlas-scripts-sml/new_blocks.sml"
val () = tryUse "atlas-scripts-sml/dual.sml"
val () = tryUse "atlas-scripts-sml/c_form_branch.sml"
val () = tryUse "atlas-scripts-sml/complex.sml"
val () = tryUse "atlas-scripts-sml/is_normal.sml"
val () = tryUse "atlas-scripts-sml/complementary_series.sml"
val () = tryUse "atlas-scripts-sml/tits.sml"
val () = tryUse "atlas-scripts-sml/test_unitarity.sml"
val () = tryUse "atlas-scripts-sml/tests.sml"
val () = tryUse "atlas-scripts-sml/lunitest.sml"
val () = tryUse "atlas-scripts-sml/sp4.sml"
val () = tryUse "atlas-scripts-sml/restricted_roots.sml"
val () = tryUse "atlas-scripts-sml/tensor_product_sl2.sml"
val () = tryUse "atlas-scripts-sml/tensor_product_A1.sml"
val () = tryUse "atlas-scripts-sml/Tensor_Products.sml"
val () = tryUse "atlas-scripts-sml/unitary_induction.sml"
val () = tryUse "atlas-scripts-sml/speh.sml"
val () = tryUse "atlas-scripts-sml/wallace.sml"
val () = tryUse "atlas-scripts-sml/weakly_unipotent.sml"
val () = tryUse "atlas-scripts-sml/harmonic.sml"
val () = tryUse "atlas-scripts-sml/induction_sp4.sml"
val () = tryUse "atlas-scripts-sml/nilpotent_induction.sml"
val () = tryUse "atlas-scripts-sml/paramRep.sml"
val () = tryUse "atlas-scripts-sml/arthur_parameters.sml"
val () = tryUse "atlas-scripts-sml/disconnected.sml"
val () = tryUse "atlas-scripts-sml/verma.sml"
val () = tryUse "atlas-scripts-sml/gl4H.sml"
val () = tryUse "atlas-scripts-sml/LKT_form.sml"
val () = tryUse "atlas-scripts-sml/nci_nilrad_roots.sml"
val () = tryUse "atlas-scripts-sml/support.sml"
val () = tryUse "atlas-scripts-sml/weyltosemisimple.sml"
val () = tryUse "atlas-scripts-sml/springer_table_A.sml"
val () = tryUse "atlas-scripts-sml/springer_table_G.sml"
val () = tryUse "atlas-scripts-sml/weak_unip.sml"
val () = tryUse "atlas-scripts-sml/hodge_functions.sml"
val () = tryUse "atlas-scripts-sml/extKLbug.sml"
val () = tryUse "atlas-scripts-sml/classical_W_classes_and_reps.sml"
val () = tryUse "atlas-scripts-sml/isomorphism_W.sml"
val () = tryUse "atlas-scripts-sml/cohomology.sml"
val () = tryUse "atlas-scripts-sml/adams_johnson.sml"
val () = tryUse "atlas-scripts-sml/test_extended.sml"
val () = tryUse "atlas-scripts-sml/extended_types.sml"
val () = tryUse "atlas-scripts-sml/coherent.sml"
val () = tryUse "atlas-scripts-sml/modules.sml"
val () = tryUse "atlas-scripts-sml/print_K_types.sml"
val () = tryUse "atlas-scripts-sml/paramChamber.sml"
val () = tryUse "atlas-scripts-sml/springer_tables_reductive.sml"
val () = tryUse "atlas-scripts-sml/springer_table_F.sml"
val () = tryUse "atlas-scripts-sml/nilpotent_orbits_exceptional.sml"
val () = tryUse "atlas-scripts-sml/nilpotent_orbit_partitions.sml"
val () = tryUse "atlas-scripts-sml/finite_unipotents.sml"
val () = tryUse "atlas-scripts-sml/weak_packet_reports.sml"
val () = tryUse "atlas-scripts-sml/ext_deform.sml"
val () = tryUse "atlas-scripts-sml/twisted_conjugacy.sml"
val () = tryUse "atlas-scripts-sml/smallQ.sml"
val () = tryUse "atlas-scripts-sml/ellipticExceptional.sml"
val () = tryUse "atlas-scripts-sml/test_non_distinguished.sml"
val () = tryUse "atlas-scripts-sml/exceptionalData.sml"
val () = tryUse "atlas-scripts-sml/hodge_test.sml"
val () = tryUse "atlas-scripts-sml/hodge_tensor.sml"
val () = tryUse "atlas-scripts-sml/new_conjugacy.sml"
val () = tryUse "atlas-scripts-sml/red_count.sml"

(* Not yet ported from `all.at` (as of this checkpoint)
   - `extParamPol.at`, `modules.at`, `print_K_types.at`, `galois.at`,
     `jantzen.at`, `finite_dimensional_signature.at`, `W_reps.at`,
     `hodge_K_type_formula.at`, `all_finite_order.at`, `hodge_test.at`,
     `hodge_tensor.at`, `K_Nilpotent.at`, `exceptionalNilpotentData.at`,
     `good_W_representatives.at`, `finite_unipotents.at`, `weyltosemisimple.at`,
     `cyclotomic_field_bracket.at`, `tits_centralizer.at`, `stable.at`,
     `GK_dimension.at`, `restricted_roots.at`, `nilpotent_centralizer.at`,
      `weak_packets_precomputed_cell_traces.at`, `arthur_parameters.at`,
     `coherent_irreducible.at`, `truncated_induction.at`, `dirac_index.at`,
     `sub_cells.at`, `associated_variety_annihilator.at`, `sommers.at`,
     `projectors_using_character_tables.at`, `new_conjugacy.at`, `test_braid.at`,
     `geck_generic.at`, `lusztig_cells.at`, `four.at`, `special_rep.at`,
     `families.at`, `FPP_globalDirac.at`, `L_packet.at`, `adams_johnson.at`,
     ...
*)
