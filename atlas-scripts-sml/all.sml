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
val () = tryUse "atlas-scripts-sml/generics.sml"
val () = tryUse "atlas-scripts-sml/combinatorics.sml"
val () = tryUse "atlas-scripts-sml/number_theory.sml"
val () = tryUse "atlas-scripts-sml/misc.sml"
val () = tryUse "atlas-scripts-sml/lazy_lists.sml"
val () = tryUse "atlas-scripts-sml/exp-generating-series.sml"
val () = tryUse "atlas-scripts-sml/sort.sml"
val () = tryUse "atlas-scripts-sml/tabulate.sml"
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
val () = tryUse "atlas-scripts-sml/lattice_aux.sml"
val () = tryUse "atlas-scripts-sml/lattice.sml"

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

(* Geometry / polytope helpers used by FPP code. *)
val () = tryUse "atlas-scripts-sml/aff_cube.sml"
val () = tryUse "atlas-scripts-sml/chopping_facets.sml"
val () = tryUse "atlas-scripts-sml/chopping_facets_fast.sml"

(* KGB / Weyl-group transport and Bruhat order (KGB part). *)
val () = tryUse "atlas-scripts-sml/WeylWord.sml"
val () = tryUse "atlas-scripts-sml/Weylgroup.sml"
val () = tryUse "atlas-scripts-sml/weylgroup_at.sml"
val () = tryUse "atlas-scripts-sml/bruhat.sml"
val () = tryUse "atlas-scripts-sml/standardize.sml"

(* FFI-facing representation utilities (partial ports). *)
val () = tryUse "atlas-scripts-sml/representations.sml"
val () = tryUse "atlas-scripts-sml/parameters.sml"
val () = tryUse "atlas-scripts-sml/complex.sml"
val () = tryUse "atlas-scripts-sml/is_normal.sml"
val () = tryUse "atlas-scripts-sml/complementary_series.sml"
val () = tryUse "atlas-scripts-sml/tits.sml"
val () = tryUse "atlas-scripts-sml/test_unitarity.sml"

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
     `speh.at`, ...
*)
