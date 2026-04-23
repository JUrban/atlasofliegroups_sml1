(*
  File: atlas-scripts-sml/ffi/AtlasFFI.sml

  Purpose
  - Poly/ML FFI bindings for a small “SML-facing Atlas API” implemented in
    `atlas-scripts-sml/ffi/atlas_smlffi.cpp` and linked as `libatlas_smlffi.so`.

  Design
  - This module is a thin, mostly 1:1 mapping from exported C symbols to SML
    functions. Names are intentionally long and match the C exports.
  - Many functions return `Foreign.Memory.null` or `"-1"` on error; callers
    should check and use `atlas_last_error()` for details.

  Ownership / lifetime rules (important)
  - `group`, `rootdatum`, `param`, and other handle types are opaque C++ objects
    allocated on the C++ side.
  - When a function name includes `new`/`clone`/`*_new_*`, it usually returns a
    freshly allocated handle that the caller must free via the corresponding
    `*_free` function.
  - When a function returns `cString`, the underlying C++ helper returns a
    pointer to an internal buffer owned by the library; treat it as ephemeral
    (copy it in SML if you need to keep it).

  Tip
  - Higher-level SML modules (e.g. `AllParameters`, `Parabolics`, `Induction`)
    wrap these low-level bindings into safer, more Atlas-script-like APIs.
*)
structure AtlasFFI = struct
  val lib = Foreign.loadLibrary "atlas-scripts-sml/ffi/libatlas_smlffi.so"

  val atlas_intmat_find_solution_text_sym =
    Foreign.getSymbol lib "atlas_intmat_find_solution_text"
  val atlas_intmat_kernel_text_sym = Foreign.getSymbol lib "atlas_intmat_kernel_text"
  val atlas_intmat_eigen_lattice_text_sym =
    Foreign.getSymbol lib "atlas_intmat_eigen_lattice_text"
  val atlas_intmat_smith_basis_text_sym =
    Foreign.getSymbol lib "atlas_intmat_smith_basis_text"
  val atlas_intmat_smith_diag_text_sym =
    Foreign.getSymbol lib "atlas_intmat_smith_diag_text"
  val atlas_intmat_diagonalize_sym = Foreign.getSymbol lib "atlas_intmat_diagonalize"
  val atlas_intmat_diagonalize_diag_text_sym =
    Foreign.getSymbol lib "atlas_intmat_diagonalize_diag_text"
  val atlas_intmat_diagonalize_row_text_sym =
    Foreign.getSymbol lib "atlas_intmat_diagonalize_row_text"
  val atlas_intmat_diagonalize_col_text_sym =
    Foreign.getSymbol lib "atlas_intmat_diagonalize_col_text"
  val atlas_intmat_diagonalize_free_sym =
    Foreign.getSymbol lib "atlas_intmat_diagonalize_free"
  val atlas_intmat_echelon_sym = Foreign.getSymbol lib "atlas_intmat_echelon"
  val atlas_intmat_echelon_M_text_sym = Foreign.getSymbol lib "atlas_intmat_echelon_M_text"
  val atlas_intmat_echelon_C_text_sym = Foreign.getSymbol lib "atlas_intmat_echelon_C_text"
  val atlas_intmat_echelon_pivots_text_sym =
    Foreign.getSymbol lib "atlas_intmat_echelon_pivots_text"
  val atlas_intmat_echelon_eps_sym = Foreign.getSymbol lib "atlas_intmat_echelon_eps"
  val atlas_intmat_echelon_free_sym = Foreign.getSymbol lib "atlas_intmat_echelon_free"
  val atlas_intmat_adapted_basis_sym = Foreign.getSymbol lib "atlas_intmat_adapted_basis"
  val atlas_intmat_adapted_basis_matrix_text_sym =
    Foreign.getSymbol lib "atlas_intmat_adapted_basis_matrix_text"
  val atlas_intmat_adapted_basis_diag_text_sym =
    Foreign.getSymbol lib "atlas_intmat_adapted_basis_diag_text"
  val atlas_intmat_adapted_basis_free_sym = Foreign.getSymbol lib "atlas_intmat_adapted_basis_free"
  val atlas_intmat_in_lattice_basis_text_sym =
    Foreign.getSymbol lib "atlas_intmat_in_lattice_basis_text"
  val atlas_intmat_cartan_matrix_type_text_sym =
    Foreign.getSymbol lib "atlas_intmat_cartan_matrix_type_text"

  val atlas_last_error_sym = Foreign.getSymbol lib "atlas_last_error"
  val atlas_kgb_size_F4_s_sym = Foreign.getSymbol lib "atlas_kgb_size_F4_s"
  val atlas_group_new_F4_s_sym = Foreign.getSymbol lib "atlas_group_new_F4_s"
  val atlas_group_new_simple_sym = Foreign.getSymbol lib "atlas_group_new_simple"
  val atlas_group_new_levi_of_parabolic_sym =
    Foreign.getSymbol lib "atlas_group_new_levi_of_parabolic"
  val atlas_group_free_sym = Foreign.getSymbol lib "atlas_group_free"
  val atlas_group_kgb_size_sym = Foreign.getSymbol lib "atlas_group_kgb_size"
  val atlas_group_rank_sym = Foreign.getSymbol lib "atlas_group_rank"
  val atlas_group_semisimple_rank_sym = Foreign.getSymbol lib "atlas_group_semisimple_rank"
  val atlas_group_real_form_number_sym = Foreign.getSymbol lib "atlas_group_real_form_number"
  val atlas_group_is_split_sym = Foreign.getSymbol lib "atlas_group_is_split"
  val atlas_group_is_compact_sym = Foreign.getSymbol lib "atlas_group_is_compact"
  val atlas_group_component_rank_sym = Foreign.getSymbol lib "atlas_group_component_rank"
  val atlas_group_rho_text_sym = Foreign.getSymbol lib "atlas_group_rho_text"
  val atlas_group_make_dominant_ratweight_text_sym =
    Foreign.getSymbol lib "atlas_group_make_dominant_ratweight_text"
  val atlas_group_from_dominant_ratweight_text_sym =
    Foreign.getSymbol lib "atlas_group_from_dominant_ratweight_text"
  val atlas_group_weyl_word_act_ratweight_text_sym =
    Foreign.getSymbol lib "atlas_group_weyl_word_act_ratweight_text"
  val atlas_group_simple_coroots_text_sym =
    Foreign.getSymbol lib "atlas_group_simple_coroots_text"
  val atlas_group_posroots_text_sym = Foreign.getSymbol lib "atlas_group_posroots_text"
  val atlas_group_num_real_forms_sym = Foreign.getSymbol lib "atlas_group_num_real_forms"
  val atlas_group_distinguished_involution_text_sym =
    Foreign.getSymbol lib "atlas_group_distinguished_involution_text"
  val atlas_group_rootdatum_new_sym = Foreign.getSymbol lib "atlas_group_rootdatum_new"
  val atlas_group_kgb_involution_matrix_text_sym =
    Foreign.getSymbol lib "atlas_group_kgb_involution_matrix_text"
  val atlas_group_kgb_involution_is_minus_identity_sym =
    Foreign.getSymbol lib "atlas_group_kgb_involution_is_minus_identity"
  val atlas_kgb_all_lambda_differential_0_text_sym =
    Foreign.getSymbol lib "atlas_kgb_all_lambda_differential_0_text"
  val atlas_kgb_status_sym = Foreign.getSymbol lib "atlas_kgb_status"
  val atlas_kgb_cross_sym = Foreign.getSymbol lib "atlas_kgb_cross"
  val atlas_kgb_cross_word_text_sym = Foreign.getSymbol lib "atlas_kgb_cross_word_text"
  val atlas_kgb_cayley_sym = Foreign.getSymbol lib "atlas_kgb_cayley"
  val atlas_kgb_length_sym = Foreign.getSymbol lib "atlas_kgb_length"
  val atlas_kgb_torus_factor_text_sym =
    Foreign.getSymbol lib "atlas_kgb_torus_factor_text"

  val atlas_rootdatum_new_simple_sym = Foreign.getSymbol lib "atlas_rootdatum_new_simple"
  val atlas_rootdatum_new_from_simple_mats_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_new_from_simple_mats_text"
  val atlas_rootdatum_free_sym = Foreign.getSymbol lib "atlas_rootdatum_free"
  val atlas_rootdatum_dual_sym = Foreign.getSymbol lib "atlas_rootdatum_dual"
  val atlas_rootdatum_rank_sym = Foreign.getSymbol lib "atlas_rootdatum_rank"
  val atlas_rootdatum_rho_text_sym = Foreign.getSymbol lib "atlas_rootdatum_rho_text"
  val atlas_rootdatum_make_dominant_ratweight_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_make_dominant_ratweight_text"
  val atlas_rootdatum_simple_roots_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_simple_roots_text"
  val atlas_rootdatum_simple_coroots_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_simple_coroots_text"
  val atlas_rootdatum_posroots_text_sym = Foreign.getSymbol lib "atlas_rootdatum_posroots_text"
  val atlas_rootdatum_poscoroots_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_poscoroots_text"
  val atlas_rootdatum_simple_factors_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_simple_factors_text"
  val atlas_rootdatum_roots_text_sym = Foreign.getSymbol lib "atlas_rootdatum_roots_text"
  val atlas_rootdatum_coroots_text_sym = Foreign.getSymbol lib "atlas_rootdatum_coroots_text"
  val atlas_rootdatum_root_coradical_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_root_coradical_text"
  val atlas_rootdatum_coroot_radical_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_coroot_radical_text"
  val atlas_rootdatum_FPP_orbit_numers_text_sym =
    Foreign.getSymbol lib "atlas_rootdatum_FPP_orbit_numers_text"

  val atlas_param_trivial_sym = Foreign.getSymbol lib "atlas_param_trivial"
  val atlas_param_free_sym = Foreign.getSymbol lib "atlas_param_free"
  val atlas_param_clone_sym = Foreign.getSymbol lib "atlas_param_clone"
  val atlas_param_normalise_sym = Foreign.getSymbol lib "atlas_param_normalise"
  val atlas_param_height_sym = Foreign.getSymbol lib "atlas_param_height"
  val atlas_param_group_handle_sym = Foreign.getSymbol lib "atlas_param_group_handle"
  val atlas_param_x_sym = Foreign.getSymbol lib "atlas_param_x"
  val atlas_param_lambda_text_sym = Foreign.getSymbol lib "atlas_param_lambda_text"
  val atlas_param_nu_text_sym = Foreign.getSymbol lib "atlas_param_nu_text"
  val atlas_param_gamma_text_sym = Foreign.getSymbol lib "atlas_param_gamma_text"
  val atlas_param_cross_sym = Foreign.getSymbol lib "atlas_param_cross"
  val atlas_param_cayley_sym = Foreign.getSymbol lib "atlas_param_cayley"
  val atlas_param_scale_sym = Foreign.getSymbol lib "atlas_param_scale"
  val atlas_param_reducibility_points_text_sym =
    Foreign.getSymbol lib "atlas_param_reducibility_points_text"
  val atlas_param_is_standard_sym = Foreign.getSymbol lib "atlas_param_is_standard"
  val atlas_param_is_final_sym = Foreign.getSymbol lib "atlas_param_is_final"
  val atlas_param_twist_sym = Foreign.getSymbol lib "atlas_param_twist"
  val atlas_param_equivalent_sym = Foreign.getSymbol lib "atlas_param_equivalent"
  val atlas_param_is_hermitian_sym = Foreign.getSymbol lib "atlas_param_is_hermitian"
  val atlas_param_finals_sym = Foreign.getSymbol lib "atlas_param_finals"
  val atlas_param_block_survivors_sym = Foreign.getSymbol lib "atlas_param_block_survivors"
  val atlas_paramlist_size_sym = Foreign.getSymbol lib "atlas_paramlist_size"
  val atlas_paramlist_start_pos_sym = Foreign.getSymbol lib "atlas_paramlist_start_pos"
  val atlas_paramlist_mult_sym = Foreign.getSymbol lib "atlas_paramlist_mult"
  val atlas_paramlist_get_param_clone_sym =
    Foreign.getSymbol lib "atlas_paramlist_get_param_clone"
  val atlas_paramlist_free_sym = Foreign.getSymbol lib "atlas_paramlist_free"
  val atlas_param_new_from_lambda_nu_sym =
    Foreign.getSymbol lib "atlas_param_new_from_lambda_nu"
  val atlas_param_new_from_lambda_nu_text_sym =
    Foreign.getSymbol lib "atlas_param_new_from_lambda_nu_text"
  val atlas_param_equal_sym = Foreign.getSymbol lib "atlas_param_equal"
  val atlas_param_hash_sym = Foreign.getSymbol lib "atlas_param_hash"
  val atlas_param_contragredient_sym = Foreign.getSymbol lib "atlas_param_contragredient"
  val atlas_param_full_deform_sym = Foreign.getSymbol lib "atlas_param_full_deform"
  val atlas_param_c_form_irreducible_sym = Foreign.getSymbol lib "atlas_param_c_form_irreducible"
  val atlas_param_hermitian_form_irreducible_sym =
    Foreign.getSymbol lib "atlas_param_hermitian_form_irreducible"
  val atlas_ktypepol_free_sym = Foreign.getSymbol lib "atlas_ktypepol_free"
  val atlas_ktypepol_num_terms_sym = Foreign.getSymbol lib "atlas_ktypepol_num_terms"
  val atlas_ktypepol_term_text_sym = Foreign.getSymbol lib "atlas_ktypepol_term_text"
  val atlas_ktypepol_is_typewise_pure_sym = Foreign.getSymbol lib "atlas_ktypepol_is_typewise_pure"
  val atlas_ktypepol_is_pure_sym = Foreign.getSymbol lib "atlas_ktypepol_is_pure"
  val atlas_ktypepol_impure_height_sym = Foreign.getSymbol lib "atlas_ktypepol_impure_height"
  val atlas_ktypepol_to_ht_sym = Foreign.getSymbol lib "atlas_ktypepol_to_ht"
  val atlas_ktypepol_clone_sym = Foreign.getSymbol lib "atlas_ktypepol_clone"
  val atlas_ktypepol_add_sym = Foreign.getSymbol lib "atlas_ktypepol_add"
  val atlas_ktypepol_scale_split_sym = Foreign.getSymbol lib "atlas_ktypepol_scale_split"
  val atlas_ktypepol_equal_sym = Foreign.getSymbol lib "atlas_ktypepol_equal"
  val atlas_ktypepol_hash_code_sym = Foreign.getSymbol lib "atlas_ktypepol_hash_code"
  val atlas_param_is_unitary_c_form_sym = Foreign.getSymbol lib "atlas_param_is_unitary_c_form"
  val atlas_param_is_unitary_sym = Foreign.getSymbol lib "atlas_param_is_unitary"
  val atlas_param_good_range_induced_from_first_text_sym =
    Foreign.getSymbol lib "atlas_param_good_range_induced_from_first_text"

  val atlas_ktype_free_sym = Foreign.getSymbol lib "atlas_ktype_free"
  val atlas_ktype_clone_sym = Foreign.getSymbol lib "atlas_ktype_clone"
  val atlas_ktype_equal_sym = Foreign.getSymbol lib "atlas_ktype_equal"
  val atlas_ktype_hash_code_sym = Foreign.getSymbol lib "atlas_ktype_hash_code"
  val atlas_param_LKTs_size_sym = Foreign.getSymbol lib "atlas_param_LKTs_size"
  val atlas_param_LKTs_get_ktype_clone_sym = Foreign.getSymbol lib "atlas_param_LKTs_get_ktype_clone"
  val atlas_param_LKTs_get_mult_sym = Foreign.getSymbol lib "atlas_param_LKTs_get_mult"
  val atlas_param_K_type_sym = Foreign.getSymbol lib "atlas_param_K_type"
  val atlas_ktype_parameter_sym = Foreign.getSymbol lib "atlas_ktype_parameter"
  val atlas_ktype_is_final_sym = Foreign.getSymbol lib "atlas_ktype_is_final"
  val atlas_ktype_x_sym = Foreign.getSymbol lib "atlas_ktype_x"
  val atlas_ktype_height_sym = Foreign.getSymbol lib "atlas_ktype_height"
  val atlas_ktype_next_to_lowest_sym = Foreign.getSymbol lib "atlas_ktype_next_to_lowest"
  val atlas_ktype_lambda_rho_text_sym = Foreign.getSymbol lib "atlas_ktype_lambda_rho_text"
  val atlas_ktype_new_from_x_lambda_rho_text_sym =
    Foreign.getSymbol lib "atlas_ktype_new_from_x_lambda_rho_text"
  val atlas_ktype_K_type_formula_sym = Foreign.getSymbol lib "atlas_ktype_K_type_formula"

  val atlas_intmat_find_solution_text =
    Foreign.buildCall2
      ( atlas_intmat_find_solution_text_sym
      , (Foreign.cString, Foreign.cString)
      , Foreign.cString
      )

  val atlas_intmat_kernel_text =
    Foreign.buildCall1 (atlas_intmat_kernel_text_sym, Foreign.cString, Foreign.cString)

  val atlas_intmat_eigen_lattice_text =
    Foreign.buildCall2
      ( atlas_intmat_eigen_lattice_text_sym
      , (Foreign.cString, Foreign.cInt)
      , Foreign.cString
      )

  val atlas_intmat_smith_basis_text =
    Foreign.buildCall1 (atlas_intmat_smith_basis_text_sym, Foreign.cString, Foreign.cString)

  val atlas_intmat_smith_diag_text =
    Foreign.buildCall1 (atlas_intmat_smith_diag_text_sym, Foreign.cString, Foreign.cString)

  type diagonalize = Foreign.Memory.voidStar

  val atlas_intmat_diagonalize =
    Foreign.buildCall1 (atlas_intmat_diagonalize_sym, Foreign.cString, Foreign.cPointer)

  val atlas_intmat_diagonalize_diag_text =
    Foreign.buildCall1 (atlas_intmat_diagonalize_diag_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_intmat_diagonalize_row_text =
    Foreign.buildCall1 (atlas_intmat_diagonalize_row_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_intmat_diagonalize_col_text =
    Foreign.buildCall1 (atlas_intmat_diagonalize_col_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_intmat_diagonalize_free =
    Foreign.buildCall1 (atlas_intmat_diagonalize_free_sym, Foreign.cPointer, Foreign.cVoid)

  type echelon = Foreign.Memory.voidStar

  val atlas_intmat_echelon =
    Foreign.buildCall1 (atlas_intmat_echelon_sym, Foreign.cString, Foreign.cPointer)

  val atlas_intmat_echelon_M_text =
    Foreign.buildCall1 (atlas_intmat_echelon_M_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_intmat_echelon_C_text =
    Foreign.buildCall1 (atlas_intmat_echelon_C_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_intmat_echelon_pivots_text =
    Foreign.buildCall1 (atlas_intmat_echelon_pivots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_intmat_echelon_eps =
    Foreign.buildCall1 (atlas_intmat_echelon_eps_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_intmat_echelon_free =
    Foreign.buildCall1 (atlas_intmat_echelon_free_sym, Foreign.cPointer, Foreign.cVoid)

  type adapted_basis = Foreign.Memory.voidStar

  val atlas_intmat_adapted_basis =
    Foreign.buildCall1 (atlas_intmat_adapted_basis_sym, Foreign.cString, Foreign.cPointer)

  val atlas_intmat_adapted_basis_matrix_text =
    Foreign.buildCall1 (atlas_intmat_adapted_basis_matrix_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_intmat_adapted_basis_diag_text =
    Foreign.buildCall1 (atlas_intmat_adapted_basis_diag_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_intmat_adapted_basis_free =
    Foreign.buildCall1 (atlas_intmat_adapted_basis_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_intmat_in_lattice_basis_text =
    Foreign.buildCall2
      ( atlas_intmat_in_lattice_basis_text_sym
      , (Foreign.cString, Foreign.cString)
      , Foreign.cString
      )

  val atlas_intmat_cartan_matrix_type_text =
    Foreign.buildCall1 (atlas_intmat_cartan_matrix_type_text_sym, Foreign.cString, Foreign.cString)

  val atlas_last_error =
    Foreign.buildCall0 (atlas_last_error_sym, (), Foreign.cString)

  val atlas_kgb_size_F4_s =
    Foreign.buildCall0 (atlas_kgb_size_F4_s_sym, (), Foreign.cLong)

  type group = Foreign.Memory.voidStar

  val atlas_group_new_F4_s =
    Foreign.buildCall0 (atlas_group_new_F4_s_sym, (), Foreign.cPointer)

  val atlas_group_new_simple =
    Foreign.buildCall4
      ( atlas_group_new_simple_sym
      , (Foreign.cChar, Foreign.cInt, Foreign.cChar, Foreign.cInt)
      , Foreign.cPointer
      )

  val atlas_group_new_levi_of_parabolic =
    Foreign.buildCall3
      ( atlas_group_new_levi_of_parabolic_sym
      , (Foreign.cPointer, Foreign.cString, Foreign.cInt)
      , Foreign.cPointer
      )

  val atlas_group_free =
    Foreign.buildCall1 (atlas_group_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_group_kgb_size =
    Foreign.buildCall1 (atlas_group_kgb_size_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_group_rank =
    Foreign.buildCall1 (atlas_group_rank_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_group_semisimple_rank =
    Foreign.buildCall1 (atlas_group_semisimple_rank_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_group_real_form_number =
    Foreign.buildCall1 (atlas_group_real_form_number_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_group_is_split =
    Foreign.buildCall1 (atlas_group_is_split_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_group_is_compact =
    Foreign.buildCall1 (atlas_group_is_compact_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_group_component_rank =
    Foreign.buildCall1 (atlas_group_component_rank_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_group_rho_text =
    Foreign.buildCall1 (atlas_group_rho_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_group_make_dominant_ratweight_text =
    Foreign.buildCall2
      ( atlas_group_make_dominant_ratweight_text_sym
      , (Foreign.cPointer, Foreign.cString)
      , Foreign.cString
      )

  val atlas_group_from_dominant_ratweight_text =
    Foreign.buildCall2
      ( atlas_group_from_dominant_ratweight_text_sym
      , (Foreign.cPointer, Foreign.cString)
      , Foreign.cString
      )

  val atlas_group_weyl_word_act_ratweight_text =
    Foreign.buildCall3
      ( atlas_group_weyl_word_act_ratweight_text_sym
      , (Foreign.cPointer, Foreign.cString, Foreign.cString)
      , Foreign.cString
      )

  val atlas_group_simple_coroots_text =
    Foreign.buildCall1 (atlas_group_simple_coroots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_group_posroots_text =
    Foreign.buildCall1 (atlas_group_posroots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_group_num_real_forms =
    Foreign.buildCall1 (atlas_group_num_real_forms_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_group_distinguished_involution_text =
    Foreign.buildCall1 (atlas_group_distinguished_involution_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_group_rootdatum_new =
    Foreign.buildCall1 (atlas_group_rootdatum_new_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_group_kgb_involution_matrix_text =
    Foreign.buildCall2
      ( atlas_group_kgb_involution_matrix_text_sym
      , (Foreign.cPointer, Foreign.cInt)
      , Foreign.cString
      )

  val atlas_group_kgb_involution_is_minus_identity =
    Foreign.buildCall2
      ( atlas_group_kgb_involution_is_minus_identity_sym
      , (Foreign.cPointer, Foreign.cInt)
      , Foreign.cInt
      )

  val atlas_kgb_all_lambda_differential_0_text =
    Foreign.buildCall2
      ( atlas_kgb_all_lambda_differential_0_text_sym
      , (Foreign.cPointer, Foreign.cInt)
      , Foreign.cString
      )

  val atlas_kgb_status =
    Foreign.buildCall3
      ( atlas_kgb_status_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cInt)
      , Foreign.cInt
      )

  val atlas_kgb_cross =
    Foreign.buildCall3
      ( atlas_kgb_cross_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cInt)
      , Foreign.cInt
      )

  val atlas_kgb_cross_word_text =
    Foreign.buildCall3
      ( atlas_kgb_cross_word_text_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cString)
      , Foreign.cInt
      )

  val atlas_kgb_cayley =
    Foreign.buildCall3
      ( atlas_kgb_cayley_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cInt)
      , Foreign.cInt
      )

  val atlas_kgb_length =
    Foreign.buildCall2 (atlas_kgb_length_sym, (Foreign.cPointer, Foreign.cInt), Foreign.cInt)

  val atlas_kgb_torus_factor_text =
    Foreign.buildCall2
      ( atlas_kgb_torus_factor_text_sym
      , (Foreign.cPointer, Foreign.cInt)
      , Foreign.cString
      )

  type rootdatum = Foreign.Memory.voidStar

  val atlas_rootdatum_new_simple =
    Foreign.buildCall3
      ( atlas_rootdatum_new_simple_sym
      , (Foreign.cChar, Foreign.cInt, Foreign.cInt)
      , Foreign.cPointer
      )

  val atlas_rootdatum_new_from_simple_mats_text =
    Foreign.buildCall3
      ( atlas_rootdatum_new_from_simple_mats_text_sym
      , (Foreign.cString, Foreign.cString, Foreign.cInt)
      , Foreign.cPointer
      )

  val atlas_rootdatum_free =
    Foreign.buildCall1 (atlas_rootdatum_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_rootdatum_dual =
    Foreign.buildCall1 (atlas_rootdatum_dual_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_rootdatum_rank =
    Foreign.buildCall1 (atlas_rootdatum_rank_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_rootdatum_rho_text =
    Foreign.buildCall1 (atlas_rootdatum_rho_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_make_dominant_ratweight_text =
    Foreign.buildCall2
      ( atlas_rootdatum_make_dominant_ratweight_text_sym
      , (Foreign.cPointer, Foreign.cString)
      , Foreign.cString
      )

  val atlas_rootdatum_simple_roots_text =
    Foreign.buildCall1 (atlas_rootdatum_simple_roots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_simple_coroots_text =
    Foreign.buildCall1 (atlas_rootdatum_simple_coroots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_posroots_text =
    Foreign.buildCall1 (atlas_rootdatum_posroots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_poscoroots_text =
    Foreign.buildCall1 (atlas_rootdatum_poscoroots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_simple_factors_text =
    Foreign.buildCall1 (atlas_rootdatum_simple_factors_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_roots_text =
    Foreign.buildCall1 (atlas_rootdatum_roots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_coroots_text =
    Foreign.buildCall1 (atlas_rootdatum_coroots_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_root_coradical_text =
    Foreign.buildCall1 (atlas_rootdatum_root_coradical_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_coroot_radical_text =
    Foreign.buildCall1 (atlas_rootdatum_coroot_radical_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_rootdatum_FPP_orbit_numers_text =
    Foreign.buildCall2
      ( atlas_rootdatum_FPP_orbit_numers_text_sym
      , (Foreign.cPointer, Foreign.cString)
      , Foreign.cString
      )

  type param = Foreign.Memory.voidStar

  val atlas_param_trivial =
    Foreign.buildCall1 (atlas_param_trivial_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_free =
    Foreign.buildCall1 (atlas_param_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_param_clone =
    Foreign.buildCall1 (atlas_param_clone_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_normalise =
    Foreign.buildCall1 (atlas_param_normalise_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_height =
    Foreign.buildCall1 (atlas_param_height_sym, Foreign.cPointer, Foreign.cLong)

  (* Borrowed pointer to the group handle associated to this parameter. *)
  val atlas_param_group_handle =
    Foreign.buildCall1 (atlas_param_group_handle_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_x =
    Foreign.buildCall1 (atlas_param_x_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_param_lambda_text =
    Foreign.buildCall1 (atlas_param_lambda_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_param_nu_text =
    Foreign.buildCall1 (atlas_param_nu_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_param_gamma_text =
    Foreign.buildCall1 (atlas_param_gamma_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_param_cross =
    Foreign.buildCall2 (atlas_param_cross_sym, (Foreign.cPointer, Foreign.cInt), Foreign.cPointer)

  val atlas_param_cayley =
    Foreign.buildCall2 (atlas_param_cayley_sym, (Foreign.cPointer, Foreign.cInt), Foreign.cPointer)

  val atlas_param_scale =
    Foreign.buildCall3
      ( atlas_param_scale_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cInt)
      , Foreign.cPointer
      )

  val atlas_param_reducibility_points_text =
    Foreign.buildCall1 (atlas_param_reducibility_points_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_param_is_standard =
    Foreign.buildCall1 (atlas_param_is_standard_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_param_is_final =
    Foreign.buildCall1 (atlas_param_is_final_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_param_twist =
    Foreign.buildCall1 (atlas_param_twist_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_equivalent =
    Foreign.buildCall2 (atlas_param_equivalent_sym, (Foreign.cPointer, Foreign.cPointer), Foreign.cInt)

  val atlas_param_is_hermitian =
    Foreign.buildCall1 (atlas_param_is_hermitian_sym, Foreign.cPointer, Foreign.cInt)

  type paramlist = Foreign.Memory.voidStar

  val atlas_param_finals =
    Foreign.buildCall1 (atlas_param_finals_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_block_survivors =
    Foreign.buildCall1 (atlas_param_block_survivors_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_paramlist_size =
    Foreign.buildCall1 (atlas_paramlist_size_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_paramlist_start_pos =
    Foreign.buildCall1 (atlas_paramlist_start_pos_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_paramlist_mult =
    Foreign.buildCall2 (atlas_paramlist_mult_sym, (Foreign.cPointer, Foreign.cLong), Foreign.cLong)

  val atlas_paramlist_get_param_clone =
    Foreign.buildCall2
      ( atlas_paramlist_get_param_clone_sym
      , (Foreign.cPointer, Foreign.cLong)
      , Foreign.cPointer
      )

  val atlas_paramlist_free =
    Foreign.buildCall1 (atlas_paramlist_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_param_new_from_lambda_nu =
    Foreign.buildCall6
      ( atlas_param_new_from_lambda_nu_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cInt, Foreign.cPointer, Foreign.cInt, Foreign.cPointer)
      , Foreign.cPointer
      )

  val atlas_param_new_from_lambda_nu_text =
    Foreign.buildCall6
      ( atlas_param_new_from_lambda_nu_text_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cString, Foreign.cInt, Foreign.cString, Foreign.cInt)
      , Foreign.cPointer
      )

  val atlas_param_equal =
    Foreign.buildCall2
      ( atlas_param_equal_sym
      , (Foreign.cPointer, Foreign.cPointer)
      , Foreign.cInt
      )

  val atlas_param_hash =
    Foreign.buildCall2
      ( atlas_param_hash_sym
      , (Foreign.cPointer, Foreign.cLong)
      , Foreign.cLong
      )

  val atlas_param_contragredient =
    Foreign.buildCall1 (atlas_param_contragredient_sym, Foreign.cPointer, Foreign.cPointer)

  type ktypepol = Foreign.Memory.voidStar

  val atlas_param_full_deform =
    Foreign.buildCall1 (atlas_param_full_deform_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_c_form_irreducible =
    Foreign.buildCall1 (atlas_param_c_form_irreducible_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_hermitian_form_irreducible =
    Foreign.buildCall1
      ( atlas_param_hermitian_form_irreducible_sym
      , Foreign.cPointer
      , Foreign.cPointer
      )

  val atlas_ktypepol_free =
    Foreign.buildCall1 (atlas_ktypepol_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_ktypepol_num_terms =
    Foreign.buildCall1 (atlas_ktypepol_num_terms_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_ktypepol_term_text =
    Foreign.buildCall2 (atlas_ktypepol_term_text_sym, (Foreign.cPointer, Foreign.cLong), Foreign.cString)

  val atlas_ktypepol_is_typewise_pure =
    Foreign.buildCall1 (atlas_ktypepol_is_typewise_pure_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_ktypepol_is_pure =
    Foreign.buildCall1 (atlas_ktypepol_is_pure_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_ktypepol_impure_height =
    Foreign.buildCall1 (atlas_ktypepol_impure_height_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_ktypepol_to_ht =
    Foreign.buildCall2 (atlas_ktypepol_to_ht_sym, (Foreign.cPointer, Foreign.cInt), Foreign.cPointer)

  val atlas_ktypepol_clone =
    Foreign.buildCall1 (atlas_ktypepol_clone_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_ktypepol_add =
    Foreign.buildCall2 (atlas_ktypepol_add_sym, (Foreign.cPointer, Foreign.cPointer), Foreign.cPointer)

  val atlas_ktypepol_scale_split =
    Foreign.buildCall3
      ( atlas_ktypepol_scale_split_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cInt)
      , Foreign.cPointer
      )

  val atlas_ktypepol_equal =
    Foreign.buildCall2 (atlas_ktypepol_equal_sym, (Foreign.cPointer, Foreign.cPointer), Foreign.cInt)

  val atlas_ktypepol_hash_code =
    Foreign.buildCall2 (atlas_ktypepol_hash_code_sym, (Foreign.cPointer, Foreign.cLong), Foreign.cLong)

  val atlas_param_is_unitary_c_form =
    Foreign.buildCall1 (atlas_param_is_unitary_c_form_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_param_is_unitary =
    Foreign.buildCall1 (atlas_param_is_unitary_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_param_good_range_induced_from_first_text =
    Foreign.buildCall1
      ( atlas_param_good_range_induced_from_first_text_sym
      , Foreign.cPointer
      , Foreign.cString
      )

  type ktype = Foreign.Memory.voidStar

  val atlas_ktype_free =
    Foreign.buildCall1 (atlas_ktype_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_ktype_clone =
    Foreign.buildCall1 (atlas_ktype_clone_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_ktype_equal =
    Foreign.buildCall2 (atlas_ktype_equal_sym, (Foreign.cPointer, Foreign.cPointer), Foreign.cInt)

  val atlas_ktype_hash_code =
    Foreign.buildCall2 (atlas_ktype_hash_code_sym, (Foreign.cPointer, Foreign.cLong), Foreign.cLong)

  val atlas_param_LKTs_size =
    Foreign.buildCall1 (atlas_param_LKTs_size_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_param_LKTs_get_ktype_clone =
    Foreign.buildCall2 (atlas_param_LKTs_get_ktype_clone_sym, (Foreign.cPointer, Foreign.cLong), Foreign.cPointer)

  val atlas_param_LKTs_get_mult =
    Foreign.buildCall2 (atlas_param_LKTs_get_mult_sym, (Foreign.cPointer, Foreign.cLong), Foreign.cLong)

  val atlas_param_K_type =
    Foreign.buildCall1 (atlas_param_K_type_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_ktype_parameter =
    Foreign.buildCall1 (atlas_ktype_parameter_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_ktype_is_final =
    Foreign.buildCall1 (atlas_ktype_is_final_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_ktype_x =
    Foreign.buildCall1 (atlas_ktype_x_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_ktype_height =
    Foreign.buildCall1 (atlas_ktype_height_sym, Foreign.cPointer, Foreign.cInt)

  val atlas_ktype_next_to_lowest =
    Foreign.buildCall1 (atlas_ktype_next_to_lowest_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_ktype_lambda_rho_text =
    Foreign.buildCall1 (atlas_ktype_lambda_rho_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_ktype_new_from_x_lambda_rho_text =
    Foreign.buildCall3
      ( atlas_ktype_new_from_x_lambda_rho_text_sym
      , (Foreign.cPointer, Foreign.cInt, Foreign.cString)
      , Foreign.cPointer
      )

  val atlas_ktype_K_type_formula =
    Foreign.buildCall2 (atlas_ktype_K_type_formula_sym, (Foreign.cPointer, Foreign.cInt), Foreign.cPointer)
end
