structure AtlasFFI = struct
  val lib = Foreign.loadLibrary "atlas-scripts-sml/ffi/libatlas_smlffi.so"

  val atlas_last_error_sym = Foreign.getSymbol lib "atlas_last_error"
  val atlas_kgb_size_F4_s_sym = Foreign.getSymbol lib "atlas_kgb_size_F4_s"
  val atlas_group_new_F4_s_sym = Foreign.getSymbol lib "atlas_group_new_F4_s"
  val atlas_group_new_simple_sym = Foreign.getSymbol lib "atlas_group_new_simple"
  val atlas_group_free_sym = Foreign.getSymbol lib "atlas_group_free"
  val atlas_group_kgb_size_sym = Foreign.getSymbol lib "atlas_group_kgb_size"
  val atlas_group_num_real_forms_sym = Foreign.getSymbol lib "atlas_group_num_real_forms"
  val atlas_group_kgb_involution_is_minus_identity_sym =
    Foreign.getSymbol lib "atlas_group_kgb_involution_is_minus_identity"

  val atlas_param_trivial_sym = Foreign.getSymbol lib "atlas_param_trivial"
  val atlas_param_free_sym = Foreign.getSymbol lib "atlas_param_free"
  val atlas_param_clone_sym = Foreign.getSymbol lib "atlas_param_clone"
  val atlas_param_height_sym = Foreign.getSymbol lib "atlas_param_height"
  val atlas_param_x_sym = Foreign.getSymbol lib "atlas_param_x"
  val atlas_param_lambda_text_sym = Foreign.getSymbol lib "atlas_param_lambda_text"
  val atlas_param_nu_text_sym = Foreign.getSymbol lib "atlas_param_nu_text"
  val atlas_param_is_standard_sym = Foreign.getSymbol lib "atlas_param_is_standard"
  val atlas_param_is_final_sym = Foreign.getSymbol lib "atlas_param_is_final"
  val atlas_param_twist_sym = Foreign.getSymbol lib "atlas_param_twist"
  val atlas_param_equivalent_sym = Foreign.getSymbol lib "atlas_param_equivalent"
  val atlas_param_is_hermitian_sym = Foreign.getSymbol lib "atlas_param_is_hermitian"
  val atlas_param_new_from_lambda_nu_sym =
    Foreign.getSymbol lib "atlas_param_new_from_lambda_nu"
  val atlas_param_new_from_lambda_nu_text_sym =
    Foreign.getSymbol lib "atlas_param_new_from_lambda_nu_text"
  val atlas_param_equal_sym = Foreign.getSymbol lib "atlas_param_equal"
  val atlas_param_hash_sym = Foreign.getSymbol lib "atlas_param_hash"
  val atlas_param_contragredient_sym = Foreign.getSymbol lib "atlas_param_contragredient"
  val atlas_param_full_deform_sym = Foreign.getSymbol lib "atlas_param_full_deform"
  val atlas_param_c_form_irreducible_sym = Foreign.getSymbol lib "atlas_param_c_form_irreducible"
  val atlas_ktypepol_free_sym = Foreign.getSymbol lib "atlas_ktypepol_free"
  val atlas_ktypepol_num_terms_sym = Foreign.getSymbol lib "atlas_ktypepol_num_terms"
  val atlas_ktypepol_term_text_sym = Foreign.getSymbol lib "atlas_ktypepol_term_text"
  val atlas_ktypepol_is_typewise_pure_sym = Foreign.getSymbol lib "atlas_ktypepol_is_typewise_pure"
  val atlas_ktypepol_is_pure_sym = Foreign.getSymbol lib "atlas_ktypepol_is_pure"
  val atlas_param_is_unitary_c_form_sym = Foreign.getSymbol lib "atlas_param_is_unitary_c_form"

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

  val atlas_group_free =
    Foreign.buildCall1 (atlas_group_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_group_kgb_size =
    Foreign.buildCall1 (atlas_group_kgb_size_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_group_num_real_forms =
    Foreign.buildCall1 (atlas_group_num_real_forms_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_group_kgb_involution_is_minus_identity =
    Foreign.buildCall2
      ( atlas_group_kgb_involution_is_minus_identity_sym
      , (Foreign.cPointer, Foreign.cInt)
      , Foreign.cInt
      )

  type param = Foreign.Memory.voidStar

  val atlas_param_trivial =
    Foreign.buildCall1 (atlas_param_trivial_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_free =
    Foreign.buildCall1 (atlas_param_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_param_clone =
    Foreign.buildCall1 (atlas_param_clone_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_height =
    Foreign.buildCall1 (atlas_param_height_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_param_x =
    Foreign.buildCall1 (atlas_param_x_sym, Foreign.cPointer, Foreign.cLong)

  val atlas_param_lambda_text =
    Foreign.buildCall1 (atlas_param_lambda_text_sym, Foreign.cPointer, Foreign.cString)

  val atlas_param_nu_text =
    Foreign.buildCall1 (atlas_param_nu_text_sym, Foreign.cPointer, Foreign.cString)

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

  val atlas_param_is_unitary_c_form =
    Foreign.buildCall1 (atlas_param_is_unitary_c_form_sym, Foreign.cPointer, Foreign.cInt)
end
