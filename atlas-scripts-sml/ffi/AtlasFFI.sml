structure AtlasFFI = struct
  val lib = Foreign.loadLibrary "atlas-scripts-sml/ffi/libatlas_smlffi.so"

  val atlas_last_error_sym = Foreign.getSymbol lib "atlas_last_error"
  val atlas_kgb_size_F4_s_sym = Foreign.getSymbol lib "atlas_kgb_size_F4_s"
  val atlas_group_new_F4_s_sym = Foreign.getSymbol lib "atlas_group_new_F4_s"
  val atlas_group_new_simple_sym = Foreign.getSymbol lib "atlas_group_new_simple"
  val atlas_group_free_sym = Foreign.getSymbol lib "atlas_group_free"
  val atlas_group_kgb_size_sym = Foreign.getSymbol lib "atlas_group_kgb_size"
  val atlas_group_num_real_forms_sym = Foreign.getSymbol lib "atlas_group_num_real_forms"

  val atlas_param_trivial_sym = Foreign.getSymbol lib "atlas_param_trivial"
  val atlas_param_free_sym = Foreign.getSymbol lib "atlas_param_free"
  val atlas_param_height_sym = Foreign.getSymbol lib "atlas_param_height"

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

  type param = Foreign.Memory.voidStar

  val atlas_param_trivial =
    Foreign.buildCall1 (atlas_param_trivial_sym, Foreign.cPointer, Foreign.cPointer)

  val atlas_param_free =
    Foreign.buildCall1 (atlas_param_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_param_height =
    Foreign.buildCall1 (atlas_param_height_sym, Foreign.cPointer, Foreign.cLong)
end
