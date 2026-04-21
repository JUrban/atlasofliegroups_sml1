structure AtlasFFI = struct
  val lib = Foreign.loadLibrary "atlas-scripts-sml/ffi/libatlas_smlffi.so"

  val atlas_last_error_sym = Foreign.getSymbol lib "atlas_last_error"
  val atlas_kgb_size_F4_s_sym = Foreign.getSymbol lib "atlas_kgb_size_F4_s"
  val atlas_group_new_F4_s_sym = Foreign.getSymbol lib "atlas_group_new_F4_s"
  val atlas_group_free_sym = Foreign.getSymbol lib "atlas_group_free"
  val atlas_group_kgb_size_sym = Foreign.getSymbol lib "atlas_group_kgb_size"

  val atlas_last_error =
    Foreign.buildCall0 (atlas_last_error_sym, (), Foreign.cString)

  val atlas_kgb_size_F4_s =
    Foreign.buildCall0 (atlas_kgb_size_F4_s_sym, (), Foreign.cLong)

  type group = Foreign.Memory.voidStar

  val atlas_group_new_F4_s =
    Foreign.buildCall0 (atlas_group_new_F4_s_sym, (), Foreign.cPointer)

  val atlas_group_free =
    Foreign.buildCall1 (atlas_group_free_sym, Foreign.cPointer, Foreign.cVoid)

  val atlas_group_kgb_size =
    Foreign.buildCall1 (atlas_group_kgb_size_sym, Foreign.cPointer, Foreign.cLong)
end
