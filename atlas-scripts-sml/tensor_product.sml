use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/tensor_product.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/tensor_product.at`.
  - The `.at` script provides assorted tensor-product examples/utilities.

  Status
  - Not yet ported. Some tensor-product-related helpers exist elsewhere in the
    SML port (`atlas-scripts-sml/tensor_product_sl2.sml`,
    `atlas-scripts-sml/Tensor_Products.sml`, `atlas-scripts-sml/tensor_product_A1.sml`),
    but this script’s particular API surface has not been reconstructed.
*)

structure Tensor_product = struct
  fun TODO (_: string) : 'a =
    raise Fail "Tensor_product: not yet ported"
end

