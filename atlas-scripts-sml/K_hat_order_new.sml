use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/K_hat_order_new.sml

  Purpose
  - Placeholder SML translation of `atlas-scripts/K_hat_order_new.at`.
  - The `.at` script defines a new preorder on pairs `(x,lambda)` (KGB element
    and imaginary-dominant weight) motivated by composition-factor relations,
    and provides exploratory enumeration routines using custom hash tables.

  Status
  - Not yet implemented in SML. A faithful port requires:
      - an `xlambda_hash` (hash set) port,
      - reflection/simple-root weight updates and “standard” tests for
        `(x,lambda)` beyond what is currently exposed,
      - and a clear decision about which parts are intended as reusable library
        versus one-off experimental code.
*)

structure K_hat_order_new = struct
  type group = AtlasFFI.group
  type ratvec = {den: int, nums: int list}

  fun xl_start (_: group, _: int, _: ratvec, _: int) : unit =
    raise Fail "K_hat_order_new.xl_start: not yet ported"
end
