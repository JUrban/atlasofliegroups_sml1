use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/support.sml

  Purpose
  - SML translation scaffold for `atlas-scripts/support.at`.
  - The `.at` script filters parameters/cells according to whether their KGB
    element (or its dual) has “proper support” (support size smaller than the
    semisimple rank).

  Status
  - Not yet ported: the relevant KGB support computations (`x.support`,
    `x.support_dual`) and the cell decomposition utilities (`blocks_and_cells`,
    `parameters(block,cell)`) are not currently available in the SML layer.
*)

structure Support = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun TODO (_: string) : 'a =
    raise Fail "Support: not yet ported"
end

