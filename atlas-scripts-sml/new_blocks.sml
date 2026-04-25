use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamBlocks.sml";

(*
  File: atlas-scripts-sml/new_blocks.sml

  Purpose
  - SML-facing analogue of `atlas-scripts/new_blocks.at`.
  - The `.at` script “replaces” older block/cross/Cayley utilities with
    versions that do not require transforming parameters to dominant
    infinitesimal character.

  SML situation
  - We do not need the `.at`-side `forget`/redefinition machinery.
  - The SML code already calls the Atlas C++ primitives directly via the FFI:
      - `atlas_param_cross`, `atlas_param_cayley`
      - `atlas_param_block_survivors`
  - This file provides small convenience wrappers with names similar to the
    `.at` functions, to support “line-by-line” translation of scripts.

  Ownership
  - `block` returns freshly-cloned `Param` handles; caller must free them with
    `free_block` (or free each with `AtlasFFI.atlas_param_free`).
  - `cross` / `Cayley` allocate fresh parameters; caller must free them.
*)

structure New_blocks = struct
  type param = AtlasFFI.param

  fun failFFI (where': string) : 'a =
    raise Fail ("New_blocks." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun expect (where': string, b: bool, msg: string) : unit =
    if b then () else raise Fail ("New_blocks." ^ where' ^ ": " ^ msg)

  fun cross (s: int, p: param) : param =
    let
      val q = AtlasFFI.atlas_param_cross (p, s)
    in
      if q = Foreign.Memory.null then failFFI "cross" else q
    end

  fun Cayley (s: int, p: param) : param =
    let
      val q = AtlasFFI.atlas_param_cayley (p, s)
    in
      if q = Foreign.Memory.null then failFFI "Cayley" else q
    end

  (* `block(p)` returns (survivors, start_pos) as in `.at`. *)
  fun block (p: param) : param list * int =
    let
      val (terms, startPos) = ParamBlocks.block_survivors p
      val () = expect ("block", List.all (fn (_, mult) => mult = 1) terms, "unexpected multiplicities in block survivors")
    in
      (List.map #1 terms, startPos)
    end

  fun block_of (p: param) : param list = #1 (block p)

  fun free_block (B: param list) : unit = List.app AtlasFFI.atlas_param_free B
end

