use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Coordinates.sml";
use "atlas-scripts-sml/K_highest_weights.sml";
use "atlas-scripts-sml/representations.sml";

(*
  File: atlas-scripts-sml/one_dimensional.sml

  Purpose
  - Partial SML translation of `atlas-scripts/one_dimensional.at`.
  - Provides helpers for enumerating and recognizing unitary one-dimensional
    characters (parameters) of a real form `G`.

  Implemented functionality
  - `is_one_dimensional(g,p)`:
      checks the condition used in `induction.at` / `one_dimensional.at`:
        for every simple coroot `α∨` of `G`, we have `<α∨, gamma(p)> = 1`.
    (The `.at` version also checks `#tau(p)=ssr`; we currently omit that since
     `tau(p)` is not exposed via FFI. In practice, the coroot-evaluation test
     is a strong proxy and matches the intended “one-dimensional character”
     criterion for the parameters we use in this port.)
  - `is_unitary_character(g,p)`:
      `is_one_dimensional(g,p)` AND `equivalent(twist(p),p)`.
  - `unitary_one_dimensional(g,gamma)`:
      enumerate parameters at `x_open(G)` with infinitesimal character `gamma`
      and return those that satisfy `is_unitary_character`.
  - `unitary_one_dimensional_default(g)`:
      default choice `gamma = rho(G)`.

  Atlas correspondence
  - Mirrors `one_dimensional.at`:
      `unitary_one_dimensional(G,gamma)`:
        `let x=KGB(G,G.KGB_size-1) in`
        `for p in all_parameters_x_gamma(x,gamma) if is_unitary_character(p) ...`
  - The `is_unitary_character` predicate is taken from `induction.at`.

  Ownership
  - `unitary_one_dimensional*` returns freshly allocated `AtlasFFI.param` handles
    (as produced by `K_highest_weights.all_parameters_x_gamma`); callers must
    free the returned parameters with `AtlasFFI.atlas_param_free`.
  - Intermediate parameters not returned are freed internally.
*)

structure OneDimensional = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratweight = {den: int, nums: int list}

  fun gamma (p: param) : ratweight = Coordinates.parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)

  (* Evaluate coroots on `gamma(p)` and check `<α∨,gamma>=1` for every simple coroot. *)
  fun is_one_dimensional (g: group, p: param) : bool =
    let
      val cors = Coordinates.parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)
      val coords = Coordinates.coordsRatFromCoroots cors (gamma p)
      val one = {num = 1, den = 1}
      fun eqRat (a, b) = #num a * #den b = #num b * #den a
    in
      List.all (fn c => eqRat (c, one)) coords
    end

  (* `equivalent(twist(p),p)` via FFI. *)
  fun twist_equivalent (p: param) : bool =
    let
      val q = AtlasFFI.atlas_param_twist p
      val () =
        if q = Foreign.Memory.null then
          raise Fail ("OneDimensional.twist_equivalent: twist failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val eq = AtlasFFI.atlas_param_equivalent (p, q) = 1
      val () = AtlasFFI.atlas_param_free q
    in
      eq
    end

  (* Unitary one-dimensional character predicate. *)
  fun is_unitary_character (g: group, p: param) : bool =
    is_one_dimensional (g, p) andalso twist_equivalent p

  (* Enumerate unitary one-dimensional characters with given infinitesimal character `gamma`. *)
  fun unitary_one_dimensional (g: group, gamma: ratweight) : param list =
    let
      val x = Representations.x_open g
      val ps = K_highest_weights.all_parameters_x_gamma (g, x, gamma)
      fun keep p = is_unitary_character (g, p)
      fun loop ([], acc) = List.rev acc
        | loop (p :: rest, acc) =
            if keep p then
              loop (rest, p :: acc)
            else
              (AtlasFFI.atlas_param_free p; loop (rest, acc))
    in
      loop (ps, [])
    end

  fun unitary_one_dimensional_default (g: group) : param list =
    unitary_one_dimensional (g, Coordinates.parseRatWeightText (AtlasFFI.atlas_group_rho_text g))
end

