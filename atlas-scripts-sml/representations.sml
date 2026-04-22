use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";

(*
  File: atlas-scripts-sml/representations.sml

  Purpose
  - Partial SML analogue of `atlas-scripts/representations.at` providing a small
    set of “common representation constructors” used by downstream scripts.
  - This file intentionally focuses on *parameter construction* for split
    groups (minimal principal series) because it is a common entry point in the
    Atlas script ecosystem (e.g. `test_unitarity.at`).

  Atlas correspondence
  - `minimal_principal_series` mirrors:
      `minimal_principal_series (RealForm G,ratvec lambda,ratvec nu)`
    from `representations.at`:
      `normal(parameter(KGB(G,KGB_size(G)-1),lambda,nu))`
  - `minimal_spherical_principal_series` mirrors:
      `minimal_spherical_principal_series (RealForm G,ratvec nu)`
    i.e. `minimal_principal_series(G,rho(G),nu)`.

  Ownership
  - Functions here allocate `AtlasFFI.param` handles and return them to the
    caller; callers must free them with `AtlasFFI.atlas_param_free`.

  Notes / limitations
  - Atlas `.at` uses higher-level objects like `RealForm` and `KGB(G,n)`. Here
    we work directly with `AtlasFFI.group` and integer KGB indices.
  - This is not (yet) a full port of `representations.at`; add constructors
    incrementally as translated scripts require them.
*)
structure Representations = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec

  (* Parse `atlas_group_rho_text` output. *)
  fun rho (g: group) : ratvec =
    AllParameters.parseRatWeightText (AtlasFFI.atlas_group_rho_text g)

  fun assertSplit (g: group) : unit =
    if AtlasFFI.atlas_group_is_split g = 1 then
      ()
    else
      raise Fail "Representations.minimal_principal_series: group is not split"

  (* Minimal principal series of a split group with given `lambda` and `nu`.
     The returned parameter is normalized (`atlas_param_normalise`).

     Ownership: returns an owned `AtlasFFI.param` handle; caller must free it. *)
  fun minimal_principal_series (g: group, lambda: ratvec, nu: ratvec) : param =
    let
      val () = assertSplit g
      val n = AtlasFFI.atlas_group_kgb_size g
      val x = n - 1
      val p0 =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          ( g
          , x
          , AllParameters.intsToCText (#nums lambda)
          , #den lambda
          , AllParameters.intsToCText (#nums nu)
          , #den nu
          )
      val () =
        if p0 = Foreign.Memory.null then
          raise Fail ("Representations.minimal_principal_series: parameter construction failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val p1 = AtlasFFI.atlas_param_normalise p0
      val () = AtlasFFI.atlas_param_free p0
      val () =
        if p1 = Foreign.Memory.null then
          raise Fail ("Representations.minimal_principal_series: normalise failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      p1
    end

  fun minimal_principal_series_default (g: group) : param =
    let
      val r = rho g
    in
      minimal_principal_series (g, r, r)
    end

  (* Minimal spherical principal series: `lambda = rho(G)`. *)
  fun minimal_spherical_principal_series (g: group, nu: ratvec) : param =
    minimal_principal_series (g, rho g, nu)

  fun minimal_spherical_principal_series_default (g: group) : param =
    minimal_spherical_principal_series (g, rho g)
end

