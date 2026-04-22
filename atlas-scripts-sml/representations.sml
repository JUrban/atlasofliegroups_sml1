use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Dominant.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/FromDominant.sml";
use "atlas-scripts-sml/cross_W_orbit.sml";

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
  - `finite_dimensional` mirrors:
      `finite_dimensional (RealForm G,vec lambda)`
    from `representations.at`:
      `let gamma=dominant(G,lambda)+rho(G) in parameter(x_open(G),gamma,gamma)`
  - `finite_dimensional_fundamental_weight_coordinates` mirrors:
      `finite_dimensional_fundamental_weight_coordinates (RealForm G,vec tau)`
    from `representations.at`: expand `tau` in the fundamental-weight basis and
    call `finite_dimensional`.

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
  type weyl_word = WeylWord.t

  (* Parse `atlas_group_rho_text` output. *)
  fun rho (g: group) : ratvec =
    AllParameters.parseRatWeightText (AtlasFFI.atlas_group_rho_text g)

  fun ratvecNeg (u: ratvec) : ratvec = Lattice.ratvecScale (u, ~1, 1)
  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec = Lattice.ratvecSub (u, ratvecNeg v)

  (* Zero rational vector of length `n`. *)
  fun ratvecZero (n: int) : ratvec = {den = 1, nums = List.tabulate (n, fn _ => 0)}

  (* Construct a parameter `parameter(G,x,lambda,nu)` in `.at`-style where
     `lambda`/`nu` are the usual “ratweight text” inputs (same as `p.lambda` /
     `p.nu` printers). This is the SML-side analogue of the `.at` `parameter`
     primitive (without additional normalization).

     Ownership: returns an owned `AtlasFFI.param` handle; caller must free it. *)
  fun parameter (g: group, x: int, lambda: ratvec, nu: ratvec) : param =
    let
      val p =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          ( g
          , x
          , AllParameters.intsToCText (#nums lambda)
          , #den lambda
          , AllParameters.intsToCText (#nums nu)
          , #den nu
          )
    in
      if p = Foreign.Memory.null then
        raise Fail ("Representations.parameter: parameter construction failed: " ^ AtlasFFI.atlas_last_error ())
      else
        p
    end

  (* Conservative equal-rank test for a real form `g`.

     Atlas meaning
     - In `.at`, `is_equal_rank(G)` is a predicate on the real form.
     - Internally Atlas detects equal-rank via existence of a KGB element whose
       involution matrix is `-I` (a compact Cartan).

     Implementation
     - We scan the KGB set for an element with `theta = -I` using the FFI
       predicate `atlas_group_kgb_involution_is_minus_identity`.
  *)
  fun is_equal_rank (g: group) : bool =
    let
      val n = AtlasFFI.atlas_group_kgb_size g
      fun loop i =
        if i >= n then
          false
        else if AtlasFFI.atlas_group_kgb_involution_is_minus_identity (g, i) = 1 then
          true
        else
          loop (i + 1)
    in
      loop 0
    end

  fun assertSplit (g: group) : unit =
    if AtlasFFI.atlas_group_is_split g = 1 then
      ()
    else
      raise Fail "Representations.minimal_principal_series: group is not split"

  fun assertEqualRank (g: group) : unit =
    if is_equal_rank g then () else raise Fail "Representations.large_discrete_series: group is not equal rank"

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

  (* Large fundamental series for (typically) quasisplit groups.

     Atlas correspondence
     - Mirrors `representations.at`:
         `parameter(G,0,lambda,lambda)`
       (no automatic normalization).

     Notes
     - The `.at` version asserts quasisplit + regularity; we currently do not
       reimplement those checks here.
  *)
  fun large_fundamental_series (g: group, lambda: ratvec) : param =
    parameter (g, 0, lambda, lambda)

  fun large_fundamental_series_default (g: group) : param =
    large_fundamental_series (g, rho g)

  (* Large discrete series (equal rank), mirroring `representations.at`:
       `large_discrete_series(G,lambda) = large_fundamental_series(G,lambda)`
     with an equal-rank assertion. *)
  fun large_discrete_series (g: group, lambda: ratvec) : param =
    (assertEqualRank g; large_fundamental_series (g, lambda))

  fun large_discrete_series_default (g: group) : param =
    large_discrete_series (g, rho g)

  (* Build `x_open(G)` from `basic.at`: `KGB(G,G.KGB_size-1)`. *)
  fun x_open (g: group) : int = AtlasFFI.atlas_group_kgb_size g - 1

  (* Discrete series parameter with given Harish-Chandra parameter `lambda`,
     with respect to a chosen KGB element `x`.

     Atlas correspondence
     - Mirrors `representations.at`:
         `discrete_series (KGBElt x,ratvec lambda)`
       which (after dominance normalization) returns:
         `parameter(cross(inverse(w),x),lambda_dom,null(rank(ic)))`
       where `(w,lambda_dom) = from_dominant(rd,lambda)` and `w*lambda_dom=lambda`.

     Notes
     - We currently perform only the minimal equal-rank sanity check; the `.at`
       assertions about integrality/regularity are not reimplemented here.
     - The returned parameter is *not* automatically normalized (to match `.at`).

     Ownership: returns an owned `AtlasFFI.param` handle; caller must free it. *)
  fun discrete_series_at_x (g: group, x: int, lambda: ratvec) : param =
    let
      val () = assertEqualRank g
      val rank = AtlasFFI.atlas_group_rank g
      val (witness, lambdaDom) = FromDominant.fromDominantRatvec (g, lambda)
      val invWitness : weyl_word = WeylWord.inverse witness
      val x2 = WeylWord.kgbCrossLeft (g, invWitness, x)
      val nu0 = ratvecZero rank
    in
      parameter (g, x2, lambdaDom, nu0)
    end

  (* Discrete series parameter with given Harish-Chandra parameter `lambda`,
     with respect to `KGB(G,0)` (mirrors the `.at` overload). *)
  fun discrete_series (g: group, lambda: ratvec) : param =
    discrete_series_at_x (g, 0, lambda)

  (* Harish-Chandra parameter of (relative) discrete series, with respect to
     a chosen base KGB element `x_b`.

     Atlas correspondence
     - Mirrors `representations.at`:
         `hc_parameter(Param p, KGBElt x_b) = let w=cross_divide(x_b,x(p)) in w*lambda(p)`

     Notes
     - This is meaningful primarily for equal-rank discrete series parameters
       with compact Cartan; we do not currently reimplement the `.at` assertions.
  *)
  fun hc_parameter_at_xb (g: group, p: param, x_b: int) : ratvec =
    let
      val x_p = AtlasFFI.atlas_param_x p
      val w = CrossWOrbit.cross_divide (g, x_b, x_p)
      val lam = Lattice.ratvecNormalize (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_lambda_text p))
    in
      WeylWord.actRatvec (g, w, lam)
    end

  (* Harish-Chandra parameter with `x_b = KGB(G,0)` (mirrors the `.at` overload). *)
  fun hc_parameter (g: group, p: param) : ratvec =
    hc_parameter_at_xb (g, p, 0)

  (* Make a rational weight dominant for `g`. *)
  fun dominant (g: group) (v: ratvec) : ratvec =
    AllParameters.parseRatWeightText
      (Dominant.makeDominantText g (Int.toString (#den v) ^ " " ^ String.concatWith " " (List.map Int.toString (#nums v))))

  (* Finite dimensional representation with highest weight `lambda`.
     Here `lambda` should be integral (as in `representations.at`), but we accept
     it as a `ratvec` for convenience. *)
  fun finite_dimensional (g: group, lambda: ratvec) : param =
    let
      val gamma = ratvecAdd (dominant g lambda, rho g)
    in
      parameter (g, x_open g, gamma, gamma)
    end

  (* Finite-dimensional constructor where `tau` is given in fundamental-weight
     coordinates (an integral vector of coefficients).

     Raises if `tau` does not define an integral weight in the group’s weight
     lattice (mirrors the `.at` assertion). *)
  fun finite_dimensional_fundamental_weight_coordinates (g: group, tau: int list) : param =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          raise Fail ("Representations.finite_dimensional_fundamental_weight_coordinates: rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val fws = RootDatum.fundamentalWeights rd
      val () = RootDatum.free rd
      val () =
        if length tau = length fws then
          ()
        else
          raise Fail "Representations.finite_dimensional_fundamental_weight_coordinates: length mismatch"

      fun scale (w: ratvec, k: int) : ratvec = Lattice.ratvecScale (w, k, 1)
      val ws = ListPair.mapEq (fn (k, w) => scale (w, k)) (tau, fws)
      val sum = List.foldl ratvecAdd (Lattice.ratvecNormalize {den = 1, nums = List.tabulate (length tau, fn _ => 0)}) ws
      val () =
        (case Lattice.ratvecToIntegral sum of
           SOME _ => ()
         | NONE => raise Fail "Representations.finite_dimensional_fundamental_weight_coordinates: weight not integral")
    in
      finite_dimensional (g, sum)
    end
end
