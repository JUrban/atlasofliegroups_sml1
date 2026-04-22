use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/ParamReduce.sml";
use "atlas-scripts-sml/Rat.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/LowestKTypes.sml";

structure K_highest_weights = struct
  type mat = IntMatrix.mat
  type ratweight = {den: int, nums: int list}
  type rat = Rat.rat
  type param = AtlasFFI.param

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("K_highest_weights: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  fun intsToCText xs =
    String.concatWith " " (List.map intToCText xs)

  fun parseRatWeightText s : ratweight =
    (case parseInts s of
       den :: rest => {den = den, nums = rest}
     | _ => raise Fail ("K_highest_weights: bad ratweight text: " ^ s))

  fun parseVecTextWithRankHeader s : int list =
    (case parseInts s of
       n :: rest =>
         if length rest <> n then
           raise Fail "K_highest_weights: parseVecTextWithRankHeader: bad length"
         else
           rest
     | _ => raise Fail "K_highest_weights: parseVecTextWithRankHeader: empty")

  fun ratweightAddVec (w: ratweight, v: int list) : ratweight =
    let
      val den = #den w
      val nums = #nums w
      val () = if length nums = length v then () else raise Fail "K_highest_weights: ratweightAddVec: length mismatch"
    in
      {den = den, nums = ListPair.mapEq (fn (a, b) => a + b * den) (nums, v)}
    end

  fun involutionMatrix (g: AtlasFFI.group, x: int) : mat =
    let
      val ns = parseInts (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
    in
      case ns of
        rank :: rest =>
          let
            val need = rank * rank
            val () =
              if length rest <> need then
                raise Fail "K_highest_weights: involutionMatrix: bad size"
              else
                ()
            fun row i =
              List.take (List.drop (rest, i * rank), rank)
          in
            List.tabulate (rank, row)
          end
      | _ => raise Fail "K_highest_weights: involutionMatrix: empty"
    end

  fun parseSimpleCorootsText s : int list list =
    let
      val ns = parseInts s
    in
      case ns of
        ssRank :: rank :: rest =>
          let
            val need = ssRank * rank
            val () =
              if ssRank < 0 orelse rank < 0 orelse length rest <> need then
                raise Fail "K_highest_weights: parseSimpleCorootsText: bad size"
              else
                ()
            fun coroot i = List.take (List.drop (rest, i * rank), rank)
          in
            List.tabulate (ssRank, coroot)
          end
      | _ => raise Fail "K_highest_weights: parseSimpleCorootsText: truncated header"
    end

  fun dot (xs: int list, ys: int list) : int =
    List.foldl (op +) 0 (ListPair.mapEq (op *) (xs, ys))

  (* Port of `is_split_spherical` from `atlas-scripts/K_highest_weights.at`,
     implemented for split real forms (uses `RealReductiveGroup::isSplit`). *)
  fun is_split_spherical (g: AtlasFFI.group, t: KType.ktype) : bool =
    let
      val () = if KType.isFinal t then () else raise Fail "K_highest_weights.is_split_spherical: K-type not final"
      val split = (AtlasFFI.atlas_group_is_split g = 1)
      val xOpen = AtlasFFI.atlas_group_kgb_size g - 1
      val xOk = (KType.x t = xOpen)
      val coroots = parseSimpleCorootsText (AtlasFFI.atlas_group_simple_coroots_text g)
      val lamRho = parseVecTextWithRankHeader (KType.lambdaRhoText t)
      val parityOk = List.all (fn a => dot (a, lamRho) mod 2 = 0) coroots
    in
      split andalso xOk andalso parityOk
    end

  fun parameter (t: KType.ktype) : param =
    KType.parameter t

  fun infinitesimal_character (p: param) : ratweight =
    parseRatWeightText (AtlasFFI.atlas_param_gamma_text p)

  fun is_split_spherical_param (g: AtlasFFI.group, p: param) : bool =
    let
      val t = LowestKTypes.LKT_param (g, p)
      val ok = is_split_spherical (g, t)
      val () = KType.free t
    in
      ok
    end

  fun basis_lambda_differential_0_theta (theta: mat) : mat =
    LambdaDifferential0.basisTheta theta

  fun basis_lambda_differential_0 (g: AtlasFFI.group, x: int) : mat =
    basis_lambda_differential_0_theta (involutionMatrix (g, x))

  fun characters_order_2_theta (theta: mat) : int list list =
    LambdaDifferential0.charactersOrder2Theta theta

  fun characters_order_2 (g: AtlasFFI.group, x: int) : int list list =
    characters_order_2_theta (involutionMatrix (g, x))

  fun all_lambda_differential_0_theta (theta: mat) : int list list =
    LambdaDifferential0.allTheta theta

  fun all_lambda_differential_0 (g: AtlasFFI.group, x: int) : int list list =
    all_lambda_differential_0_theta (involutionMatrix (g, x))

  (* Port of `all_parameters_x_gamma` from `atlas-scripts/K_highest_weights.at`.
     `.at` version makes gamma dominant; here we do the same. *)
  fun all_parameters_x_gamma (g: AtlasFFI.group, x: int, gamma: ratweight) : AtlasFFI.param list =
    AllParameters.all_parameters_x_gamma_dominant (g, x, gamma)

  fun all_parameters_gamma (g: AtlasFFI.group, gamma: ratweight) : AtlasFFI.param list =
    AllParameters.all_parameters_gamma_dominant (g, gamma)

  (* Port of `all_parameters(p)` from `atlas-scripts/K_highest_weights.at`:
     all parameters with same d_lambda as p and same nu, at fixed x. *)
  fun all_parameters (g: AtlasFFI.group, p: AtlasFFI.param) : AtlasFFI.param list =
    let
      val x = AtlasFFI.atlas_param_x p
      val lambda = parseRatWeightText (AtlasFFI.atlas_param_lambda_text p)
      val nu = parseRatWeightText (AtlasFFI.atlas_param_nu_text p)
      val twists = all_lambda_differential_0 (g, x)

      fun mk v =
        let
          val lam2 = ratweightAddVec (lambda, v)
          val p0 =
            AtlasFFI.atlas_param_new_from_lambda_nu_text
              (g, x, intsToCText (#nums lam2), #den lam2, intsToCText (#nums nu), #den nu)
          val () =
            if p0 = Foreign.Memory.null then
              raise Fail ("K_highest_weights.all_parameters: parameter failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
          val p1 = AtlasFFI.atlas_param_normalise p0
          val () = AtlasFFI.atlas_param_free p0
          val () =
            if p1 = Foreign.Memory.null then
              raise Fail ("K_highest_weights.all_parameters: normalise failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
        in
          p1
        end
    in
      List.map mk twists
    end

  (* Port of `reduce([Param])` from `atlas-scripts/K_highest_weights.at`.
     Returned params are freshly allocated and must be freed by caller. *)
  fun reduce_parameters (ps: AtlasFFI.param list) : AtlasFFI.param list =
    ParamReduce.reduce ps

  (* Port of `all_equal_dlambda_K_parameters(t)` from `atlas-scripts/K_highest_weights.at`.
     Returned K_types are freshly allocated and must be freed by caller. *)
  fun all_equal_dlambda_K_parameters (g: AtlasFFI.group, t: KType.ktype) : KType.ktype list =
    let
      val x = KType.x t
      val base = parseVecTextWithRankHeader (KType.lambdaRhoText t)
      val twists = all_lambda_differential_0 (g, x)

      fun mk v =
        let
          val w = ListPair.mapEq (op +) (base, v)
          val t0 = KType.newFromXAndLambdaRhoText (g, x, intsToCText w)
        in
          if KType.isFinal t0 then SOME t0 else (KType.free t0; NONE)
        end
    in
      List.mapPartial mk twists
    end

  (* Port of `reduce([KType])` from `atlas-scripts/K_highest_weights.at`.
     Returned K_types are freshly allocated and must be freed by caller. *)
  fun reduce_K_parameters (kts: KType.ktype list) : KType.ktype list =
    let
      val ps = List.map KType.parameter kts
      val rs = ParamReduce.reduce ps
      val () = List.app AtlasFFI.atlas_param_free ps
      val ts = List.map KType.ofParam rs
      val () = List.app AtlasFFI.atlas_param_free rs
    in
      ts
    end

  (* Port of `LKTs` / `LKT` / `final` from `atlas-scripts/K_highest_weights.at`.
     Interpretation: use `full_deform(param(t))` and take the lowest-height terms. *)
  fun LKTs (g: AtlasFFI.group, t: KType.ktype) : KType.ktype list =
    LowestKTypes.LKTs_ktype (g, t)

  fun LKTs_param (g: AtlasFFI.group, p: AtlasFFI.param) : KType.ktype list =
    LowestKTypes.LKTs_param (g, p)

  fun LKT (g: AtlasFFI.group, t: KType.ktype) : KType.ktype =
    LowestKTypes.LKT_ktype (g, t)

  fun LKT_param (g: AtlasFFI.group, p: AtlasFFI.param) : KType.ktype =
    LowestKTypes.LKT_param (g, p)

  fun final (g: AtlasFFI.group, t: KType.ktype) : KType.ktype =
    LKT (g, t)

  (* Port of `all_G_spherical_same_differential` from `atlas-scripts/K_highest_weights.at`. *)
  fun all_G_spherical_same_differential (g: AtlasFFI.group, mu: KType.ktype) : KType.ktype list =
    let
      val p = parameter mu
      val x = AtlasFFI.atlas_param_x p
      val gamma = infinitesimal_character p
      val () = AtlasFFI.atlas_param_free p

      val qs = all_parameters_x_gamma (g, x, gamma)

      fun one (q: param) =
        if is_split_spherical_param (g, q) then
          let
            val t = LKT_param (g, q)
            val () = AtlasFFI.atlas_param_free q
          in
            SOME t
          end
        else
          (AtlasFFI.atlas_param_free q; NONE)

      val ts = List.mapPartial one qs
    in
      reduce_K_parameters ts
    end

  (* Port of `cone(limit,cs)` from `atlas-scripts/K_highest_weights.at`.
     Returns an `n x m` matrix (row-major) whose columns are the weight vectors. *)
  fun cone (limit: rat, cs: rat list) : mat =
    let
      val limit = Rat.normalize limit
      val cs = List.map Rat.normalize cs
      val n = length cs

      fun cn (lim: rat, csRemaining: rat list) : int list list =
        (case csRemaining of
           [] => [[]]
         | c :: rest =>
             let
               val () =
                 if #num c <= 0 then
                   raise Fail "K_highest_weights.cone: non-positive coefficient"
                 else
                   ()
               val maxFirst = Rat.divFloor (lim, c)
               fun oneFirst first =
                 let
                   val lim2 = Rat.sub (lim, Rat.mulInt (c, first))
                   val () = if Rat.isNonNeg lim2 then () else raise Fail "K_highest_weights.cone: negative remainder"
                 in
                   List.map (fn tail => first :: tail) (cn (lim2, rest))
                 end
             in
               List.concat (List.tabulate (maxFirst + 1, oneFirst))
             end)

      val cols = cn (limit, cs)
      val () =
        if List.all (fn col => length col = n) cols then
          ()
        else
          raise Fail "K_highest_weights.cone: internal length mismatch"

      fun row i = List.map (fn col => List.nth (col, i)) cols
    in
      List.tabulate (n, row)
    end
end
