use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";
use "atlas-scripts-sml/AllParameters.sml";

structure K_highest_weights = struct
  type mat = IntMatrix.mat
  type ratweight = {den: int, nums: int list}

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
end
