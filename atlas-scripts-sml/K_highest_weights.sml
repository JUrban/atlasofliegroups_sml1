use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";

structure K_highest_weights = struct
  type mat = IntMatrix.mat

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("K_highest_weights: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
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
end

