use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";

structure TestLambdaDifferential0Theta = struct
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("test_lambda_differential_0_theta: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun parseThetaMatrixText s : IntMatrix.mat =
    let
      val ns = parseInts s
    in
      case ns of
        rank :: rest =>
          let
            val need = rank * rank
            val () =
              if length rest <> need then
                raise Fail "test_lambda_differential_0_theta: bad size"
              else
                ()
            fun row i =
              List.take (List.drop (rest, i * rank), rank)
          in
            List.tabulate (rank, row)
          end
      | _ => raise Fail "test_lambda_differential_0_theta: empty"
    end

  fun sameSet (xs: int list list, ys: int list list) : bool =
    length xs = length ys
    andalso List.all (fn x => List.exists (fn y => x = y) ys) xs

  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      fun check x =
        let
          val vsKGB = LambdaDifferential0.all (g, x)
          val theta = parseThetaMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
          val vsTheta = LambdaDifferential0.allTheta theta
        in
          if sameSet (vsKGB, vsTheta) then () else raise Fail ("test_lambda_differential_0_theta: mismatch at x=" ^ Int.toString x)
        end
      val () = check 3
      val () = check 4
      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestLambdaDifferential0Theta.run ();

