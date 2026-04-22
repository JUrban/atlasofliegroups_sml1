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

  fun isPowerOfTwo n =
    n > 0 andalso
    let
      fun loop m =
        m = 1 orelse (m mod 2 = 0 andalso loop (m div 2))
    in
      loop n
    end

  fun pow2 k =
    let
      fun loop (0, acc) = acc
        | loop (n, acc) = loop (n - 1, acc * 2)
    in
      if k < 0 then 0 else loop (k, 1)
    end

  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      fun check x =
        let
          val vsKGB = LambdaDifferential0.all (g, x)
          val theta = parseThetaMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
          val vsTheta = LambdaDifferential0.allTheta theta
          val basis = LambdaDifferential0.basisTheta theta
          val t =
            (case basis of
               [] => 0
             | r :: _ => length r)
          val () =
            if length vsTheta = 1 andalso t = 0 then
              ()
            else if length vsTheta = 1 then
              raise Fail "test_lambda_differential_0_theta: basis has columns but allTheta singleton"
            else if not (isPowerOfTwo (length vsTheta)) then
              raise Fail "test_lambda_differential_0_theta: allTheta count not power of two"
            else if length vsTheta <> pow2 t then
              raise Fail "test_lambda_differential_0_theta: allTheta count doesn't match basis columns"
            else
              ()
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
