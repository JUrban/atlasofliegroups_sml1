use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/MatReduc.sml";

structure LambdaDifferential0 = struct
  type mat = IntMatrix.mat

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("LambdaDifferential0: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun all (g: AtlasFFI.group, x: int) : int list list =
    let
      val s = AtlasFFI.atlas_kgb_all_lambda_differential_0_text (g, x)
      val ns = parseInts s
    in
      case ns of
        count :: rank :: rest =>
          let
            fun takeVec (0, xs, acc) = (List.rev acc, xs)
              | takeVec (n, x :: xs, acc) = takeVec (n - 1, xs, x :: acc)
              | takeVec _ = raise Fail "LambdaDifferential0: truncated vectors"

            fun loop (0, xs, acc) = (List.rev acc, xs)
              | loop (k, xs, acc) =
                  let
                    val (v, xs') = takeVec (rank, xs, [])
                  in
                    loop (k - 1, xs', v :: acc)
                  end

            val (vs, leftover) = loop (count, rest, [])
          in
            if null leftover then vs else raise Fail "LambdaDifferential0: extra ints"
          end
      | _ => raise Fail "LambdaDifferential0: bad header"
    end

  fun matShape (rows: mat) : int * int = IntMatrix.matShape rows

  fun matSub (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op -) (ra, rb)) (a, b)

  fun identity (n: int) : mat =
    List.tabulate (n, fn i => List.tabulate (n, fn j => if i = j then 1 else 0))

  fun matMul (a: mat, b: mat) : mat =
    let
      val (ar, ac) = matShape a
      val (br, bc) = matShape b
      val () = if ac = br then () else raise Fail "LambdaDifferential0.matMul: dim mismatch"
      fun col j = List.map (fn row => List.nth (row, j)) b
      val cols = List.tabulate (bc, col)
      fun dot (xs, ys) =
        let
          fun loop ([], [], acc) = acc
            | loop (x :: xs', y :: ys', acc) = loop (xs', ys', acc + x * y)
            | loop _ = raise Fail "LambdaDifferential0.dot: mismatch"
        in
          loop (xs, ys, 0)
        end
      fun rowMul r = List.map (fn c => dot (r, c)) cols
    in
      if ar = 0 then [] else List.map rowMul a
    end

  fun selectColumns (cols: int list, m: mat) : mat =
    let
      val (_, c) = matShape m
      val () =
        if List.all (fn j => 0 <= j andalso j < c) cols then () else raise Fail "LambdaDifferential0.selectColumns: oob"
      fun row r = List.map (fn j => List.nth (r, j)) cols
    in
      List.map row m
    end

  fun allTheta (theta: mat) : int list list =
    let
      val (n, n2) = matShape theta
      val () = if n = n2 then () else raise Fail "LambdaDifferential0.allTheta: expected square theta"

      val e = IntMatrix.eigenLattice (theta, ~1) (* n x k *)
      val (_, k) = matShape e
      val zero = List.tabulate (n, fn _ => 0)
    in
      if k = 0 then
        [zero]
      else
        let
          val oneMinusTheta = matSub (identity n, theta) (* n x n *)
          val c = MatReduc.inLatticeBasis (e, oneMinusTheta) (* k x n *)
          val (a, diag) = MatReduc.adaptedBasis c (* k x k, diag length k *)

          val colsWith2 =
            let
              fun loop ([], _, acc) = List.rev acc
                | loop (d :: ds, j, acc) =
                    if d = 2 then loop (ds, j + 1, j :: acc) else loop (ds, j + 1, acc)
            in
              loop (diag, 0, [])
            end

          val a2 = selectColumns (colsWith2, a) (* k x t *)
          val basis = matMul (e, a2) (* n x t *)
          val (_, t) = matShape basis

          fun vecAdd (xs, ys) = ListPair.mapEq (op +) (xs, ys)
          fun col j = List.map (fn row => List.nth (row, j)) basis
          val basisCols = List.tabulate (t, col)

          fun enum [] acc = acc
            | enum (bcol :: rest) acc =
                let
                  val withCol = List.map (fn v => vecAdd (v, bcol)) acc
                in
                  enum rest (acc @ withCol)
                end
        in
          enum basisCols [zero]
        end
    end
end
