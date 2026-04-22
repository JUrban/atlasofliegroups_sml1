use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure IntMatrix = struct
  type mat = int list list

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("IntMatrix: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun matShape (rows: mat) : int * int =
    case rows of
      [] => (0, 0)
    | r :: rs =>
        let
          val m = length r
          val () = if List.all (fn r2 => length r2 = m) rs then () else raise Fail "IntMatrix: ragged matrix"
        in
          (length rows, m)
        end

  fun toCText (s: string) : string =
    String.translate (fn #"~" => "-" | c => str c) s

  fun transpose (a: mat) : mat =
    let
      val (n, m) = matShape a
      fun col j = List.map (fn row => List.nth (row, j)) a
    in
      List.tabulate (m, col)
    end

  fun neg (a: mat) : mat = List.map (fn row => List.map (fn x => ~x) row) a

  fun add (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op +) (ra, rb)) (a, b)

  fun sub (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op -) (ra, rb)) (a, b)

  fun hcat (a: mat, b: mat) : mat =
    let
      val (na, ma) = matShape a
      val (nb, mb) = matShape b
      val () = if na = nb then () else raise Fail "IntMatrix.hcat: row mismatch"
    in
      ListPair.mapEq (op @) (a, b)
    end

  fun matMul (a: mat, b: mat) : mat =
    let
      val (ar, ac) = matShape a
      val (br, bc) = matShape b
      val () = if ac = br then () else raise Fail "IntMatrix.matMul: dim mismatch"
      fun col j = List.map (fn row => List.nth (row, j)) b
      val cols = List.tabulate (bc, col)
      fun dot (xs, ys) =
        let
          fun loop ([], [], acc) = acc
            | loop (x :: xs', y :: ys', acc) = loop (xs', ys', acc + x * y)
            | loop _ = raise Fail "IntMatrix.dot: mismatch"
        in
          loop (xs, ys, 0)
        end
      fun rowMul r = List.map (fn c => dot (r, c)) cols
    in
      if ar = 0 then [] else List.map rowMul a
    end

  fun firstRows (k: int, a: mat) : mat =
    let
      val (n, _) = matShape a
      val () = if 0 <= k andalso k <= n then () else raise Fail "IntMatrix.firstRows: oob"
    in
      List.take (a, k)
    end

  fun firstCols (k: int, a: mat) : mat =
    let
      val (_, m) = matShape a
      val () = if 0 <= k andalso k <= m then () else raise Fail "IntMatrix.firstCols: oob"
      fun row r = List.take (r, k)
    in
      List.map row a
    end

  fun matToText (a: mat) : string =
    let
      val (n, m) = matShape a
      val entries = List.concat a
      fun intToCText n =
        let
          val s = Int.toString n
        in
          if String.size s > 0 andalso String.sub (s, 0) = #"~" then
            "-" ^ String.extract (s, 1, NONE)
          else
            s
        end
    in
      String.concatWith " " (Int.toString n :: Int.toString m :: List.map intToCText entries)
    end

  fun parseMatText s : mat =
    let
      val ns = parseInts s
    in
      case ns of
        n :: m :: rest =>
          let
            val need = n * m
            val () = if n < 0 orelse m < 0 then raise Fail "IntMatrix: negative dims" else ()
            val () = if length rest <> need then raise Fail "IntMatrix: entry count mismatch" else ()
            fun row i = List.take (List.drop (rest, i * m), m)
          in
            List.tabulate (n, row)
          end
      | _ => raise Fail "IntMatrix: truncated header"
    end

  fun kernel (a: mat) : mat =
    let
      val out = AtlasFFI.atlas_intmat_kernel_text (matToText a)
    in
      case parseInts out of
        [~1] => raise Fail ("IntMatrix.kernel: C++ error: " ^ AtlasFFI.atlas_last_error ())
      | _ => parseMatText out
    end

  fun eigenLattice (a: mat, eigenValue: int) : mat =
    let
      val out = AtlasFFI.atlas_intmat_eigen_lattice_text (matToText a, eigenValue)
    in
      case parseInts out of
        [~1] => raise Fail ("IntMatrix.eigenLattice: C++ error: " ^ AtlasFFI.atlas_last_error ())
      | _ => parseMatText out
    end

  type echelon = AtlasFFI.echelon

  fun echelon (a: mat) : mat * mat * int list * int =
    let
      val h = AtlasFFI.atlas_intmat_echelon (matToText a)
      val () =
        if h = Foreign.Memory.null then
          raise Fail ("IntMatrix.echelon: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val mText = AtlasFFI.atlas_intmat_echelon_M_text h
      val cText = AtlasFFI.atlas_intmat_echelon_C_text h
      val pText = AtlasFFI.atlas_intmat_echelon_pivots_text h
      val eps = AtlasFFI.atlas_intmat_echelon_eps h
      val () = AtlasFFI.atlas_intmat_echelon_free h

      val () =
        if mText = "-1" orelse cText = "-1" orelse pText = "-1" orelse eps = 0 then
          raise Fail ("IntMatrix.echelon: C++ error: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val m = parseMatText mText
      val c = parseMatText cText
      val ps =
        (case parseInts pText of
           k :: rest =>
             if k < 0 orelse length rest <> k then raise Fail "IntMatrix.echelon: bad pivots" else rest
         | _ => raise Fail "IntMatrix.echelon: bad pivots header")
    in
      (m, c, ps, eps)
    end
end
