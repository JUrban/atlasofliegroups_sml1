use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/IntMatrix.sml

  Purpose
  - Small integer-matrix utility layer used throughout the SML port.
  - Provides basic linear algebra on `int list list` matrices plus thin wrappers
    around Atlas C++ routines (Smith normal form, kernels, echelon forms, etc.)
    exposed via the SML FFI.

  Matrix representation and text formats
  - `type mat = int list list` is row-major: each inner list is a row.
  - `parseMatText` / `matToText` use the Atlas “int matrix text” convention:
      `n m a11 a12 ... a1m a21 ... anm`
    i.e. the first two integers are the number of rows and columns, followed by
    row-major entries.

  Error handling
  - Pure SML routines raise `Fail` on shape mismatches.
  - FFI-backed routines return `"-1"`/`null` on errors; wrappers translate this
    into `Fail` with `AtlasFFI.atlas_last_error ()`.
*)
structure IntMatrix = struct
  type mat = int list list

  (* Parse a whitespace-separated list of integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("IntMatrix: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Return the `(nRows,nCols)` shape; rejects ragged matrices. *)
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

  (* Convert SML `~` negatives to C-style `-` negatives (Atlas parsers). *)
  fun toCText (s: string) : string =
    String.translate (fn #"~" => "-" | c => str c) s

  (* Matrix transpose. *)
  fun transpose (a: mat) : mat =
    let
      val (n, m) = matShape a
      fun col j = List.map (fn row => List.nth (row, j)) a
    in
      List.tabulate (m, col)
    end

  (* Entrywise negation. *)
  fun neg (a: mat) : mat = List.map (fn row => List.map (fn x => ~x) row) a

  (* Entrywise addition (shape must match). *)
  fun add (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op +) (ra, rb)) (a, b)

  (* Entrywise subtraction (shape must match). *)
  fun sub (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op -) (ra, rb)) (a, b)

  (* Horizontal concatenation `[a | b]` (same number of rows). *)
  fun hcat (a: mat, b: mat) : mat =
    let
      val (na, ma) = matShape a
      val (nb, mb) = matShape b
      val () = if na = nb then () else raise Fail "IntMatrix.hcat: row mismatch"
    in
      ListPair.mapEq (op @) (a, b)
    end

  (* Matrix multiplication `a*b` (row-major, integer arithmetic). *)
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

  (* Identity matrix of size `n`. *)
  fun identity (n: int) : mat =
    List.tabulate (n, fn i => List.tabulate (n, fn j => if i = j then 1 else 0))

  (* Multiply matrix by an integer column vector. *)
  fun matVecMul (a: mat, x: int list) : int list =
    let
      val (_, m) = matShape a
      val () = if length x = m then () else raise Fail "IntMatrix.matVecMul: dim mismatch"
      fun dot (xs, ys) = List.foldl (op +) 0 (ListPair.mapEq (op *) (xs, ys))
    in
      List.map (fn row => dot (row, x)) a
    end

  (* First `k` rows of a matrix. *)
  fun firstRows (k: int, a: mat) : mat =
    let
      val (n, _) = matShape a
      val () = if 0 <= k andalso k <= n then () else raise Fail "IntMatrix.firstRows: oob"
    in
      List.take (a, k)
    end

  (* First `k` columns of a matrix. *)
  fun firstCols (k: int, a: mat) : mat =
    let
      val (_, m) = matShape a
      val () = if 0 <= k andalso k <= m then () else raise Fail "IntMatrix.firstCols: oob"
      fun row r = List.take (r, k)
    in
      List.map row a
    end

  (* Serialize to the Atlas “int matrix text” format `n m ...` (row-major). *)
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

  (* Parse the Atlas “int matrix text” format `n m ...` (row-major). *)
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

  (* Integer kernel basis as rows, via Atlas C++ routine. *)
  fun kernel (a: mat) : mat =
    let
      val out = AtlasFFI.atlas_intmat_kernel_text (matToText a)
    in
      case parseInts out of
        [~1] => raise Fail ("IntMatrix.kernel: C++ error: " ^ AtlasFFI.atlas_last_error ())
      | _ => parseMatText out
    end

  (* Cokernel matrix, in the sense of `basic.at`:
       cokernel(M) = transpose(kernel(transpose(M))).
     Here `kernel` returns a matrix whose columns form a Z-basis of the kernel,
     so `kernel(transpose(M))` is an `n x t` matrix (columns are kernel vectors)
     and its transpose is `t x n`, whose rows can be viewed as linear forms
     annihilating the image of `M`. *)
  fun cokernel (a: mat) : mat =
    transpose (kernel (transpose a))

  (* Eigenlattice for eigenvalue `eigenValue` of an integer matrix. *)
  fun eigenLattice (a: mat, eigenValue: int) : mat =
    let
      val out = AtlasFFI.atlas_intmat_eigen_lattice_text (matToText a, eigenValue)
    in
      case parseInts out of
        [~1] => raise Fail ("IntMatrix.eigenLattice: C++ error: " ^ AtlasFFI.atlas_last_error ())
      | _ => parseMatText out
    end

  (* Smith normal form data:
       - returns `(basis, diag)`
       - `basis` is a change-of-basis matrix (Atlas convention)
       - `diag` is the diagonal invariant list (no header). *)
  fun smithBasis (a: mat) : mat * int list =
    let
      val basisText = AtlasFFI.atlas_intmat_smith_basis_text (matToText a)
      val diagText = AtlasFFI.atlas_intmat_smith_diag_text (matToText a)
      val () =
        if basisText = "-1" orelse diagText = "-1" then
          raise Fail ("IntMatrix.smithBasis: C++ error: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val basis = parseMatText basisText
      val ds = parseInts diagText
    in
      case ds of
        k :: rest =>
          if k < 0 orelse length rest <> k then
            raise Fail "IntMatrix.smithBasis: bad diag length"
          else
            (basis, rest)
      | _ => raise Fail "IntMatrix.smithBasis: bad diag header"
    end

  (* Diagonalization wrapper returning `(diag,row,col)`:
       - `diag` is the diagonal list
       - `row` and `col` are unimodular transforms in Atlas conventions. *)
  fun diagonalize (a: mat) : int list * mat * mat =
    let
      val h = AtlasFFI.atlas_intmat_diagonalize (matToText a)
      val () =
        if h = Foreign.Memory.null then
          raise Fail ("IntMatrix.diagonalize: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val dText = AtlasFFI.atlas_intmat_diagonalize_diag_text h
      val rText = AtlasFFI.atlas_intmat_diagonalize_row_text h
      val cText = AtlasFFI.atlas_intmat_diagonalize_col_text h
      val () = AtlasFFI.atlas_intmat_diagonalize_free h
      val () =
        if dText = "-1" orelse rText = "-1" orelse cText = "-1" then
          raise Fail ("IntMatrix.diagonalize: C++ error: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val ds =
        (case parseInts dText of
           k :: rest =>
             if k < 0 orelse length rest <> k then raise Fail "IntMatrix.diagonalize: bad diag length" else rest
         | _ => raise Fail "IntMatrix.diagonalize: bad diag header")
      val row = parseMatText rText
      val col = parseMatText cText
    in
      (ds, row, col)
    end
 
  type echelon = AtlasFFI.echelon
 
  (* Compute an echelon form:
       returns `(M,C,pivots,eps)` following the Atlas C++ helper.
     `eps` is a nonzero “tolerance”/flag value from the C++ side. *)
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
