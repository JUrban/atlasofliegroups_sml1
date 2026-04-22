use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/Lattice.sml

  Purpose
  - Basic integer lattice and rational-vector operations used throughout the
    port (matrix/vector arithmetic, rational normalization, and solving integer
    linear systems via Atlas C++ helpers).

  Types
  - `vec` / `mat` are integer vectors/matrices (row-major for matrices).
  - `ratvec` is a rational vector stored as `{den, nums}` meaning `nums/den`.

  Text format helpers
  - `matToText` matches the Atlas “int matrix text” format `n m ...` (row-major).
  - `vecToText` matches the Atlas “int vector text” format `k x1 ... xk`.
*)
structure Lattice = struct
  type vec = int list
  type mat = int list list
  type ratvec = {den: int, nums: int list}

  (* Integer gcd (nonnegative result). *)
  fun gcd (a: int, b: int) : int =
    let
      val a = Int.abs a
      val b = Int.abs b
      fun loop (x, 0) = x
        | loop (x, y) = loop (y, x mod y)
    in
      if a = 0 then b else loop (a, b)
    end

  (* GCD of a list (0 for empty list). *)
  fun gcdList xs =
    case xs of
      [] => 0
    | x :: rest => List.foldl gcd (Int.abs x) rest

  (* Normalize a rational vector: reduce by gcd and force positive denominator. *)
  fun ratvecNormalize (u: ratvec) : ratvec =
    let
      val den0 = #den u
      val nums0 = #nums u
      val () = if den0 = 0 then raise Fail "Lattice.ratvecNormalize: zero denom" else ()
      val sign = if den0 < 0 then ~1 else 1
      val den1 = den0 * sign
      val nums1 = List.map (fn a => a * sign) nums0
      val g = gcd (den1, gcdList nums1)
    in
      if g <= 1 then {den = den1, nums = nums1} else {den = den1 div g, nums = List.map (fn a => a div g) nums1}
    end

  (* If all coordinates are integral, return the integral vector (after normalization). *)
  fun ratvecToIntegral (u: ratvec) : vec option =
    let
      val u = ratvecNormalize u
      val den = #den u
      val nums = #nums u
    in
      if den = 1 then SOME nums
      else if List.all (fn a => a mod den = 0) nums then SOME (List.map (fn a => a div den) nums)
      else NONE
    end

  (* Return `(nRows,nCols)` shape; rejects ragged matrices. *)
  fun matShape (rows: mat) : int * int =
    case rows of
      [] => (0, 0)
    | r :: rs =>
        let
          val m = length r
          val () = if List.all (fn r2 => length r2 = m) rs then () else raise Fail "Lattice: ragged matrix"
        in
          (length rows, m)
        end

  (* Dot product of integer vectors. *)
  fun dot (xs: vec, ys: vec) : int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "Lattice.dot: length mismatch"
    in
      loop (xs, ys, 0)
    end

  (* Multiply an integer matrix by an integer column vector. *)
  fun matVecMulInt (a: mat) (x: vec) : vec =
    List.map (fn row => dot (row, x)) a

  (* Subtract rational vectors `u - v`. *)
  fun ratvecSub (u: ratvec, v: ratvec) : ratvec =
    let
      val du = #den u
      val dv = #den v
      val numsU = #nums u
      val numsV = #nums v
      val () = if du = 0 orelse dv = 0 then raise Fail "Lattice.ratvecSub: zero denom" else ()
      val () = if length numsU = length numsV then () else raise Fail "Lattice.ratvecSub: length mismatch"
      val g = gcd (du, dv)
      val aMul = dv div g
      val bMul = du div g
      val den = du * aMul
      val nums = ListPair.mapEq (fn (a, b) => a * aMul - b * bMul) (numsU, numsV)
    in
      ratvecNormalize {den = den, nums = nums}
    end

  (* Scale a rational vector by `num/den`. *)
  fun ratvecScale (u: ratvec, num: int, den: int) : ratvec =
    if den = 0 then raise Fail "Lattice.ratvecScale: zero denom"
    else ratvecNormalize {den = #den u * den, nums = List.map (fn a => a * num) (#nums u)}

  (* Multiply an integer matrix by a rational vector. *)
  fun matVecMulRatvec (a: mat) (u: ratvec) : ratvec =
    ratvecNormalize {den = #den u, nums = matVecMulInt a (#nums u)}

  (* Entrywise addition of integer matrices (shape must match). *)
  fun matAdd (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op +) (ra, rb)) (a, b)

  (* Identity matrix of size `n`. *)
  fun identity (n: int) : mat =
    List.tabulate (n, fn i => List.tabulate (n, fn j => if i = j then 1 else 0))

  (* Serialize a matrix to Atlas “int matrix text” format. *)
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

  (* Serialize an integer vector to Atlas “int vector text” format. *)
  fun vecToText (xs: vec) : string =
    let
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
      String.concatWith " " (Int.toString (length xs) :: List.map intToCText xs)
    end

  (* Parse whitespace-separated integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("Lattice: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Solve `a*x = b` over the integers using an Atlas C++ helper.
     Returns `SOME x` if a solution is found, otherwise `NONE`. *)
  fun solve (a: mat, b: vec) : vec option =
    let
      val matText = matToText a
      val vecText = vecToText b
      val out = AtlasFFI.atlas_intmat_find_solution_text (matText, vecText)
      val ns = parseInts out
    in
      case ns of
        [0] => NONE
      | [~1] => raise Fail ("Lattice.solve: C++ error: " ^ AtlasFFI.atlas_last_error ())
      | m :: rest =>
          if m < 0 then raise Fail "Lattice.solve: negative size"
          else if length rest <> m then raise Fail "Lattice.solve: truncated output"
          else SOME rest
      | _ => raise Fail "Lattice.solve: unexpected output"
    end

  (* Solve `a*x = u` when the right-hand side is a rational vector; succeeds only
     when `u` is integral (after normalization). *)
  fun vec_solve (a: mat, u: ratvec) : vec option =
    case ratvecToIntegral u of
      NONE => NONE
    | SOME b => solve (a, b)
end
