use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure Lattice = struct
  type vec = int list
  type mat = int list list
  type ratvec = {den: int, nums: int list}

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

  fun dot (xs: vec, ys: vec) : int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) = loop (as', bs', acc + a * b)
        | loop _ = raise Fail "Lattice.dot: length mismatch"
    in
      loop (xs, ys, 0)
    end

  fun matVecMulInt (a: mat) (x: vec) : vec =
    List.map (fn row => dot (row, x)) a

  fun ratvecSub (u: ratvec, v: ratvec) : ratvec =
    let
      val du = #den u
      val dv = #den v
      val numsU = #nums u
      val numsV = #nums v
      val () = if du = 0 orelse dv = 0 then raise Fail "Lattice.ratvecSub: zero denom" else ()
      val () = if length numsU = length numsV then () else raise Fail "Lattice.ratvecSub: length mismatch"
      val den = du * dv
      val nums = ListPair.mapEq (fn (a, b) => a * dv - b * du) (numsU, numsV)
    in
      {den = den, nums = nums}
    end

  fun ratvecScale (u: ratvec, num: int, den: int) : ratvec =
    if den = 0 then raise Fail "Lattice.ratvecScale: zero denom"
    else {den = #den u * den, nums = List.map (fn a => a * num) (#nums u)}

  fun matVecMulRatvec (a: mat) (u: ratvec) : ratvec =
    {den = #den u, nums = matVecMulInt a (#nums u)}

  fun matAdd (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op +) (ra, rb)) (a, b)

  fun identity (n: int) : mat =
    List.tabulate (n, fn i => List.tabulate (n, fn j => if i = j then 1 else 0))

  fun matToText (a: mat) : string =
    let
      val (n, m) = matShape a
      val entries = List.concat a
    in
      String.concatWith " " (Int.toString n :: Int.toString m :: List.map Int.toString entries)
    end

  fun vecToText (xs: vec) : string =
    String.concatWith " " (Int.toString (length xs) :: List.map Int.toString xs)

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("Lattice: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun solve (a: mat, b: vec) : vec option =
    let
      val matText = matToText a
      val vecText = vecToText b
      val out = AtlasFFI.atlas_intmat_find_solution_text (matText, vecText)
      val ns = parseInts out
    in
      case ns of
        [0] => NONE
      | m :: rest =>
          if m < 0 then raise Fail "Lattice.solve: negative size"
          else if length rest <> m then raise Fail "Lattice.solve: truncated output"
          else SOME rest
      | _ => raise Fail "Lattice.solve: unexpected output"
    end

  fun vec_solve (a: mat, u: ratvec) : vec option =
    if #den u <> 1 then
      NONE
    else
      solve (a, #nums u)
end
