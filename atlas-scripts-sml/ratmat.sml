use "atlas-scripts-sml/BigRat.sml";
use "atlas-scripts-sml/Gaussian_elim.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/ratmat.sml

  Purpose
  - SML translation of `atlas-scripts/ratmat.at`.
  - Provides a rational-matrix layer for doing linear algebra over Q in a way
    that is robust against overflow (uses `IntInf.int` via `BigRat`).

  Representation
  - The `.at` script defines `ratmat` as `(mat,"/",int)` where the integer
    matrix stores numerators in a common denominator.
  - In this SML port we use a safer representation aligned with
    `Gaussian_elim.sml`:
      - a matrix is `BigRat.t list list` in column-major form
        (`type ratmat = ratvec list`, each `ratvec` is a column).

  Why this representation
  - It makes it easy to reuse the already-ported Gauss–Jordan elimination
    routines (`Gaussian_elim.inverse`, `Gaussian_elim.full_solve`) and keeps
    arithmetic exact.

  Scope
  - This is not yet a full port of every convenience overload in `ratmat.at`.
    It includes the core operations needed by `coordinates.at`:
      - conversion from integral matrices / rational vectors
      - transpose, multiplication, inverse
      - left/right inverses in the full-column/full-row-rank sense
*)

structure RatMat = struct
  type rat = BigRat.t
  type ratvec = rat list
  type ratmat = ratvec list (* column-major *)
  type intmat = IntMatrix.mat
  type ratvec_int = Lattice.ratvec

  fun rat0 () = BigRat.zero ()
  fun rat1 () = BigRat.one ()

  fun fromInt (n: int) : rat = BigRat.make (IntInf.fromInt n, 1)

  fun fromRatvec (v: ratvec_int) : ratvec =
    let
      val den = IntInf.fromInt (#den v)
      val () = if den = 0 then raise Fail "RatMat.fromRatvec: zero denom" else ()
    in
      List.map (fn a => BigRat.make (IntInf.fromInt a, den)) (#nums v)
    end

  fun toRatvec (v: ratvec) : ratvec_int option =
    let
      fun asIntRat q =
        let
          val q = BigRat.normalize q
        in
          if #den q = 1 then SOME (IntInf.toInt (#num q) handle _ => raise Fail "RatMat.toRatvec: overflow")
          else NONE
        end
    in
      case List.mapPartial asIntRat v of
        xs =>
          if length xs = length v then SOME {den = 1, nums = xs} else NONE
    end

  fun shape (m: ratmat) : int * int =
    (case m of
       [] => (0, 0)
     | col0 :: cols =>
         let
           val nRows = length col0
           val () = if List.all (fn c => length c = nRows) cols then () else raise Fail "RatMat.shape: ragged"
         in
           (nRows, length m)
         end)

  fun transpose (m: ratmat) : ratmat =
    let
      val (nRows, nCols) = shape m
      fun col i = List.tabulate (nCols, fn j => List.nth (List.nth (m, j), i))
    in
      List.tabulate (nRows, col)
    end

  fun mul (a: ratmat, b: ratmat) : ratmat =
    Gaussian_elim.timesMatMat (a, b)

  fun mulVec (a: ratmat, v: ratvec) : ratvec =
    Gaussian_elim.timesMatVec (a, v)

  fun inverse (a: ratmat) : ratmat =
    Gaussian_elim.inverse a

  fun ofIntMat (m: intmat) : ratmat =
    let
      val (nRows, nCols) = IntMatrix.matShape m
      fun entry (i, j) = List.nth (List.nth (m, i), j)
      fun col j = List.tabulate (nRows, fn i => fromInt (entry (i, j)))
    in
      List.tabulate (nCols, col)
    end

  fun toIntMat (m: ratmat) : intmat option =
    let
      val (nRows, nCols) = shape m
      fun asInt q =
        let
          val q = BigRat.normalize q
        in
          if #den q = 1 then SOME (IntInf.toInt (#num q) handle _ => raise Fail "RatMat.toIntMat: overflow")
          else NONE
        end
      fun row i =
        let
          val ints = List.mapPartial asInt (List.tabulate (nCols, fn j => List.nth (List.nth (m, j), i)))
        in
          if length ints = nCols then SOME ints else NONE
        end
      val rowsOpt = List.tabulate (nRows, row)
    in
      if List.all Option.isSome rowsOpt then
        SOME (List.map valOf rowsOpt)
      else
        NONE
    end

  fun equal (a: ratmat, b: ratmat) : bool =
    let
      val (ar, ac) = shape a
      val (br, bc) = shape b
    in
      ar = br andalso ac = bc andalso
      List.all BigRat.equal (ListPair.zipEq (List.concat a, List.concat b))
      handle _ => false
    end

  fun id_mat (n: int) : ratmat = Gaussian_elim.id_mat n

  (* `rational_inverse(mat)` from `ratmat.at`. *)
  fun rational_inverse (m: intmat) : ratmat =
    inverse (ofIntMat m)

  (*
    One-sided inverses

    `ratmat.at` defines:
      left_inverse(A)  = ^[ A * inverse(^A*A) ]
      right_inverse(A) = formal dual (use ^A and transpose at the end)

    These coincide with the usual least-squares pseudoinverse formulas when
    `A` has full column / full row rank over Q.
  *)

  fun left_inverse (a: ratmat) : ratmat =
    let
      val at = transpose a
      val gram = mul (at, a)
      val gramInv = inverse gram
    in
      mul (gramInv, at)
    end

  fun right_inverse (a: ratmat) : ratmat =
    let
      val at = transpose a
      val gram = mul (a, at)
      val gramInv = inverse gram
    in
      mul (at, gramInv)
    end
end

