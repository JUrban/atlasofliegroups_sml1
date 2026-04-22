use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(*
  File: atlas-scripts-sml/MatReduc.sml

  Purpose
  - Small wrappers around Atlas C++ routines for “matrix reduction” tasks used
    in lattice computations (adapted bases, expressing matrices in a lattice basis).

  Notes
  - These helpers are used by `LambdaDifferential0` and other modules that
    manipulate sublattices/eigenlattices.
*)
structure MatReduc = struct
  type mat = IntMatrix.mat

  (* Parse whitespace-separated integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("MatReduc: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Atlas wrapper for `adapted_basis`:
     returns `(basisMatrix, diag)` where `diag` is the invariant list. *)
  fun adaptedBasis (a: mat) : mat * int list =
    let
      val h = AtlasFFI.atlas_intmat_adapted_basis (IntMatrix.matToText a)
      val () =
        if h = Foreign.Memory.null then
          raise Fail ("MatReduc.adaptedBasis: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()

      val mText = AtlasFFI.atlas_intmat_adapted_basis_matrix_text h
      val dText = AtlasFFI.atlas_intmat_adapted_basis_diag_text h
      val () = AtlasFFI.atlas_intmat_adapted_basis_free h

      val () =
        (case parseInts mText of
           [~1] => raise Fail ("MatReduc.adaptedBasis: matrix_text failed: " ^ AtlasFFI.atlas_last_error ())
         | _ => ())
      val () =
        (case parseInts dText of
           [~1] => raise Fail ("MatReduc.adaptedBasis: diag_text failed: " ^ AtlasFFI.atlas_last_error ())
         | _ => ())

      val m = IntMatrix.parseMatText mText
      val ds = parseInts dText
    in
      case ds of
        len :: rest =>
          if len < 0 then raise Fail "MatReduc.adaptedBasis: negative diag len"
          else if length rest <> len then raise Fail "MatReduc.adaptedBasis: truncated diag"
          else (m, rest)
      | _ => raise Fail "MatReduc.adaptedBasis: empty diag"
    end

  (* Express matrix `m` in the lattice basis given by columns of `a`. *)
  fun inLatticeBasis (a: mat, m: mat) : mat =
    let
      val out = AtlasFFI.atlas_intmat_in_lattice_basis_text (IntMatrix.matToText a, IntMatrix.matToText m)
    in
      case parseInts out of
        [~1] => raise Fail ("MatReduc.inLatticeBasis: C++ error: " ^ AtlasFFI.atlas_last_error ())
      | _ => IntMatrix.parseMatText out
    end
end
