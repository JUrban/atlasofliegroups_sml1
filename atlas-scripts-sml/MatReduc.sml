use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";

structure MatReduc = struct
  type mat = IntMatrix.mat

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("MatReduc: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

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

  fun inLatticeBasis (a: mat, m: mat) : mat =
    let
      val out = AtlasFFI.atlas_intmat_in_lattice_basis_text (IntMatrix.matToText a, IntMatrix.matToText m)
    in
      case parseInts out of
        [~1] => raise Fail ("MatReduc.inLatticeBasis: C++ error: " ^ AtlasFFI.atlas_last_error ())
      | _ => IntMatrix.parseMatText out
    end
end
