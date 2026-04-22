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

  fun toCText (s: string) : string =
    String.translate (fn #"~" => "-" | c => str c) s

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
end

