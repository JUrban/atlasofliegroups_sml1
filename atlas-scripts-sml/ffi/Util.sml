structure FFIUtil = struct
  fun withInt32Array (xs: int list) (f: Foreign.Memory.voidStar -> 'a) : 'a =
    let
      val n = List.length xs
      val bytes = Word.fromInt (4 * n)
      val p = Foreign.Memory.malloc bytes
      fun write ([], _) = ()
        | write (x :: rest, i) =
            (Foreign.Memory.set32 (p, Word.fromInt (4 * i), Word32.fromInt x);
             write (rest, i + 1))
      val () = write (xs, 0)
      val result = f p
    in
      Foreign.Memory.free p;
      result
    end
end

