use "atlas-scripts-sml/Iterator.sml";
use "atlas-scripts-sml/generics.sml";

val () = if Generics.I 3 = 3 then () else raise Fail "I";
val () = if Generics.constant 7 "x" = 7 then () else raise Fail "constant";
val () = if Generics.compose (fn x => x + 1) (fn x => x * 2) 3 = 7 then () else raise Fail "compose";
val () = if Generics.swap (1, 2) = (2, 1) then () else raise Fail "swap";
val () = if Generics.curry (fn (a, b) => a + b) 2 3 = 5 then () else raise Fail "curry";
val () = if Generics.uncurry (fn a => fn b => a + b) (2, 3) = 5 then () else raise Fail "uncurry";

val () = if Generics.foldl (0, op +) [1, 2, 3] = 6 then () else raise Fail "foldl";
val () = if Generics.foldr (op ::, []) [1, 2, 3] = [1, 2, 3] then () else raise Fail "foldr";
val () = if Generics.combine [1, 2, 3, 4] (op +) = 10 then () else raise Fail "combine";

val it : int Iterator.t =
  let
    val i = ref 0
    fun peek () = if !i < 3 then SOME (!i) else NONE
    fun advance () = i := !i + 1
  in
    {peek = peek, advance = advance}
  end;

val () = if Iterator.take 2 it = [0, 1] then () else raise Fail "Iterator.take";
val () = if Iterator.to_list it = [2] then () else raise Fail "Iterator.to_list";

val () = print "OK\n";

