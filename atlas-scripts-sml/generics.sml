(*
  File: atlas-scripts-sml/generics.sml

  Purpose
  - Small SML analogue of `atlas-scripts/generics.at`.
  - Provides basic functional-programming combinators used by some translated
    scripts.
*)

structure Generics = struct
  fun I x = x

  fun constant x _ = x

  fun compose g f x = g (f x)

  fun swap (x, y) = (y, x)

  fun swapargs f (y, x) = f (x, y)

  fun curry f x y = f (x, y)

  fun uncurry f (x, y) = f x y

  fun curried_swapargs f y x = f x y

  fun foldl (start, opf) xs = List.foldl (fn (x, acc) => opf (acc, x)) start xs

  fun foldr (opf, start) xs = List.foldr (fn (x, acc) => opf (x, acc)) start xs

  fun combine xs opf =
    (case xs of
       [] => raise Fail "Generics.combine: empty list"
     | x :: rest => List.foldl (fn (y, acc) => opf (acc, y)) x rest)
end

