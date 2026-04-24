use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/IntListData.sml";

(*
  File: atlas-scripts-sml/character_table_E7.sml

  Purpose
  - Partial SML translation of `atlas-scripts/character_table_E7.at`.
  - This port focuses on precomputed data for W(E7) irreducible characters in
    Magma ordering, and the derived full character table built by tensoring
    the provided half-table with the sign character (as in the `.at` script).

  What is provided
  - `E7_positions`: the Magma indices of the characters explicitly listed in
    the `.at` half-table.
  - `e7_half_table`: 30 characters (each length 60) in the order matching
    `E7_positions`.
  - `e7_characters`: the full list of 60 characters (each length 60) in Magma
    order, reconstructed exactly as in `character_table_E7.at`.
  - `e7_profile_cols`: for each conjugacy class (in Magma order), the profile
    vector `[order, class_size, sign_value, reflection_value]` used for
    ranking/lookup in the `.at` file.
  - `to_special_E7`: the “to special” index map from the `.at` file.

  Data source
  - `atlas-scripts-sml/data/character_table_E7_positions.txt`
  - `atlas-scripts-sml/data/character_table_E7_half_table.txt`
  - `atlas-scripts-sml/data/character_table_E7_orders_magma.txt`
  - `atlas-scripts-sml/data/character_table_E7_sizes_magma.txt`
*)

structure CharacterTable_E7 = struct
  fun appi f xs =
    let
      fun loop ([], _, _) = ()
        | loop (x :: rest, i, f) = (f (i, x); loop (rest, i + 1, f))
    in
      loop (xs, 0, f)
    end

  fun tensor (x: int list, y: int list) : int list =
    ListPair.mapEq (op * ) (x, y)

  fun complement (n: int, xs: int list) : int list =
    let
      val mark = Array.array (n, false)
      val () =
        List.app
          (fn i =>
             if i < 0 orelse i >= n then
               raise Fail "CharacterTable_E7.complement: out of range"
             else
               Array.update (mark, i, true))
          xs
      fun loop i acc =
        if i = n then
          List.rev acc
        else if Array.sub (mark, i) then
          loop (i + 1) acc
        else
          loop (i + 1) (i :: acc)
    in
      loop 0 []
    end

  val E7_positions : int list =
    (case IntListData.loadIntLists "atlas-scripts-sml/data/character_table_E7_positions.txt" of
       [xs] => xs
     | _ => raise Fail "CharacterTable_E7: expected a single line of positions")

  val e7_half_table : int list list =
    IntListData.loadIntLists "atlas-scripts-sml/data/character_table_E7_half_table.txt"

  val e7_orders_magma : int list =
    IntListData.loadInts "atlas-scripts-sml/data/character_table_E7_orders_magma.txt"

  val e7_sizes_magma : int list =
    IntListData.loadInts "atlas-scripts-sml/data/character_table_E7_sizes_magma.txt"

  val () =
    if length E7_positions = 30 andalso length e7_half_table = 30 andalso List.all (fn row => length row = 60) e7_half_table then
      ()
    else
      raise Fail "CharacterTable_E7: unexpected positions/half_table shape"

  val () =
    if length e7_orders_magma = 60 andalso length e7_sizes_magma = 60 then
      ()
    else
      raise Fail "CharacterTable_E7: unexpected orders/sizes length"

  val e7_characters : int list list =
    let
      val sign = List.hd e7_half_table
      val comp = complement (60, E7_positions)
      val result = Array.array (60, ([]: int list))

      fun set i v = Array.update (result, i, v)
      fun get i = Array.sub (result, i)

      val () =
        appi
          (fn (i, p) =>
             let
               val chi = List.nth (e7_half_table, i)
               val () = set p chi
               val () = set (List.nth (comp, i)) (tensor (chi, sign))
             in
               ()
             end)
          E7_positions

      val out = Array.foldr (op ::) [] result
      val () = if List.all (fn row => length row = 60) out then () else raise Fail "CharacterTable_E7: bad reconstruction"
    in
      out
    end

  val sign_index = 1
  val reflection_index = 3

  val sign_char = List.nth (e7_characters, sign_index)
  val reflection_char = List.nth (e7_characters, reflection_index)

  val e7_profile_cols : int list list =
    List.tabulate
      (60, fn j =>
         [ List.nth (e7_orders_magma, j)
         , List.nth (e7_sizes_magma, j)
         , List.nth (sign_char, j)
         , List.nth (reflection_char, j)
         ])

  val to_special_E7_table : int list =
    [ 0, 1
    , 2, 3
    , 29, 28
    , 6, 16, 17, 9
    , 10, 11
    , 16, 48, 49, 17
    , 16, 17
    , 48, 49
    , 56, 57
    , 22, 28, 29, 25, 26, 27
    , 28, 29
    , 30, 31
    , 32, 33, 34, 35, 54, 55
    , 38, 39, 40, 41
    , 54, 55
    , 48, 48, 49, 49
    , 48, 49
    , 56, 57
    , 52, 53
    , 54, 55
    , 56, 57
    , 58, 58
    ]

  fun to_special_E7 (i: int) : int =
    if i < 0 orelse i >= length to_special_E7_table then
      raise Fail "CharacterTable_E7.to_special_E7: index out of range"
    else
      List.nth (to_special_E7_table, i)
end
