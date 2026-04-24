(*
  File: atlas-scripts-sml/character_table_G.sml

  Purpose
  - Partial SML translation of `atlas-scripts/character_table_G.at`.
  - Provides the (small) W(G2) character table in the fixed class ordering used
    by `class_table_G` in the `.at` code.

  Scope
  - Data-only: we expose class names, irreps (character rows + names), and the
    `to_special_G2` mapping.
  - We do not yet rebuild `WeylClassTable`/`CharacterTable` wrappers in SML.
*)

structure CharacterTable_G = struct
  val class_names : string list =
    [ "e", "S_long", "S_short", "R/2", "R/3", "R/6" ]

  val irreps : (int list * string) list =
    [ ([ 1, 1, 1, 1, 1, 1 ], "trivial")
    , ([ 1, 1, ~1, ~1, 1, ~1 ], "short sign")
    , ([ 1, ~1, 1, ~1, 1, ~1 ], "long sign")
    , ([ 1, ~1, ~1, 1, 1, 1 ], "full sign")
    , ([ 2, 0, 0, 2, ~1, ~1 ], "triangular reflection")
    , ([ 2, 0, 0, ~2, ~1, 1 ], "hexagonal reflection")
    ]

  val () =
    if length class_names = 6
       andalso length irreps = 6
       andalso List.all (fn (chi, _) => length chi = 6) irreps then
      ()
    else
      raise Fail "CharacterTable_G: unexpected table shape"

  val to_special_G2_table : int list = [ 0, 5, 5, 3, 5, 5 ]

  fun to_special_G2 (i: int) : int =
    if i < 0 orelse i >= length to_special_G2_table then
      raise Fail "CharacterTable_G.to_special_G2: index out of range"
    else
      List.nth (to_special_G2_table, i)
end

