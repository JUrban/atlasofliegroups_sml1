use "atlas-scripts-sml/IntListData.sml";

(*
  File: atlas-scripts-sml/character_table_F.sml

  Purpose
  - Partial SML translation of `atlas-scripts/character_table_F.at`.
  - The `.at` script defines a complete character table for the Weyl group
    W(F4), written in the fixed Kondo conjugacy-class ordering used by
    `classes_Kondo_F4` (defined in `class_tables.at`).

  Scope of this port
  - This module provides the *raw character data* and the `to_special_F4`
    mapping.
  - It does NOT yet rebuild the full `WeylClassTable`/`CharacterTable`
    infrastructure, nor does it implement the class-reordering step
    `Kondo_positions(Wct)` from the `.at` file.

  Provided values
  - `character_table_F4_data`:
      list of 25 triples `(chi, degree, n_primes)` where `chi` is an integer
      vector of length 25 (Kondo class order).
  - `irreps_kondo`:
      list of `(chi, name)` where `name` is the `.at` naming scheme
      `phi(dim,degree)` with `'` repeated `n_primes` times.
  - `to_special_F4`: mapping from irrep index to “special” representative index.
*)

structure CharacterTable_F = struct
  fun replicateChar (c: char, n: int) : string =
    if n <= 0 then "" else String.implode (List.tabulate (n, fn _ => c))

  fun F4_name (dim: int, degree: int, n_primes: int) : string =
    "phi(" ^ Int.toString dim ^ "," ^ Int.toString degree ^ ")"
    ^ replicateChar (#"'", n_primes)

  val character_table_F4_data : (int list * int * int) list =
    [ ([ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ], 0, 0 )
    , ([ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~1, ~1, ~1, ~1, ~1, ~1, ~1, ~1, ~1 ], 12, 2 )
    , ([ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~1, ~1, ~1, ~1, ~1, 1, 1, 1, 1, 1, ~1, ~1, ~1, ~1 ], 12, 1 )
    , ([ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~1, ~1, ~1, ~1, ~1, ~1, ~1, ~1, ~1, ~1, 1, 1, 1, 1 ], 24, 0 )
    , ([ 2, 2, 2, 2, 2, 2, ~1, ~1, ~1, ~1, ~1, 2, 2, ~1, ~1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 4, 2 )
    , ([ 2, 2, 2, 2, 2, 2, ~1, ~1, ~1, ~1, ~1, ~2, ~2, 1, 1, ~2, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 16, 1 )
    , ([ 2, 2, 2, ~1, ~1, 2, 2, 2, ~1, ~1, ~1, 0, 0, 0, 0, 0, 2, 2, ~1, ~1, 2, 0, 0, 0, 0 ], 4, 1 )
    , ([ 2, 2, 2, ~1, ~1, 2, 2, 2, ~1, ~1, ~1, 0, 0, 0, 0, 0, ~2, ~2, 1, 1, ~2, 0, 0, 0, 0 ], 16, 2 )
    , ([ 4, 4, 4, ~2, ~2, 4, ~2, ~2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 8, 0 )
    , ([ 9, 9, 1, 0, 0, ~3, 0, 0, 0, 0, 0, 3, 3, 0, 0, ~1, 3, 3, 0, 0, ~1, 1, 1, 1, ~1 ], 2, 0 )
    , ([ 9, 9, 1, 0, 0, ~3, 0, 0, 0, 0, 0, 3, 3, 0, 0, ~1, ~3, ~3, 0, 0, 1, ~1, ~1, ~1, 1 ], 6, 2 )
    , ([ 9, 9, 1, 0, 0, ~3, 0, 0, 0, 0, 0, ~3, ~3, 0, 0, 1, 3, 3, 0, 0, ~1, ~1, ~1, ~1, 1 ], 6, 1 )
    , ([ 9, 9, 1, 0, 0, ~3, 0, 0, 0, 0, 0, ~3, ~3, 0, 0, 1, ~3, ~3, 0, 0, 1, 1, 1, 1, ~1 ], 10, 0 )
    , ([ 6, 6, ~2, 0, 0, 2, 0, 0, 3, 3, ~1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, ~2, ~2, 0 ], 6, 1 )
    , ([ 6, 6, ~2, 0, 0, 2, 0, 0, 3, 3, ~1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~2, 2, 2, 0 ], 6, 2 )
    , ([ 12, 12, ~4, 0, 0, 4, 0, 0, ~3, ~3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 4, 0 )
    , ([ 4, ~4, 0, 1, ~1, 0, 1, ~1, ~2, 2, 0, 2, ~2, ~1, 1, 0, 2, ~2, ~1, 1, 0, 0, 2, ~2, 0 ], 1, 0 )
    , ([ 4, ~4, 0, 1, ~1, 0, 1, ~1, ~2, 2, 0, 2, ~2, ~1, 1, 0, ~2, 2, 1, ~1, 0, 0, ~2, 2, 0 ], 7, 2 )
    , ([ 4, ~4, 0, 1, ~1, 0, 1, ~1, ~2, 2, 0, ~2, 2, 1, ~1, 0, 2, ~2, ~1, 1, 0, 0, ~2, 2, 0 ], 7, 1 )
    , ([ 4, ~4, 0, 1, ~1, 0, 1, ~1, ~2, 2, 0, ~2, 2, 1, ~1, 0, ~2, 2, 1, ~1, 0, 0, 2, ~2, 0 ], 13, 0 )
    , ([ 8, ~8, 0, 2, ~2, 0, ~1, 1, 2, ~2, 0, 4, ~4, 1, ~1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 3, 2 )
    , ([ 8, ~8, 0, 2, ~2, 0, ~1, 1, 2, ~2, 0, ~4, 4, ~1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 9, 1 )
    , ([ 8, ~8, 0, ~1, 1, 0, 2, ~2, 2, ~2, 0, 0, 0, 0, 0, 0, 4, ~4, 1, ~1, 0, 0, 0, 0, 0 ], 3, 1 )
    , ([ 8, ~8, 0, ~1, 1, 0, 2, ~2, 2, ~2, 0, 0, 0, 0, 0, 0, ~4, 4, ~1, 1, 0, 0, 0, 0, 0 ], 9, 2 )
    , ([ 16, ~16, 0, ~2, 2, 0, ~2, 2, ~2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 5, 0 )
    ]

  val () =
    if length character_table_F4_data = 25
       andalso List.all (fn (chi, _, _) => length chi = 25) character_table_F4_data then
      ()
    else
      raise Fail "CharacterTable_F: unexpected character_table_F4_data shape"

  val irreps_kondo : (int list * string) list =
    List.map
      (fn (chi, degree, n_primes) =>
         let
           val dim = List.nth (chi, 0)
         in
           (chi, F4_name (dim, degree, n_primes))
         end)
      character_table_F4_data

  val to_special_F4_table : int list =
    [ 0, 15, 15, 3
    , 16, 19, 16, 19
    , 15
    , 9, 15, 15, 12
    , 15, 15
    , 15
    , 16, 15, 15, 19
    , 20, 21, 22, 23
    , 15
    ]

  val () =
    if length to_special_F4_table = 25 then
      ()
    else
      raise Fail "CharacterTable_F: unexpected to_special_F4_table length"

  fun to_special_F4 (i: int) : int =
    if i < 0 orelse i >= length to_special_F4_table then
      raise Fail "CharacterTable_F.to_special_F4: index out of range"
    else
      List.nth (to_special_F4_table, i)
end

