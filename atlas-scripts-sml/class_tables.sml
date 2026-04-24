use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/class_tables.sml

  Purpose
  - Partial SML translation of `atlas-scripts/class_tables.at`.
  - The `.at` file defines a substantial amount of Weyl-group conjugacy-class
    infrastructure (`WeylClassTable`) for many root data types.

  Scope of this port (current)
  - This module is intentionally incremental. At this stage we implement only
    the G2 Weyl-group class table (`class_table_G`) in a way that is sufficient
    to support character-table scripts such as `character_table_G.at`.
  - Types F4/E6/E7/E8, and the generic `W_class_table` constructor, are not yet
    ported to SML.

  Representation choice
  - We represent Weyl-group elements as `WeylWord.t` (simple reflection words),
    and we compute class membership for G2 using the same invariant as the `.at`
    code: `magic_coweight*(w*rho) mod 15`.
*)

structure WeylClassTable = struct
  type weyl_word = WeylWord.t

  type t =
    { n_classes: int
    , class_representatives: weyl_word list
    , class_sizes: int list
    , class_orders: int list
    , class_of: weyl_word -> int
    , class_power: int * int -> int
    }
end

structure ClassTables = struct
  type group = AtlasFFI.group
  type weyl_word = WeylWord.t
  type ratvec = Lattice.ratvec

  (* -------------------- G2 -------------------- *)

  val G2_class_words : weyl_word list =
    [ []
    , [ 1 ]
    , [ 0 ]
    , [ 0, 1, 0, 1, 0, 1 ]
    , [ 0, 1, 0, 1 ]
    , [ 0, 1 ]
    ]

  fun looksLikeG2 (g: group) : bool =
    AtlasFFI.atlas_group_semisimple_rank g = 2
    andalso AtlasFFI.atlas_group_rank g = 2

  fun rho (g: group) : ratvec =
    AllParameters.parseRatWeightText (AtlasFFI.atlas_group_rho_text g)

  (* Pair the G2 “magic coweight” with a weight expressed in fundamental-weight
     coordinates. For a weight `v = (v0,v1)`, this pairing is `5*v0 + 9*v1`.

     In the `.at` file:
       magic_coweight = 5*coroot(rd,map[0]) + 9*coroot(rd,map[1]).
     In fundamental-weight coordinates, `coroot(i)` pairs with coordinate `i`.
  *)
  fun g2_magic_pair (v: ratvec) : int =
    let
      val v = Lattice.ratvecNormalize v
      val den = #den v
      val nums = #nums v
      val () =
        if length nums = 2 then
          ()
        else
          raise Fail "ClassTables.g2_magic_pair: expected rank 2"
      val n0 = List.nth (nums, 0)
      val n1 = List.nth (nums, 1)
      val num = 5 * n0 + 9 * n1
    in
      if den = 1 then
        num
      else if num mod den = 0 then
        num div den
      else
        raise Fail "ClassTables.g2_magic_pair: non-integral pairing"
    end

  fun imod (n: int, m: int) : int =
    let
      val r = n mod m
    in
      if r < 0 then r + m else r
    end

  (* Map residue class in `0..14` to a conjugacy class number in `0..5`,
     or `~1` for “die” (should not occur for Weyl group elements in G2). *)
  val g2_residue_to_class : int list =
    [ ~1, 3, 1, ~1, 2, ~1, ~1, 5, 4, ~1, ~1, 1, ~1, 2, 0 ]

  fun class_table_G (g: group) : WeylClassTable.t =
    if not (looksLikeG2 g) then
      raise Fail "ClassTables.class_table_G: expected a rank-2 semisimple group (G2)"
    else
      let
        val rho0 = rho g

        fun class_of_word (w: weyl_word) : int =
          let
            val v = WeylWord.actRatvec (g, w, rho0)
            val r = imod (g2_magic_pair v, 15)
            val cls = List.nth (g2_residue_to_class, r)
          in
            if cls < 0 then raise Fail "ClassTables.class_table_G: invariant hit 'die'" else cls
          end

        val class_sizes = [ 1, 3, 3, 1, 2, 2 ]
        val class_orders = [ 1, 2, 2, 2, 3, 6 ]

        fun class_power (i: int, k: int) : int =
          if i < 0 orelse i >= 6 then
            raise Fail "ClassTables.class_power(G2): class index out of range"
          else
            (case i of
               0 => 0
             | 1 => (case k of 0 => 0 | 1 => 1 | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | 2 => (case k of 0 => 0 | 1 => 2 | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | 3 => (case k of 0 => 0 | 1 => 3 | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | 4 => (case k of 0 => 0 | 1 => 4 | 2 => 4 | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | 5 =>
                 (case k of
                    0 => 0
                  | 1 => 5
                  | 2 => 4
                  | 3 => 3
                  | 4 => 4
                  | 5 => 5
                  | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | _ => raise Fail "ClassTables.class_power(G2): unreachable")
      in
        { n_classes = 6
        , class_representatives = G2_class_words
        , class_sizes = class_sizes
        , class_orders = class_orders
        , class_of = class_of_word
        , class_power = class_power
        }
      end
end

