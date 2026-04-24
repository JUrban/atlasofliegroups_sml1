use "atlas-scripts-sml/class_tables.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/WeylWord.sml";

(*
  File: atlas-scripts-sml/character_table_reps.sml

  Purpose
  - Partial SML translation of `atlas-scripts/character_table_reps.at`.
  - The `.at` file contains utilities relating Weyl-group characters and
    “W-representations” coming from cells, including induction between Weyl
    groups of Levi subgroups and permutations of simple roots.

  Scope of this port (current)
  - Implements only the core character-induction routine:
      `induce_character(Wct_L, Wct_G, f, pi_L) : [int]`
    together with a convenience wrapper for `CharacterTables.CharacterTable.t`.
  - The more advanced parts of the `.at` file (cell actions, `W_rep`, root datum
    embeddings via `Levi_witness`/`root_permutation`, etc.) are not yet ported.

  Mathematical background (same as `.at`)
  - Given a group homomorphism `f : W(L) -> W(G)` and a class function `pi_L`
    tabulated on conjugacy classes of `W(L)`, define the induced class function
    `pi_G` on `W(G)` by:
      pi_G(C) = [W(G):W(L)] * sum_{C_j subset C ∩ W(L)} |C_j|/|C| * pi_L(C_j).
  - Operationally, for each conjugacy class representative `w_L` in `W(L)`,
    compute `w_G = f(w_L)`, determine its class `C` in `W(G)`, and add:
      pi_L(w_L) * [W(G):W(L)] * |class(w_L)| / |class(w_G)|
    to the corresponding entry of `pi_G`.
*)

structure CharacterTableReps = struct
  type weyl_word = WeylWord.t
  type wct = WeylClassTable.t
  type ct = CharacterTables.CharacterTable.t

  fun order_W_wct (w: wct) : int =
    List.foldl (op +) 0 (#class_sizes w)

  fun induce_character_wct
    ( Wct_L: wct
    , Wct_G: wct
    , f: weyl_word -> weyl_word
    , pi_L: int list
    ) : int list =
    let
      val nL = #n_classes Wct_L
      val nG = #n_classes Wct_G
      val () =
        if length pi_L = nL then
          ()
        else
          raise Fail "CharacterTableReps.induce_character_wct: pi_L length mismatch"

      val orderL = order_W_wct Wct_L
      val orderG = order_W_wct Wct_G
      val () =
        if orderL > 0 andalso orderG > 0 andalso orderG mod orderL = 0 then
          ()
        else
          raise Fail "CharacterTableReps.induce_character_wct: bad group orders"
      val index = orderG div orderL

      val valuesG = Array.array (nG, 0)

      fun addAt (j: int, delta: int) =
        Array.update (valuesG, j, Array.sub (valuesG, j) + delta)

      fun loop (i: int, repsL: weyl_word list) : unit =
        (case repsL of
           [] => ()
         | wL :: rest =>
             let
               val wG = f wL
               val j = (#class_of Wct_G) wG
               val () =
                 if j < 0 orelse j >= nG then
                   raise Fail "CharacterTableReps.induce_character_wct: class_of returned out-of-range"
                 else
                   ()

               val clsSizeL = List.nth (#class_sizes Wct_L, i)
               val clsSizeG = List.nth (#class_sizes Wct_G, j)
               val num = index * clsSizeL
               val () =
                 if clsSizeG <> 0 andalso num mod clsSizeG = 0 then
                   ()
                 else
                   raise Fail "CharacterTableReps.induce_character_wct: non-integral centralizer index"

               val factor = num div clsSizeG
               val delta = List.nth (pi_L, i) * factor
             in
               addAt (j, delta);
               loop (i + 1, rest)
             end)

      val () = loop (0, #class_representatives Wct_L)
    in
      List.tabulate (nG, fn j => Array.sub (valuesG, j))
    end

  fun induce_character_ct
    ( ct_L: ct
    , ct_G: ct
    , f: weyl_word -> weyl_word
    , pi_L: int list
    ) : int list =
    induce_character_wct (#class_table ct_L, #class_table ct_G, f, pi_L)
end

