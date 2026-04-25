use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/combinatorics.sml";
use "atlas-scripts-sml/partitions.sml";

(*
  File: atlas-scripts-sml/elliptic.sml

  Purpose
  - Standard ML translation (mostly literal) of the “word-level” portion of
    `atlas-scripts/elliptic.at`.
  - The original script’s main output is *Weyl words* (lists of simple
    reflection indices) representing elliptic conjugacy classes in Weyl groups.

  What is implemented
  - The pure word generators, independent of Atlas interpreter types:
      - `G2_elliptic_words`, `F4_elliptic_words`, `E6_elliptic_words`,
        `E7_elliptic_words`, `E8_elliptic_words`
      - `exceptional_elliptic_conjugacy_class_orders`
      - `elliptic_simple (type,rank)` returning a list of Weyl words, using the
        Bourbaki numbering convention as in the `.at` file.

  What is NOT implemented yet
  - The parts of `elliptic.at` that produce actual `WeylElt` values for an
    arbitrary `RootDatum` (`combine_W_lists`, `elliptic_conjugacy_class_reps`).
    Those require a richer Weyl-element API than the current SML port exposes.

  Conventions
  - A “word” is an `int list` of generator indices.
  - Concatenation corresponds to multiplication in the Weyl group with the
    usual “apply generators from left to right” convention used across the
    Atlas scripts.
*)

structure Elliptic = struct
  type word = int list
  type words = word list

  fun fail where' msg = raise Fail ("Elliptic." ^ where' ^ ": " ^ msg)

  fun charIndex (c: char, s: string) : int option =
    let
      val n = String.size s
      fun loop i =
        if i >= n then NONE
        else if String.sub (s, i) = c then SOME i
        else loop (i + 1)
    in
      loop 0
    end

  (* `up(low,count)` from the `.at` script: [low, low+1, ..., low+count-1]. *)
  fun up (low: int, count: int) : int list =
    if count <= 0 then [] else List.tabulate (count, fn i => low + i)

  (* `down(low,count)` from the `.at` script: [low, low-1, ..., low-count+1]. *)
  fun down (low: int, count: int) : int list =
    if count <= 0 then [] else List.tabulate (count, fn i => low - i)

  (* Local helpers from `elliptic.at` for types B/C/D. *)
  fun BC_cycle (a: int, b: int, n: int) : word =
    up (a, n - a) @ down (b - 1, n - b)

  fun D_cycles (a: int, b: int, c: int, n: int) : word =
    let
      val () = if a < b andalso b < c andalso c <= n then () else fail "D_cycles" "expected a<b<c<=n"
      val w1 = up (a, n - a) @ down (b - 1, n - 1 - b)
      val w2 =
        if c < n then
          up (b, n - b) @ down (c - 1, n - 1 - c)
        else if b < n - 1 then
          up (b, n - b - 2) @ [n - 1]
        else
          []
    in
      w1 @ w2
    end

  (* Exceptional word tables (verbatim from `elliptic.at`). *)
  val G2_elliptic_words : words = [[0, 1], [0, 1, 0, 1], [0, 1, 0, 1, 0, 1]]

  val F4_elliptic_words : words =
    [ up (0, 4)
    , [0, 1, 2, 1, 2, 3]
    , [0, 1, 0, 2, 1, 2, 3, 2]
    , [0, 1, 2, 1, 2, 3, 2, 1, 2, 3]
    , [0, 1, 0, 2, 1, 0, 2, 1, 2, 3]
    , [0, 1, 0, 2, 1, 0, 2, 3, 2, 1, 2, 3]
    , [0, 1, 0, 2, 1, 0, 2, 1, 2, 3, 2, 1, 2, 3]
    , [0, 1, 0, 2, 1, 0, 2, 3, 2, 1, 0, 2, 1, 2, 3, 2]
    , [0, 1, 0, 2, 1, 0, 2, 1, 2, 3, 2, 1, 0, 2, 1, 2, 3, 2, 1, 0, 2, 1, 2, 3]
    ]

  val E6_elliptic_words : words =
    [ up (0, 6)
    , [0, 1, 2, 3, 1, 4, 3, 5]
    , [0, 1, 2, 0, 3, 1, 2, 3, 4, 3, 5, 4]
    , [0, 1, 2, 3, 1, 2, 3, 4, 3, 1, 2, 3, 4, 5]
    , [0, 1, 2, 0, 3, 1, 2, 0, 3, 4, 3, 1, 2, 0, 3, 4, 5, 4, 3, 1, 2, 3, 4, 5]
    ]

  val E7_elliptic_words : words =
    [ up (0, 7)
    , [0, 1, 2, 3, 1, 4, 3, 5, 6]
    , [0, 1, 2, 3, 1, 4, 3, 5, 4, 3, 6]
    , [0, 1, 2, 3, 1, 4, 3, 1, 2, 3, 4, 5, 6]
    , [0, 1, 2, 3, 1, 2, 3, 4, 3, 1, 2, 3, 4, 5, 6]
    , [0, 1, 2, 3, 1, 2, 3, 4, 3, 1, 2, 3, 4, 5, 4, 6, 5]
    , [0, 1, 2, 0, 3, 1, 2, 0, 3, 2, 4, 3, 1, 2, 3, 5, 4, 3, 6, 5, 4]
    , [0, 1, 2, 0, 3, 1, 2, 0, 3, 2, 4, 3, 1, 2, 0, 3, 2, 4, 3, 5, 4, 6, 5]
    , [0, 1, 2, 0, 3, 1, 2, 0, 3, 4, 3, 1, 2, 0, 3, 4, 5, 4, 3, 1, 2, 3, 4, 5, 6]
    , [0, 1, 2, 3, 1, 2, 3, 4, 3, 1, 2, 3, 4, 5, 4, 3, 1, 2, 3, 4, 5, 6, 5, 4, 3, 1, 2, 3, 4, 5, 6]
    , [0, 1, 2, 0, 3, 1, 2, 3, 4, 3, 1, 2, 0, 3, 4, 5, 4, 3, 1, 2, 3, 4, 5, 6, 5, 4, 3, 1, 2, 3, 4, 5, 6]
    , [ 0, 1, 2, 0, 3, 1, 2, 0, 3, 2, 4, 3, 1, 2, 0, 3, 2, 4, 3, 1, 5, 4, 3, 1, 2, 0, 3
      , 2, 4, 3, 1, 5, 4, 3, 2, 0, 6, 5, 4, 3, 1, 2, 0, 3, 2, 4, 3, 1, 5, 4, 3, 2, 0
      , 6, 5, 4, 3, 1, 2, 3, 4, 5, 6
      ]
    ]

  val E8_elliptic_words : words =
    [ up (0, 8)
    , [0, 1, 2, 3, 1, 4, 3, 5, 6, 7]
    , [0, 1, 2, 3, 1, 4, 3, 5, 4, 3, 6, 7]
    , [0, 1, 2, 0, 3, 1, 2, 3, 4, 3, 5, 4, 6, 7]
    , [0, 1, 2, 0, 3, 1, 2, 3, 4, 3, 5, 4, 6, 5, 4, 7]
    , [0, 1, 2, 3, 1, 2, 3, 4, 3, 1, 2, 3, 4, 5, 6, 7]
    , [0, 1, 2, 3, 1, 2, 3, 4, 3, 1, 2, 3, 4, 5, 4, 6, 5, 7]
    , [0, 1, 2, 0, 3, 1, 2, 5, 4, 3, 6, 5, 4, 3, 7]
    , [0, 1, 2, 3, 1, 2, 3, 4, 3, 1, 2, 3, 4, 5, 4, 3, 6, 5, 4, 7, 6, 5]
    , [0, 1, 2, 0, 3, 1, 2, 0, 3, 2, 4, 3, 1, 2, 3, 5, 4, 3, 6, 5, 4, 7]
    , [ 0, 1, 2, 0, 3, 1, 2, 0, 3, 4, 3, 1, 2, 3, 4, 5, 4, 3, 6, 5, 4, 7, 6, 5 ]
    , [ 0, 1, 2, 0, 3, 1, 2, 0, 3, 2, 4, 3, 1, 2, 0, 3, 2, 4, 3, 5, 4, 6, 5, 7 ]
    , [ 0, 1, 2, 0, 3, 1, 2, 0, 3, 4, 3, 1, 2, 0, 3, 4, 5, 4, 3, 1, 2, 3, 4, 5, 6, 7 ]
    , [ 0, 1, 2, 0, 3, 1, 2, 0, 3, 4, 3, 1, 2, 0, 3, 2, 4, 5, 4, 3, 6, 5, 4, 7, 6, 5 ]
    , [ 0, 1, 2, 0, 3, 1, 2, 0, 3, 4, 3, 1, 2, 0, 3, 4, 5, 4, 3, 1, 2, 3, 4, 5, 6, 5, 7, 6 ]
    , [ 0, 1, 2, 0, 3, 1, 2, 0, 3, 2, 4, 3, 1, 2, 0, 3, 5, 4, 3, 1, 2, 3, 6, 5, 4, 3, 7, 6, 5, 4 ]
    , [ 0, 1, 2, 3, 1, 2, 3, 4, 3, 1, 2, 3, 4, 5, 4, 3, 1, 2, 3, 4, 5, 6, 5, 4, 3, 1, 2, 3, 4, 5, 6, 7 ]
    , [ 0, 1, 2, 0, 3, 1, 2, 3, 4, 3, 1, 2, 0, 3, 4, 5, 4, 3, 1, 2, 3, 4, 5, 6, 5, 4, 3, 1, 2, 3, 4, 5, 6, 7 ]
    ]

  val exceptional_elliptic_conjugacy_class_orders : int list list =
    [ [2, 2, 1]
    , [96, 144, 16, 32, 32, 12, 36, 16, 1]
    , [4320, 5760, 720, 1440, 80]
    , [ 161280, 207360, 120960, 96768, 48384, 90720, 2240, 13440, 20160, 672, 3780, 1 ]
    , [ 23224320, 29030400, 34836480, 12902400, 23224320, 6451200, 24883200, 2419200, 9676800
      , 2419200, 1161216, 4838400, 2419200, 11612160, 12902400, 3628800, 580608, 5443200, 4480
      , 89600, 268800, 403200, 806400, 1209600, 1161216, 15120, 2240, 37800, 4480, 1
      ]
    ]

  (*
    elliptic_simple(type,rank) : words

    `.at`: case char_index(type,"ABCDEFG") in ...

    Returns Weyl words for elliptic conjugacy classes in the simple Weyl group
    of the given Lie type and rank.
  *)
  fun elliptic_simple (typeName: string, rank: int) : words =
    if String.size typeName = 0 then
      fail "elliptic_simple" "empty type string"
    else
      let
        val c = String.sub (typeName, 0)
      in
        case charIndex (c, "ABCDEFG") of
          SOME 0 => [up (0, rank)] (* A: Coxeter element only *)
        | SOME 1 => List.map
            (fn (p: Partitions.partition) =>
               let
                 (* Maintain `acc` as the reverse of the output word. *)
                 fun loopParts ([], _, accRev) = accRev
                   | loopParts (part :: rest, a, acc) =
                       loopParts (rest, a + part, List.revAppend (BC_cycle (a, a + part, rank), acc))
               in
                 List.rev (loopParts (p, 0, []))
               end)
            (Partitions.partitions rank)
        | SOME 2 => (* C: same as B *)
            elliptic_simple ("B", rank)
        | SOME 3 => (* D: partitions with even number of parts, paired into D_cycles *)
            let
              fun evenParts p = (length p) mod 2 = 0
              fun mkWord (p: Partitions.partition) : word =
                let
                  (* Maintain `acc` as the reverse of the output word. *)
                  fun loop ([], _, accRev) = accRev
                    | loop (_ :: [], _, _) = fail "elliptic_simple/D" "odd number of parts"
                    | loop (p1 :: p2 :: rest, a, acc) =
                        let
                          val b = a + p1
                          val c = b + p2
                          val w = D_cycles (a, b, c, rank)
                        in
                          loop (rest, c, List.revAppend (w, acc))
                        end
                in
                  List.rev (loop (p, 0, []))
                end
            in
              List.map mkWord (List.filter evenParts (Partitions.partitions rank))
            end
        | SOME 4 => (* E: tabulated for E6/E7/E8 *)
            (case rank of
               6 => E6_elliptic_words
             | 7 => E7_elliptic_words
             | 8 => E8_elliptic_words
             | _ => fail "elliptic_simple/E" "rank must be 6,7,8")
        | SOME 5 => (* F: only F4 is supported in the `.at` table *)
            if rank = 4 then F4_elliptic_words else fail "elliptic_simple/F" "rank must be 4"
        | SOME 6 => (* G: only G2 is supported in the `.at` table *)
            if rank = 2 then G2_elliptic_words else fail "elliptic_simple/G" "rank must be 2"
        | SOME _ => fail "elliptic_simple" ("unknown type " ^ typeName)
        | NONE => fail "elliptic_simple" ("unknown type " ^ typeName)
      end

  (* Stubs for the `WeylElt`-level functions; left unported for now. *)
  type weyl_elt = unit
  type rootdatum = unit

  fun combine_W_lists (_: weyl_elt list list) : weyl_elt list =
    fail "combine_W_lists" "not yet ported (needs WeylElt representation)"

  fun elliptic_conjugacy_class_reps (_: rootdatum) : weyl_elt list =
    fail "elliptic_conjugacy_class_reps" "not yet ported (needs RootDatum/WeylElt API)"
end
