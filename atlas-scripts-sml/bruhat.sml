use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/tabulate.sml";
use "atlas-scripts-sml/WeylElt.sml";
use "atlas-scripts-sml/weylgroup_at.sml";

(*
  File: atlas-scripts-sml/bruhat.sml

  Purpose
  - SML translation of the KGB-element Bruhat order utilities from
    `atlas-scripts/bruhat.at`.
  - Provides `bruhat_leq` / `bruhat_geq` comparisons for KGB elements in a
    fixed real form (represented here by an `AtlasFFI.group` handle).

  Scope / current limitations
  - This file currently ports the KGB Bruhat order portion of `bruhat.at`.
  - It also ports the Weyl-group Bruhat order portion (`bruhat_W_*`) using the
    minimal `WeylElt` representation in `atlas-scripts-sml/WeylElt.sml`.

  Atlas correspondence
  - In `.at`, `KGBElt` is a value with a `real_form` field. In this SML port we
    represent a KGB element by its integer index `x : int` together with the
    ambient real form encoded by a group handle `g : AtlasFFI.group`.
  - We use the C++ shim primitives:
      - `atlas_kgb_status(g,s,x)`   for root status codes (0..4)
      - `atlas_kgb_cross(g,s,x)`    for cross action
      - `atlas_kgb_cayley(g,s,x)`   for Cayley transforms
      - `atlas_kgb_length(g,x)`     for the Bruhat length used in the algorithm

  API (KGB part)
  - `descend (g,s,x)`         : one- or two-valued descent (C- or real).
  - `bruhat_leq (g,x,y)`      : decide `x <= y` in Bruhat order.
  - `bruhat_geq (g,x,y)`      : decide `x >= y`.
  - `bruhat_leq_table g`      : precompute the boolean matrix `[i][j]=i<=j`.
  - `show_bruhat_leq g`       : pretty-print a table (indices, lengths, upper set).
  - `show_bruhat_geq g`       : pretty-print a table (indices, lengths, lower set).

  Notes
  - This is intended as a literal port of the recursive decision procedure in
    `bruhat.at`. It memoizes subproblems to avoid exponential recursion.
*)

structure Bruhat = struct
  type group = AtlasFFI.group
  type kgb = int
  type rootdatum = RootDatum.t
  type weyl = WeylElt.t

  fun format_int_list (xs: int list) : string =
    "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"

  fun checkNonNeg (name: string, x: int) : int =
    if x < 0 then raise Fail ("Bruhat." ^ name ^ ": failed: " ^ AtlasFFI.atlas_last_error ()) else x

  fun status (g: group, s: int, x: kgb) : int =
    checkNonNeg ("status", AtlasFFI.atlas_kgb_status (g, s, x))

  fun cross (g: group, s: int, x: kgb) : kgb =
    checkNonNeg ("cross", AtlasFFI.atlas_kgb_cross (g, s, x))

  fun cayley (g: group, s: int, x: kgb) : kgb =
    checkNonNeg ("cayley", AtlasFFI.atlas_kgb_cayley (g, s, x))

  fun length (g: group, x: kgb) : int =
    checkNonNeg ("length", AtlasFFI.atlas_kgb_length (g, x))

  (* Single- or multivalued descent along a complex (C-) or real root.
     Mirrors `descend@(int,KGBElt)` from `bruhat.at`. *)
  fun descend (g: group, s: int, x: kgb) : kgb list =
    (case status (g, s, x) of
       0 => [cross (g, s, x)] (* C- *)
     | 2 => (* real: possibly double-valued *)
         let
           val x1 = cayley (g, s, x)
           val x2 = cross (g, s, x1)
         in
           if x1 = x2 then [x1] else [x1, x2]
         end
     | k => raise Fail ("Bruhat.descend: not a descent (status=" ^ Int.toString k ^ ")"))

  (* Find the first strict descent index for `y`: status 0 (C-) or 2 (real).
     The `.at` version uses `is_strict_descent(i,y)` and avoids `ci` cases. *)
  fun firstStrictDescent (g: group, y: kgb) : int option =
    let
      val r = AtlasFFI.atlas_group_semisimple_rank g
      fun loop i =
        if i = r then NONE
        else
          (case status (g, i, y) of
             0 => SOME i
           | 2 => SOME i
           | _ => loop (i + 1))
    in
      loop 0
    end

  (* Build a memoized decision procedure `leq(x,y)` for a fixed group `g`.
     This is useful when making many queries (tables, upper/lower sets). *)
  fun make_bruhat_leq (g: group) : (kgb * kgb -> bool) * (kgb -> int) * int =
    let
      val n = AtlasFFI.atlas_group_kgb_size g
      val () = if n <= 0 then raise Fail "Bruhat.make_bruhat_leq: empty KGB" else ()
      val lens = Array.tabulate (n, fn i => length (g, i))
      val memo : bool option array = Array.array (n * n, NONE)
      fun key (a: int, b: int) : int = a * n + b
      fun len (x: kgb) : int = Array.sub (lens, x)

      fun leq (a: kgb, b: kgb) : bool =
        if a = b then true
        else if len b <= len a then false
        else
          (case Array.sub (memo, key (a, b)) of
             SOME v => v
           | NONE =>
               let
                 val s =
                   (case firstStrictDescent (g, b) of
                      SOME i => i
                    | NONE => raise Fail "Bruhat.make_bruhat_leq: y has no strict descent")
                 val b' = hd (descend (g, s, b))

                 val v =
                   (case status (g, s, a) of
                      0 => leq (hd (descend (g, s, a)), b') (* C- *)
                    | 1 => leq (a, b') (* ic/ci ascent *)
                    | 2 => (* r1 or r2: possibly double-valued *)
                        (case descend (g, s, a) of
                           [u] => leq (u, b') (* r2 *)
                         | u1 :: u2 :: _ => leq (u1, b') orelse leq (u2, b') (* r1 *)
                         | [] => raise Fail "Bruhat.make_bruhat_leq: descend returned empty")
                    | 3 => (* nci1 or nci2 *)
                        let
                          val a' = cross (g, s, a)
                        in
                          if a = a' then leq (a, b') (* nci1 *)
                          else leq (a, b') orelse leq (a', b') (* nci2 *)
                        end
                    | 4 => leq (a, b') (* C+ ascent *)
                    | k => raise Fail ("Bruhat.make_bruhat_leq: bad status " ^ Int.toString k))
               in
                 Array.update (memo, key (a, b), SOME v);
                 v
               end)
    in
      (leq, len, n)
    end

  (* Decide `x <= y` in KGB Bruhat order using the recursive algorithm from
     `bruhat.at`, with memoization on pairs. *)
  fun bruhat_leq (g: group, x: kgb, y: kgb) : bool =
    let
      val (leq, _, _) = make_bruhat_leq g
    in
      leq (x, y)
    end

  fun bruhat_geq (g: group, x: kgb, y: kgb) : bool =
    bruhat_leq (g, y, x)

  (* Return the list of `y` such that `y <= x`. *)
  fun bruhat_leq_list (g: group, x: kgb) : kgb list =
    let
      val (leq, _, n) = make_bruhat_leq g
    in
      List.filter (fn y => leq (y, x)) (List.tabulate (n, fn i => i))
    end

  (* Return the list of `y` such that `y >= x`. *)
  fun bruhat_geq_list (g: group, x: kgb) : kgb list =
    let
      val (leq, _, n) = make_bruhat_leq g
    in
      List.filter (fn y => leq (x, y)) (List.tabulate (n, fn i => i))
    end

  (* Precompute a boolean table `T[i][j] = (i <= j)` for KGB elements. *)
  fun bruhat_leq_table (g: group) : bool list list =
    let
      val (leq, _, n) = make_bruhat_leq g
      fun row i = List.tabulate (n, fn j => leq (i, j))
    in
      List.tabulate (n, row)
    end

  fun bruhat_geq_table (g: group) : bool list list =
    let
      val (leq, _, n) = make_bruhat_leq g
      fun row i = List.tabulate (n, fn j => leq (j, i))
    in
      List.tabulate (n, row)
    end

  fun show_bruhat_leq (g: group) : unit =
    let
      val (leq, len, n) = make_bruhat_leq g
      fun upperSet i =
        let
          fun collect j acc =
            if j = n then List.rev acc
            else if leq (i, j) then collect (j + 1) (j :: acc) else collect (j + 1) acc
        in
          collect 0 []
        end

      val rows =
        ["x", "len", "x<="]
        :: List.tabulate
             ( n
             , fn i =>
                 [ Int.toString i
                 , Int.toString (len i)
                 , format_int_list (upperSet i)
                 ]
             )
    in
      Tabulate.tabulateDefault rows
    end

  fun show_bruhat_geq (g: group) : unit =
    let
      val (leq, len, n) = make_bruhat_leq g
      fun lowerSet i =
        let
          fun collect j acc =
            if j = n then List.rev acc
            else if leq (j, i) then collect (j + 1) (j :: acc) else collect (j + 1) acc
        in
          collect 0 []
        end

      val rows =
        ["x", "len", "x>="]
        :: List.tabulate
             ( n
             , fn i =>
                 [ Int.toString i
                 , Int.toString (len i)
                 , format_int_list (lowerSet i)
                 ]
             )
    in
      Tabulate.tabulateDefault rows
    end

  (* Defaults mirroring `.at`. *)
  val bruhat = bruhat_leq
  val show_bruhat = show_bruhat_leq

  (* ---------------- Weyl-group Bruhat order (RootDatum-based) ---------------- *)

  (* `.at`: lengthens(s,w) = is_positive_coroot(coroot(rd,s) * w) *)
  fun lengthensW (s: int, w: weyl) : bool =
    let
      val rd = WeylElt.root_datum w
    in
      WeylgroupAT.lengthens_left (rd, s, WeylElt.matrix w)
    end

  fun shortensW (s: int, w: weyl) : bool = not (lengthensW (s, w))

  (* `.at`: bruhat_W_leq(x,y) *)
  fun bruhat_W_leq (x: weyl, y: weyl) : bool =
    let
      val rd = WeylElt.root_datum x
      val () =
        if rd = WeylElt.root_datum y then ()
        else raise Fail "Bruhat.bruhat_W_leq: root data don't match"

      fun firstDescent (w: weyl) : int option =
        let
          val ssr = RootDatum.semisimpleRank rd
          fun loop s =
            if s = ssr then NONE else if shortensW (s, w) then SOME s else loop (s + 1)
        in
          loop 0
        end

      fun leq (a: weyl, b: weyl) : bool =
        if WeylElt.eq (a, b) then
          true
        else if WeylElt.length b <= WeylElt.length a then
          false
        else
          (case firstDescent b of
             NONE => raise Fail "Bruhat.bruhat_W_leq: y has no descent"
           | SOME s =>
               let
                 val sElt = WeylElt.simple (rd, s)
                 val b' = WeylElt.mul (sElt, b)
               in
                 if shortensW (s, a) then leq (WeylElt.mul (sElt, a), b') else leq (a, b')
               end)
    in
      leq (x, y)
    end

  fun bruhat_W_geq (x: weyl, y: weyl) : bool = bruhat_W_leq (y, x)
  fun bruhat_W (x: weyl, y: weyl) : bool = bruhat_W_leq (x, y)

  fun sort_by_length (ws: weyl list) : weyl list =
    Basic.sort_by (WeylElt.length, op <=) ws

  fun sort_by_length_descending (ws: weyl list) : weyl list =
    Basic.sort_by (WeylElt.length, op >=) ws
end
