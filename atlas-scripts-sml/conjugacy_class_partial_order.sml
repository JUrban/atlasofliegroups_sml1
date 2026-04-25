use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/elliptic.sml";
use "atlas-scripts-sml/bruhat.sml";
use "atlas-scripts-sml/WeylElt.sml";

(*
  File: atlas-scripts-sml/conjugacy_class_partial_order.sml

  Purpose
  - Standard ML translation of `atlas-scripts/conjugacy_class_partial_order.at`.
  - Provides helpers for working with the partial order on Weyl-group
    conjugacy classes induced by Bruhat order:
      - enumerate minimal-length representatives in a conjugacy class
      - decide `[x] <= [y]` by searching minimal representatives of `[x]`
      - compute a transitive-closure table for a list of class reps

  Dependencies
  - Uses the RootDatum-based Weyl-element layer `WeylElt` and the Weyl-group
    Bruhat order `Bruhat.bruhat_W_leq`.

  Notes / limitations
  - The `.at` version assumes the inputs are minimal-length representatives of
    their conjugacy classes in some places; this port follows the same
    convention.
  - The `Elliptic.elliptic_conjugacy_class_poset` wrapper depends on
    `Elliptic.elliptic_conjugacy_class_reps`; see `atlas-scripts-sml/elliptic.sml`
    for current limitations of that generator.
*)

structure ConjugacyClassPartialOrder = struct
  type weyl = WeylElt.t
  type rootdatum = RootDatum.t

  fun fail where' msg = raise Fail ("ConjugacyClassPartialOrder." ^ where' ^ ": " ^ msg)

  fun containsInt (xs: int list, x: int) : bool = List.exists (fn y => y = x) xs

  fun unionInts (a: int list, b: int list) : int list =
    Basic.sort_u (op <=) (a @ b)

  fun simple (rd: rootdatum, s: int) : weyl = WeylElt.simple (rd, s)

  fun conj_by_simple (rd: rootdatum, s: int, w: weyl) : weyl =
    let
      val sW = simple (rd, s)
    in
      WeylElt.mul (sW, WeylElt.mul (w, sW))
    end

  (*
    minimal_representatives(w0) : (conjugators, class)

    `.at`: generate all minimal-length reps of the conjugacy class of `w0`,
    assuming `w0` is already minimal in its class.
  *)
  fun minimal_representatives (w0: weyl) : weyl list * weyl list =
    let
      val rd = WeylElt.root_datum w0
      val ssr = RootDatum.semisimpleRank rd

      val classRef = ref [w0]
      val conjRef = ref [WeylElt.id_W rd]
      val current = ref 0

      fun absent x = not (List.exists (fn y => WeylElt.eq (x, y)) (!classRef))

      fun loop () =
        if !current >= length (!classRef) then
          ()
        else
          let
            val c = List.nth (!conjRef, !current)
            val w = List.nth (!classRef, !current)
            val () = current := !current + 1

            fun stepS s =
              let
                val sws = conj_by_simple (rd, s, w)
              in
                if absent sws andalso WeylElt.length sws = WeylElt.length w then
                  (classRef := !classRef @ [sws];
                   conjRef := !conjRef @ [WeylElt.mul (simple (rd, s), c)])
                else
                  ()
              end
          in
            List.app stepS (List.tabulate (ssr, fn i => i));
            loop ()
          end
    in
      loop ();
      (!conjRef, !classRef)
    end

  (*
    cc_less_than(w0,w1) : bool

    `.at`: decide `[w0] <= [w1]`, assuming `w0` is minimal length in its class.
  *)
  fun cc_less_than (w0: weyl, w1: weyl) : bool =
    if Bruhat.bruhat_W_leq (w0, w1) then
      true
    else if WeylElt.length w0 > WeylElt.length w1 then
      false
    else
      let
        val rd = WeylElt.root_datum w0
        val () = if rd = WeylElt.root_datum w1 then () else fail "cc_less_than" "root data don't match"
        val ssr = RootDatum.semisimpleRank rd

        val classRef = ref [w0]
        val current = ref 0

        fun absent x = not (List.exists (fn y => WeylElt.eq (x, y)) (!classRef))

        fun loop () =
          if !current >= length (!classRef) then
            false
          else
            let
              val w = List.nth (!classRef, !current)
              val () = current := !current + 1

              fun tryS s =
                let
                  val sws = conj_by_simple (rd, s, w)
                in
                  if absent sws andalso WeylElt.length sws = WeylElt.length w then
                    if Bruhat.bruhat_W_leq (sws, w1) then
                      SOME true
                    else
                      (classRef := !classRef @ [sws]; NONE)
                  else
                    NONE
                end

              fun loopS [] = loop ()
                | loopS (s :: ss) =
                    (case tryS s of
                       SOME true => true
                     | SOME false => loopS ss
                     | NONE => loopS ss)
            in
              loopS (List.tabulate (ssr, fn i => i))
            end
      in
        loop ()
      end

  (*
    conjugacy_class_poset(classes) : int list list

    Returns a table `rv` where `rv[i]` is the sorted list of indices `j` such
    that `[classes[i]] <= [classes[j]]` in the induced partial order.

    This matches the intent of the `.at` implementation: it uses transitive
    closure shortcuts `rv[i] := rv[i] ∪ rv[j]` whenever `i <= j`.
  *)
  fun conjugacy_class_poset (classes0: weyl list) : int list list =
    let
      val classes = Bruhat.sort_by_length_descending classes0
      val n = length classes
      val rv = Array.tabulate (n, fn i => [i])

      fun get i = Array.sub (rv, i)
      fun setRow (i, xs) = Array.update (rv, i, xs)

      fun loopI i =
        if i = n then
          ()
        else
          let
            val rowRef = ref [i]

            fun loopJ j =
              if j < 0 then
                ()
              else if containsInt (!rowRef, j) then
                loopJ (j - 1)
              else if cc_less_than (List.nth (classes, i), List.nth (classes, j)) then
                (rowRef := unionInts (!rowRef, get j); loopJ (j - 1))
              else
                loopJ (j - 1)

            val () = loopJ (n - 1)
            val () = setRow (i, !rowRef)
          in
            loopI (i + 1)
          end
    in
      loopI 0;
      List.tabulate (n, fn i => get i)
    end

  fun elliptic_conjugacy_class_poset (rd: rootdatum) : int list list =
    conjugacy_class_poset (Elliptic.elliptic_conjugacy_class_reps rd)
end
