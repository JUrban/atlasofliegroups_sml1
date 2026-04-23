use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/lazy_lists.sml

  Purpose
  - Partial SML translation of `atlas-scripts/lazy_lists.at`.
  - Provides a small library for lazy infinite lists (“streams”) together with
    memoization, which is used by some Atlas `.at` scripts (notably arithmetic
    ones like prime generation).

  Core idea (matching `.at`)
  - An infinite list of `'a` is represented as a thunk producing a node:
      `type 'a infinite_list = unit -> 'a infinite_node`
      `type 'a infinite_node = 'a * 'a infinite_list`
    where each node contains the current head and a thunk for the tail.

  Implemented subset
  - `series coef`          : stream `coef(0), coef(1), ...`
  - `initial n f`          : first `n` elements of a stream as a finite list
  - `prepend x f`          : stream with head `x` and tail `f`
  - `shift n f`            : drop `n` nodes from a stream
  - `constant_term f`      : head of a stream
  - `coefficient i f`      : element at index `i` (0-based)
  - `memoize f`            : cache the first node of a stream thunk (and
                             recursively memoize the tail)

  Not implemented (yet)
  - The formal power series arithmetic layer from the bottom of the `.at`
    file; it depends on additional Atlas-specific helpers and is not currently
    needed by the SML ports in this repo.
*)

structure Lazy_lists = struct
  datatype 'a infinite_list = InfList of unit -> 'a infinite_node
  and 'a infinite_node = InfNode of 'a * 'a infinite_list

  type inf_list = int infinite_list
  type inf_node = int infinite_node

  fun force (InfList f) : 'a infinite_node = f ()

  (* `series coef` = coef(0), coef(1), ... *)
  fun series (coef: int -> 'a) : 'a infinite_list =
    let
      fun up_from n () : 'a infinite_node = InfNode (coef n, InfList (up_from (n + 1)))
    in
      InfList (up_from 0)
    end

  (* First `n` elements of a stream as a finite list. *)
  fun initial (n: int) (f0: 'a infinite_list) : 'a list =
    if n < 0 then
      raise Fail "Lazy_lists.initial: negative n"
    else
      let
        fun loop (0, _, acc) = List.rev acc
          | loop (k, f, acc) =
              let
                val InfNode (head, tail) = force f
              in
                loop (k - 1, tail, head :: acc)
              end
      in
        loop (n, f0, [])
      end

  fun prepend (c: 'a, f: 'a infinite_list) : 'a infinite_list =
    InfList (fn () => InfNode (c, f))

  fun shift (n: int, f0: 'a infinite_list) : 'a infinite_list =
    if n < 0 then
      raise Fail "Lazy_lists.shift: negative n"
    else
      let
        fun loop (0, f) = f
          | loop (k, f) =
              let
                val InfNode (_, tail) = force f
              in
                loop (k - 1, tail)
              end
      in
        loop (n, f0)
      end

  fun constant_term (f: 'a infinite_list) : 'a =
    let
      val InfNode (c, _) = force f
    in
      c
    end

  fun coefficient (i: int, f: 'a infinite_list) : 'a =
    constant_term (shift (i, f))

  (*
    Memoize an infinite list thunk:
    - The first call to the returned thunk evaluates the original one and stores
      the resulting node, but with the tail recursively memoized.
    - Subsequent calls return the stored node without re-evaluation.
  *)
  fun memoize (l: 'a infinite_list) : 'a infinite_list =
    let
      val store : 'a infinite_node option ref = ref NONE
    in
      InfList
        (fn () =>
           (case !store of
              SOME node => node
            | NONE =>
                let
                  val InfNode (head, tail) = force l
                  val node = InfNode (head, memoize tail)
                  val () = store := SOME node
                in
                  node
                end))
    end

  (* ---------- finite prefix + constant tail (from `lazy_lists.at`) ---------- *)

  (* `extend(pol,zero)` = stream with prefix `pol` then infinitely many `zero`. *)
  fun extend (pol: 'a list, zero: 'a) : 'a infinite_list =
    let
      fun zeros () = InfNode (zero, InfList zeros)
      fun loop xs () =
        (case xs of
           [] => zeros ()
         | x :: rest => InfNode (x, InfList (loop rest)))
    in
      InfList (loop pol)
    end

  fun extend_0 (pol: int list) : inf_list = extend (pol, 0)

  val series_0 : inf_list = extend_0 []
  val series_1 : inf_list = extend_0 [1]
  val X : inf_list = extend_0 [0, 1]
  val inf_ones : inf_list = series (fn _ => 1)

  (* ---------- integer stream arithmetic (small subset) ---------- *)

  fun sum_int (f: inf_list, g: inf_list) : inf_list =
    InfList
      (fn () =>
         let
           val InfNode (x, ff) = force f
           val InfNode (y, gg) = force g
         in
           InfNode (x + y, sum_int (ff, gg))
         end)

  fun scale_by_int (a: int) (f: inf_list) : inf_list =
    if a = 1 then f
    else if a = 0 then series_0
    else
      InfList
        (fn () =>
           let
             val InfNode (x, ff) = force f
           in
             InfNode (a * x, scale_by_int a ff)
           end)
end
