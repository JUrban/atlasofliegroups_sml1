(*
  File: atlas-scripts-sml/Iterator.sml

  Purpose
  - Small helper module for iterator-style APIs used throughout the SML port.
  - Many ports use a record `{peek, advance}` which mirrors the `.at` notion of
    an `Iterator<T>` with `get` + `incr`.

  Iterator contract
  - `peek()` returns `SOME x` for the current item, or `NONE` when finished.
  - `advance()` moves to the next item; it should only be called when `peek()`
    returns `SOME _`.
  - Calling `peek()` repeatedly without `advance()` should return the same value.
*)

structure Iterator = struct
  type 'a t = {peek: unit -> 'a option, advance: unit -> unit}

  fun take (n: int) (it: 'a t) : 'a list =
    let
      fun loop (k, acc) =
        if k <= 0 then
          List.rev acc
        else
          (case #peek it () of
             NONE => List.rev acc
           | SOME x => (#advance it (); loop (k - 1, x :: acc)))
    in
      loop (n, [])
    end

  fun to_list (it: 'a t) : 'a list =
    let
      fun loop acc =
        (case #peek it () of
           NONE => List.rev acc
         | SOME x => (#advance it (); loop (x :: acc)))
    in
      loop []
    end

  fun count (it: 'a t) : int = length (to_list it)
end

