use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/WeylWord.sml";

(*
  File: atlas-scripts-sml/cross_W_orbit.sml

  Purpose
  - Partial SML translation of `atlas-scripts/cross_W_orbit.at`, focused on the
    `cross_divide` operation:
      given KGB elements `x` and `y`, find a Weyl word `w` such that
        `y = cross(w, x)`
    where `cross(WeylElt w, KGBElt x)` is the `.at` left cross action from
    `basic.at` (apply the simple-reflection word in reverse order).

  Implementation
  - We perform a BFS in the graph whose edges are the simple cross actions
    `x -> cross(s, x)` for each simple generator `s`.
  - During BFS we store a parent pointer `(prev, s)` so we can reconstruct a
    witness word. The reconstructed word is in the `.at` `word(w)` convention:
    if `y = cross(w, x)`, then `word(w)` is the returned list.

  Notes
  - This is intentionally a minimal “search” implementation, not a full port of
    the `.at` hash-based orbit enumerator.
  - For the ranks/groups we currently target, KGB sizes are small enough that
    BFS over the entire KGB set is acceptable.
*)

structure CrossWOrbit = struct
  type group = AtlasFFI.group
  type weyl_word = WeylWord.t

  (* Find a word `w` with `y = cross(w, x)`; raises if not found. *)
  fun cross_divide (g: group, y: int, x: int) : weyl_word =
    if y = x then
      []
    else
      let
        val n = AtlasFFI.atlas_group_kgb_size g
        val r = AtlasFFI.atlas_group_semisimple_rank g
        val () =
          if x < 0 orelse x >= n orelse y < 0 orelse y >= n then
            raise Fail "CrossWOrbit.cross_divide: KGB index out of range"
          else
            ()

        val visited = Array.array (n, false)
        val parentPrev = Array.array (n, ~1)
        val parentGen = Array.array (n, ~1)

        (* Simple queue implemented as a ring-less array with head/tail. *)
        val q = Array.array (n, 0)
        val head = ref 0
        val tail = ref 0

        fun enqueue v =
          (Array.update (q, !tail, v); tail := !tail + 1)

        fun dequeue () =
          let
            val v = Array.sub (q, !head)
          in
            head := !head + 1;
            v
          end

        fun qEmpty () = !head >= !tail

        val () = Array.update (visited, x, true)
        val () = Array.update (parentPrev, x, x)
        val () = enqueue x

        fun stepFrom v =
          let
            fun loopS s =
              if s = r then
                ()
              else
                let
                  val u = AtlasFFI.atlas_kgb_cross (g, s, v)
                  val () =
                    if u < 0 then
                      raise Fail ("CrossWOrbit.cross_divide: kgb_cross failed: " ^ AtlasFFI.atlas_last_error ())
                    else
                      ()
                in
                  if Array.sub (visited, u) then
                    loopS (s + 1)
                  else
                    (Array.update (visited, u, true);
                     Array.update (parentPrev, u, v);
                     Array.update (parentGen, u, s);
                     enqueue u;
                     loopS (s + 1))
                end
          in
            loopS 0
          end

        fun bfs () =
          if qEmpty () then
            ()
          else
            let
              val v = dequeue ()
              val () = if v = y then () else stepFrom v
            in
              if v = y then () else bfs ()
            end

        val () = bfs ()

        val () =
          if not (Array.sub (visited, y)) then
            raise Fail "CrossWOrbit.cross_divide: did not find y in cross orbit"
          else
            ()

        (* Reconstruct by walking parents from y back to x. *)
        fun backtrack (v: int, accRev: int list) : int list =
          if v = x then
            accRev
          else
            let
              val p = Array.sub (parentPrev, v)
              val s = Array.sub (parentGen, v)
            in
              if p < 0 orelse s < 0 then
                raise Fail "CrossWOrbit.cross_divide: broken parent pointers"
              else
                backtrack (p, s :: accRev)
            end

        val gensApplied = backtrack (y, [])
        val word = List.rev gensApplied
      in
        word
      end
end

