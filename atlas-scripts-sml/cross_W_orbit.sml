use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/cross_W_orbit.sml

  Purpose
  - Partial SML translation of `atlas-scripts/cross_W_orbit.at`, focused on the
    `cross_divide` operation, plus a couple of orbit helpers:
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

  (* Run a BFS in the KGB cross graph starting at `x0`, returning parent pointers.

     For each visited vertex `v` (including `x0`):
       - `parentPrev[v]` is the predecessor along the BFS tree.
       - `parentGen[v]` is the simple generator used for the edge:
             v = cross(parentGen[v], parentPrev[v])
       - `visited[v]` is true.

     We set `parentPrev[x0]=x0` and `parentGen[x0]=~1`.
  *)
  fun bfsParents (g: group, x0: int) : bool array * int array * int array =
    let
      val n = AtlasFFI.atlas_group_kgb_size g
      val r = AtlasFFI.atlas_group_semisimple_rank g
      val () =
        if x0 < 0 orelse x0 >= n then
          raise Fail "CrossWOrbit.bfsParents: KGB index out of range"
        else
          ()

      val visited = Array.array (n, false)
      val parentPrev = Array.array (n, ~1)
      val parentGen = Array.array (n, ~1)

      val q = Array.array (n, 0)
      val head = ref 0
      val tail = ref 0

      fun enqueue v = (Array.update (q, !tail, v); tail := !tail + 1)
      fun dequeue () = (head := !head + 1; Array.sub (q, !head - 1))
      fun qEmpty () = !head >= !tail

      val () = Array.update (visited, x0, true)
      val () = Array.update (parentPrev, x0, x0)
      val () = Array.update (parentGen, x0, ~1)
      val () = enqueue x0

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
                    raise Fail ("CrossWOrbit.bfsParents: kgb_cross failed: " ^ AtlasFFI.atlas_last_error ())
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
            val () = stepFrom v
          in
            bfs ()
          end
    in
      bfs ();
      (visited, parentPrev, parentGen)
    end

  (* Reconstruct a witness word from BFS parent pointers. *)
  fun wordFromParents (x0: int, v: int, visited: bool array, parentPrev: int array, parentGen: int array) : weyl_word =
    if not (Array.sub (visited, v)) then
      raise Fail "CrossWOrbit.wordFromParents: vertex not visited"
    else if v = x0 then
      []
    else
      let
        fun backtrack (cur: int, accRev: int list) : int list =
          if cur = x0 then
            accRev
          else
            let
              val p = Array.sub (parentPrev, cur)
              val s = Array.sub (parentGen, cur)
            in
              if p < 0 orelse s < 0 then
                raise Fail "CrossWOrbit.wordFromParents: broken parent pointers"
              else
                backtrack (p, s :: accRev)
            end

        val gensApplied = backtrack (v, [])
        val word = List.rev gensApplied
      in
        word
      end

  (* Find a word `w` with `y = cross(w, x)`; raises if not found. *)
  fun cross_divide (g: group, y: int, x: int) : weyl_word =
    if y = x then
      []
    else
      let
        val () =
          if x < 0 orelse x >= AtlasFFI.atlas_group_kgb_size g orelse y < 0 orelse y >= AtlasFFI.atlas_group_kgb_size g then
            raise Fail "CrossWOrbit.cross_divide: KGB index out of range"
          else
            ()

        val (visited, parentPrev, parentGen) = bfsParents (g, x)

        val () =
          if not (Array.sub (visited, y)) then
            raise Fail "CrossWOrbit.cross_divide: did not find y in cross orbit"
          else
            ()
      in
        wordFromParents (x, y, visited, parentPrev, parentGen)
      end

  (* Orbit enumerator: returns `[(z,witness_w)]` for every KGB element z in the
     cross orbit of `x`, sorted by increasing KGB index. *)
  fun cross_orbit (g: group, x: int) : (int * weyl_word) list =
    let
      val n = AtlasFFI.atlas_group_kgb_size g
      val (visited, parentPrev, parentGen) = bfsParents (g, x)
      val zs = List.filter (fn i => Array.sub (visited, i)) (List.tabulate (n, fn i => i))
      fun mk z = (z, wordFromParents (x, z, visited, parentPrev, parentGen))
      val orbit = List.map mk zs
    in
      Basic.sort_by (#1, op <=) orbit
    end

  (* Predicate+Witness: returns a function matching `.at`’s `is_in_cross_orbit`.
     If `y` is not in the orbit, returns `(false, [])`. *)
  fun is_in_cross_orbit (g: group, x: int) : int -> bool * weyl_word =
    let
      val orbit = cross_orbit (g, x)
      val keys = List.map #1 orbit
      fun witnessAt i = #2 (List.nth (orbit, i))

      fun pred y =
        let
          val n = length keys
          fun at i = List.nth (keys, i)
          val k = Basic.binary_search_first (fn j => y <= at j, 0, n)
        in
          if k < n andalso at k = y then (true, witnessAt k) else (false, [])
        end
    in
      pred
    end
end
