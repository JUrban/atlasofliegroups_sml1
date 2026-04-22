use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/parabolics.sml

  Purpose
  - Partial SML analogue of `atlas-scripts/parabolics.at` (and a few helpers
    from `basic.at`) focused on the “KGB graph” operations and theta-stable
    parabolic enumeration needed by `induction.sml`.

  Key data types (SML-side)
  - `kgbelt` is an integer index into the KGB set of a real form `g`.
  - `parabolic` is represented as `(S,x)` where `S` is a list of simple-root
    indices and `x` is a KGB index (typically canonical/maximal in its class).

  Atlas correspondence
  - `ascents`, `down_neighbors`, `maximal`, `x_min`, `is_closed` mirror the
    corresponding functions in `parabolics.at`.
  - `twist` mirrors `basic.at`’s `twist(InnerClass)` computed from the
    distinguished involution acting on simple roots.
  - `theta_stable_parabolics_*` mirror `parabolics.at`’s theta-stable parabolic
    generation, using `distinguished_fiber` (the length-0 KGB elements).

  Performance notes
  - For the small ranks used so far (F4, G2), brute-force enumeration of
    twist-stable subsets is inexpensive; it also avoids any `.at` evaluation.
*)
structure Parabolics = struct
  type group = AtlasFFI.group
  type kgbelt = int
  type parabolic = int list * kgbelt (* (S, x) *)

  (* `distinguished_fiber(G)` from `basic.at`: KGB elements of length 0, in
     increasing index order. *)
  fun distinguished_fiber (g: group) : kgbelt list =
    let
      val n = AtlasFFI.atlas_group_kgb_size g
      fun loop (i, acc) =
        if i >= n then
          List.rev acc
        else if AtlasFFI.atlas_kgb_length (g, i) = 0 then
          loop (i + 1, i :: acc)
        else
          loop (i + 1, acc)
    in
      loop (0, [])
    end

  (* `twist(G)` from `basic.at`: diagram automorphism induced by the
     distinguished involution. Returns a list `tw` of length `ssrank` such that
     `tw[i]` is the index of the simple root `delta(alpha_i)`. *)
  fun twist (g: group) : int list =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          raise Fail ("Parabolics.twist: rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val delta = IntMatrix.parseMatText (AtlasFFI.atlas_group_distinguished_involution_text g)
      val simples = RootDatum.simpleRootsCols rd
      val () = RootDatum.free rd

      fun indexOfSimple v =
        let
          fun loop ([], _) = raise Fail "Parabolics.twist: delta(simple_root) not a simple root"
            | loop (w :: ws, i) = if w = v then i else loop (ws, i + 1)
        in
          loop (simples, 0)
        end

      fun image alpha = IntMatrix.matVecMul (delta, alpha)
    in
      List.map (fn alpha => indexOfSimple (image alpha)) simples
    end

  (* Partition the simple indices into orbits of the involution `tw`, as in
     `parabolics.at`:
       - fixed points yield singleton parts `[s]`
       - 2-cycles yield parts `[s,t]` with `t>s`. *)
  fun twist_partition (tw: int list) : int list list =
    let
      val r = length tw
      fun nth i = List.nth (tw, i)
      fun one s =
        let
          val t = nth s
        in
          if t >= s then
            if t = s then SOME [s] else SOME [s, t]
          else
            NONE
        end
    in
      List.mapPartial one (List.tabulate (r, fn i => i))
    end

  (* Enumerate all subsets S of simple roots which are stable under the twist.
     Returned subsets are sorted. *)
  fun twist_stable_subsets (g: group) : int list list =
    let
      val part = twist_partition (twist g)
      fun flatten (xss: int list list) = List.concat xss
      fun mk (parts: int list list) = Sort.sort (op <=) (flatten parts)
    in
      List.map mk (Basic.power_set part)
    end

  (* Decide whether a subset `S` is stable under the twist permutation `tw`. *)
  fun is_twist_stable_subset (tw: int list) (S: int list) : bool =
    let
      val r = length tw
      val inS = Array.array (r, false)
      val () =
        List.app
          (fn s =>
            if 0 <= s andalso s < r then
              Array.update (inS, s, true)
            else
              raise Fail "Parabolics.is_twist_stable_subset: simple index oob")
          S
      fun mem s = Array.sub (inS, s)
    in
      List.all (fn s => mem (List.nth (tw, s))) S
    end

  (* Ascents of `x` by generators in `S`; duplicates are possible.
     This follows the `parabolics.at` convention: only `nc` (Cayley) and `C+`
     (cross) moves count as ascents. *)
  fun ascents (g: group) (S: int list, x: kgbelt) : kgbelt list =
    let
      fun one s =
        case AtlasFFI.atlas_kgb_status (g, s, x) of
          3 => [AtlasFFI.atlas_kgb_cayley (g, s, x)] (* nc *)
        | 4 => [AtlasFFI.atlas_kgb_cross (g, s, x)] (* C+ *)
        | _ => [] (* descents / not used *)
    in
      List.concat (List.map one S)
    end

  (* Descents/down-neighbors of `x` by generators in `S`; duplicates possible. *)
  fun down_neighbors (g: group) (S: int list, x: kgbelt) : kgbelt list =
    let
      fun one s =
        case AtlasFFI.atlas_kgb_status (g, s, x) of
          0 => [AtlasFFI.atlas_kgb_cross (g, s, x)] (* C- *)
        | 2 =>
            let
              val y = AtlasFFI.atlas_kgb_cayley (g, s, x)
            in
              [y, AtlasFFI.atlas_kgb_cross (g, s, y)]
            end
        | _ => []
    in
      List.concat (List.map one S)
    end

  (* Whether `x` is maximal in the partial order generated by `S`. *)
  fun is_maximal_for (g: group) (S: int list, x: kgbelt) : bool =
    null (ascents g (S, x))

  (* The unique maximal element in the `S`-equivalence class of `x`. *)
  fun maximal (g: group) (S: int list, x: kgbelt) : kgbelt =
    let
      fun loop x =
        case ascents g (S, x) of
          [] => x
        | y :: _ => loop y
    in
      loop x
    end

  (* Parabolic equivalence (K-orbit on G/P_S):
     `(S,x) = (T,y)` iff `S=T` and `maximal(S,x)=maximal(S,y)`.
     This mirrors `parabolics.at`’s `=` definition for `KGPElt`. *)
  fun eq (g: group) ((S, x): parabolic, (T, y): parabolic) : bool =
    S = T andalso maximal g (S, x) = maximal g (T, y)

  (* A (not necessarily unique) minimal element in the `S`-equivalence class. *)
  fun x_min (g: group) (S: int list, x: kgbelt) : kgbelt =
    let
      fun loop x =
        case down_neighbors g (S, x) of
          [] => x
        | y :: _ => loop y
    in
      loop x
    end

  (* Canonical representative of a parabolic orbit: the maximal KGB element. *)
  fun representative (g: group) (P: parabolic) : kgbelt =
    maximal g P

  (* Closed orbit test: length of the minimal representative is 0. *)
  fun is_closed (g: group) (P as (S, x): parabolic) : bool =
    AtlasFFI.atlas_kgb_length (g, x_min g (S, x)) = 0

  (* `theta_stable_parabolics(G,(rd,S))` from `parabolics.at` (type-level
     variant): for twist-stable `S`, return parabolics `(S,maximal(S,x))` for
     all `x` in the distinguished fiber, with duplicates removed. *)
  fun theta_stable_parabolics_of_type (g: group) (S: int list) : parabolic list =
    let
      val tw = twist g
    in
      if not (is_twist_stable_subset tw S) then
        []
      else
        let
          val xs = distinguished_fiber g
          val ys = List.map (fn x0 => maximal g (S, x0)) xs
          val ysU = Sort.sort_u (op <=) ys
        in
          List.map (fn y => (S, y)) ysU
        end
    end

  (* `theta_stable_parabolics(G)` from `parabolics.at` (all types). *)
  fun theta_stable_parabolics (g: group) : parabolic list =
    List.concat (List.map (theta_stable_parabolics_of_type g) (twist_stable_subsets g))

  (* `theta_stable_parabolics_with(x)` analog from `induction.at`, but computed directly.
     Returns those theta-stable parabolics `P` for which `(S,x)=(S,representative(P))`. *)
  fun theta_stable_parabolics_with (g: group) (x: kgbelt) : parabolic list =
    let
      val tsp = theta_stable_parabolics g
      fun keep (P as (S, y)) = eq g ((S, x), P)
    in
      List.filter keep tsp
    end
end
