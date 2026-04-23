use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/generate_groups.sml

  Purpose
  - SML translation of `atlas-scripts/generate_groups.at`.
  - Provides simple enumeration utilities for “all simple groups” in a given
    rank range, parameterized by:
      - isogeny: simply connected vs adjoint
      - inner class letter: e.g. "e" (equal rank) or "u" (unequal rank)
      - real form number within that inner class

  Differences from `.at`
  - The Atlas interpreter’s `RealForm` is a rich value with many methods.
    In this SML port we represent a “real form” by an `AtlasFFI.group` handle.
  - Ownership matters: each created group handle must be freed with
    `AtlasFFI.atlas_group_free` when no longer needed.

  FFI dependency
  - Uses `atlas_group_new_simple_isogeny(type,rank,ic,rf,isog)` from the SML
    FFI layer; internally this calls a text-based C++ shim export so that the
    Poly/ML FFI never has to call a 4/5-argument foreign function directly.
*)

structure GenerateGroups = struct
  type group = AtlasFFI.group

  datatype isogeny = SC | AD

  fun isogenyToChar SC = #"s"
    | isogenyToChar AD = #"a"

  fun parseTypeAndRank (s: string) : char * int =
    if String.size s < 2 then
      raise Fail "GenerateGroups.parseTypeAndRank: expected like \"A2\""
    else
      let
        val typeLetter = String.sub (s, 0)
        val rankText = String.extract (s, 1, NONE)
      in
        case Int.fromString rankText of
          SOME r => (typeLetter, r)
        | NONE => raise Fail ("GenerateGroups.parseTypeAndRank: bad rank in " ^ s)
      end

  (* Port of `all_simple_root_systems_given_rank`. *)
  fun all_simple_root_systems_given_rank (rank: int) : string list =
    if rank > 8 orelse rank = 5 then
      ["A" ^ Int.toString rank, "B" ^ Int.toString rank, "C" ^ Int.toString rank, "D" ^ Int.toString rank]
    else if rank > 5 then
      [ "A" ^ Int.toString rank
      , "B" ^ Int.toString rank
      , "C" ^ Int.toString rank
      , "D" ^ Int.toString rank
      , "E" ^ Int.toString rank
      ]
    else
      let
        val types =
          [ []
          , ["A"]
          , ["A", "B", "G"]
          , ["A", "B", "C"]
          , ["A", "B", "C", "D", "F"]
          ]
        val ts = List.nth (types, rank)
      in
        List.map (fn t => t ^ Int.toString rank) ts
      end

  (* Port of `all_inner_classes`. *)
  fun all_inner_classes (typeAndRank: string) : string list =
    let
      val (ty, r) = parseTypeAndRank typeAndRank
    in
      if (ty = #"A" andalso r <> 1) orelse ty = #"D" orelse (ty = #"E" andalso r = 6) then
        ["e", "u"]
      else
        ["e"]
    end

  (* Construct a group handle; raises Fail on FFI error. *)
  fun new_simple (iso: isogeny, typeLetter: char, rank: int, innerClassLetter: char, rf: int) : group =
    let
      val g = AtlasFFI.atlas_group_new_simple_isogeny (typeLetter, rank, innerClassLetter, rf, isogenyToChar iso)
    in
      if g = Foreign.Memory.null then
        raise Fail ("GenerateGroups.new_simple: " ^ AtlasFFI.atlas_last_error ())
      else
        g
    end

  (* Return the number of real forms for this (type,rank,inner class,isogeny). *)
  fun num_real_forms (iso: isogeny, typeLetter: char, rank: int, innerClassLetter: char) : int =
    let
      val g0 = new_simple (iso, typeLetter, rank, innerClassLetter, 0)
      val nrf = AtlasFFI.atlas_group_num_real_forms g0
      val () = AtlasFFI.atlas_group_free g0
    in
      if nrf < 0 then
        raise Fail ("GenerateGroups.num_real_forms: " ^ AtlasFFI.atlas_last_error ())
      else
        nrf
    end

  (* Enumerate all simple groups of a given rank, returning owned group handles.
     Caller must free each with `atlas_group_free`. *)
  fun all_simple_given_isogeny_and_rank (iso: isogeny, rank: int) : group list =
    let
      val rootsystems = all_simple_root_systems_given_rank rank

      fun oneRootSystem rs =
        let
          val (ty, r) = parseTypeAndRank rs
          val innerClasses = all_inner_classes rs

          fun oneIC icStr =
            let
              val ic = String.sub (icStr, 0)
              val nrf = num_real_forms (iso, ty, r, ic)
            in
              List.tabulate (nrf, fn rf => new_simple (iso, ty, r, ic, rf))
            end
        in
          List.concat (List.map oneIC innerClasses)
        end
    in
      List.concat (List.map oneRootSystem rootsystems)
    end

  (* Convenience synonym as in `.at`. *)
  val all_simple = all_simple_given_isogeny_and_rank

  (* Resource-safe iterator that avoids accumulating group handles in memory. *)
  fun app_all_simple_given_isogeny_and_rank (iso: isogeny, rank: int, f: group -> unit) : unit =
    let
      val rootsystems = all_simple_root_systems_given_rank rank

      fun forRF (ty, r, ic, rf) =
        let
          val g = new_simple (iso, ty, r, ic, rf)
        in
          (f g; AtlasFFI.atlas_group_free g) handle e => (AtlasFFI.atlas_group_free g; raise e)
        end

      fun oneRootSystem rs =
        let
          val (ty, r) = parseTypeAndRank rs
          val innerClasses = all_inner_classes rs
          fun oneIC icStr =
            let
              val ic = String.sub (icStr, 0)
              val nrf = num_real_forms (iso, ty, r, ic)
            in
              List.app (fn rf => forRF (ty, r, ic, rf)) (List.tabulate (nrf, fn i => i))
            end
        in
          List.app oneIC innerClasses
        end
    in
      List.app oneRootSystem rootsystems
    end
end
