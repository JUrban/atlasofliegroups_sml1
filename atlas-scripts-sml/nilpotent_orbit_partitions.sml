use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/partitions.sml";
use "atlas-scripts-sml/combinatorics.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/nilpotent_orbit_partitions.sml

  Purpose
  - Partial SML port of `atlas-scripts/nilpotent_orbit_partitions.at`.

  What this file provides today (pure combinatorics)
  - Partition classification of complex nilpotent orbits in simple classical
    types A–D:
      `nilpotent_orbit_partitions : LieType.t -> partition list`
      `nilpotent_orbit_partitions_rd : RootDatum.t -> partition list`
  - Orbit closure order (dominance order) on partitions:
      `closures : partition list -> int list list`
  - Classical “semisimple element” vectors derived from partitions and type:
      `semisimple_element : LieType.t * partition -> int list`

  What is not yet ported
  - The Atlas-level `ComplexNilpotent` type and orbit constructors
    (`complex_nilpotent_from_partition`, `dim_nilpotent_partition`, etc.).
  - Dominance normalization against a specific root datum (the `.at` version
    calls `dominant(.,rd)` in standard coordinates).

  Status
  - Incremental port: enough to support scripts that only need the partition
    layer.
*)

structure Nilpotent_orbit_partitions = struct
  structure P = Partitions
  structure Cb = Combinatorics

  type partition = P.partition
  type LieType = LieType.t
  type RootDatum = RootDatum.t

  fun fail msg = raise Fail ("Nilpotent_orbit_partitions: " ^ msg)

  (* Utility to convert type letter A–G to an index 0–6; pure torus -> 7.
     Port of `type_number(LieType)` from `.at`. *)
  fun type_number (t: LieType) : int =
    (case t of
       [] => 7
     | [(c, _)] => Char.ord c - Char.ord #"A"
     | _ => raise Fail "Nilpotent_orbit_partitions.type_number: non-simple LieType")

  fun type_number_rd (rd: RootDatum) : int =
    type_number (RootDatum.lieType rd)

  val C_partitions = Cb.parity_restricted_partitions true  (* odd parts even multiplicity *)
  val BD_partitions = Cb.parity_restricted_partitions false (* even parts even multiplicity *)

  (* Partitions classifying complex nilpotent orbits, for simple types A–D. *)
  fun nilpotent_orbit_partitions (t: LieType) : partition list =
    let
      val n = LieType.semisimple_rank t
    in
      case type_number t of
        0 => P.partitions (n + 1)
      | 1 => BD_partitions (2 * n + 1)
      | 2 => C_partitions (2 * n)
      | 3 => BD_partitions (2 * n)
      | _ => raise Fail "Nilpotent_orbit_partitions.nilpotent_orbit_partitions: exceptional type"
    end

  fun nilpotent_orbit_partitions_rd (rd: RootDatum) : partition list =
    nilpotent_orbit_partitions (RootDatum.lieType rd)

  (* Nilpotent partitions of the dual group, for simple types A–D. *)
  fun dual_nilpotent_orbit_partitions_rd (rd: RootDatum) : partition list =
    let
      val drd = RootDatum.dual rd
      val out = nilpotent_orbit_partitions_rd drd
      val () = RootDatum.free drd
    in
      out
    end

  (* Tools for computing h(O^vee): apply to rows of Young diagram. *)
  fun tworho (n: int) : int list =
    if n < 0 then fail "tworho: n<0"
    else List.tabulate (n, fn k => (n - 1) - 2 * k)

  fun string_partition (n: int) : partition =
    if n < 0 then fail "string_partition: n<0"
    else Cb.strip_to_partition (List.tabulate (n div 2, fn k => (n - 1) - 2 * k))

  (* Compute 1/2 h(O^vee) in standard classical coordinates, for types A–D.
     Port of `.at` `semisimple_element(LieType,Partition)`, but kept purely
     combinatorial (no root-datum dominance normalization). *)
  fun semisimple_element (t: LieType, p0: partition) : int list =
    let
      val p = Cb.strip_to_partition p0
    in
      if type_number t = 0 then
        List.concat (List.map tworho p)
      else
        let
          val strs = List.concat (List.map string_partition p)
          val pad = (Cb.sumInts p) div 2 - length strs
          val () = if pad >= 0 then () else fail "semisimple_element: negative pad"
        in
          strs @ List.tabulate (pad, fn _ => 0)
        end
    end

  fun nilpotent_orbit_semisimple_elements (t: LieType) : int list list =
    List.map (fn p => semisimple_element (t, p)) (nilpotent_orbit_partitions t)

  fun nilpotent_orbit_semisimple_elements_rd (rd: RootDatum) : int list list =
    nilpotent_orbit_semisimple_elements (RootDatum.lieType rd)

  (* Orbits = partitions. `closures(orbits)[i]` is the set of j (j≠i) such that
     `orbits[j]` is contained in the closure of `orbits[i]` (dominance order). *)
  fun closures (orbits0: partition list) : int list list =
    let
      val orbits = List.map Cb.strip_to_partition orbits0
      fun rel (p, q) = Cb.dominance_leq_partitions (p, q)
      val n = length orbits
      val indexed = ListPair.zipEq (orbits, List.tabulate (n, fn k => k))
        handle ListPair.UnequalLengths => fail "closures: internal zip mismatch"
      fun row (p, i) =
        List.foldr
          (fn ((q, j), acc) =>
             if i <> j andalso rel (q, p) then j :: acc else acc)
          []
          indexed
    in
      List.map row indexed
    end

  (* TODO
     - Add `ComplexNilpotent` constructors and dimension computations once the
       nilpotent-orbit layer is exposed via `AtlasFFI`. *)
end
