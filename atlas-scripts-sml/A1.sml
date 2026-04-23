use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/groups_at.sml";
use "atlas-scripts-sml/group_operations.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/A1.sml

  Purpose
  - Partial SML port of `atlas-scripts/A1.at`.
  - Provides small constructors for root data built from products of A1 factors
    and central tori, plus the helper for encoding (Z/2)^k central subgroups as
    rational coweights.

  What is implemented
  - `A1_Lie_type(nA1,nT)` as a `LieType.t` description (used for diagnostics).
  - `A1_root_datum(nA1,nT)` as the simply-connected semisimple part (`A1^nA1`)
    crossed with a torus of rank `nT`.
  - `central_subgroup_as_ratvec`: convert binary vectors to `1/2`-valued
    rational coweights.

  Not yet implemented
  - Quotient root data by a specified central subgroup (the
    `root_datum(Lie_type,center)` constructor used in `.at`), and all inner
    class / twisting constructors. These require a more complete SML-facing
    root-datum quotient API than currently exposed.
*)

structure A1 = struct
  type lieType = LieType.t
  type rootdatum = RootDatum.t
  type vec = int list
  type ratvec = Lattice.ratvec

  fun requireNonNeg (name: string, n: int) =
    if n < 0 then raise Fail ("A1." ^ name ^ ": expected nonnegative int") else ()

  (* `.at`: A1_Lie_type(n_A1_factors, n_torus_factors). *)
  fun A1_Lie_type (nA1: int, nT: int) : lieType =
    let
      val () = requireNonNeg ("A1_Lie_type", nA1)
      val () = requireNonNeg ("A1_Lie_type", nT)
      val as_ = List.tabulate (nA1, fn _ => (#"A", 1))
    in
      if nT = 0 then as_ else as_ @ [(#"T", nT)]
    end

  (* Simply connected (cross torus): `A1^nA1 × T^{nT}`. *)
  fun A1_root_datum (nA1: int, nT: int) : rootdatum =
    let
      val () = requireNonNeg ("A1_root_datum", nA1)
      val () = requireNonNeg ("A1_root_datum", nT)

      fun semisimple () : rootdatum option =
        if nA1 = 0 then NONE
        else
          let
            val rd1 = RootDatum.newSimple (#"A", 1, false)
            fun loop (0, acc) = acc
              | loop (k, acc) =
                  let
                    val rdA1 = RootDatum.newSimple (#"A", 1, false)
                    val prod = GroupOperations.* (acc, rdA1)
                    val () = RootDatum.free acc
                    val () = RootDatum.free rdA1
                  in
                    loop (k - 1, prod)
                  end
          in
            SOME (loop (nA1 - 1, rd1))
          end

      val tor = GroupsAT.torus_datum nT
    in
      case semisimple () of
        NONE => tor
      | SOME ss =>
          let
            val prod = GroupOperations.* (ss, tor)
            val () = RootDatum.free ss
            val () = RootDatum.free tor
          in
            prod
          end
    end

  (* `.at`: central_subgroup_as_ratvec([vec] center) = for v in center do v/2%1 od *)
  fun central_subgroup_as_ratvec (center: vec list) : ratvec list =
    List.map (fn v => Lattice.ratvecNormalize {den = 2, nums = v}) center
end

