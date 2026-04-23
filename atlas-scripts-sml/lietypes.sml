use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/simple_factors.sml";

(*
  File: atlas-scripts-sml/lietypes.sml

  Purpose
  - SML port of `atlas-scripts/lietypes.at`.
  - Provides small Lie-type utilities used across many `.at` scripts:
      - canonicalization (`to_canonical`) for comparisons
      - isomorphism predicates (`is_isomorphic`, `is_locally_isomorphic`)
      - a compact pretty-printer (`nice_format`)
      - a `Cartan_matrix_type` wrapper for `RootDatum`
      - helpers for scripts that expect "simple type" (`simple_type`)

  Representation
  - We use `LieType.t = (char * int) list` from `atlas-scripts-sml/LieType.sml`.
  - By convention, a central torus factor is represented as `(#"T", k)` where
    `k` is the torus rank; semisimple factors are the usual A/B/C/D/E/F/G.

  Atlas correspondence and caveats
  - The `.at` environment treats `C2` and `B2` as interchangeable for some
    comparisons; `to_canonical` follows `lietypes.at` and maps `C2 -> B2`.
  - In `.at`, `nice_format` is primarily a diagnostic printer; it sorts and
    groups semisimple factors, and prints the central torus as `kT1` when
    present.
*)

structure LieTypes = struct
  type LieType = LieType.t
  type RootDatum = RootDatum.t

  (*
    Constants mirroring the `set A1="A1"` block in `lietypes.at`.

    In SML we expose them as `LieType.t` values rather than strings.
  *)
  fun simple (ty: char, r: int) : LieType = [(ty, r)]

  val A1 = simple (#"A", 1)
  val A2 = simple (#"A", 2)
  val A3 = simple (#"A", 3)
  val A4 = simple (#"A", 4)
  val A5 = simple (#"A", 5)
  val A6 = simple (#"A", 6)
  val A7 = simple (#"A", 7)
  val A8 = simple (#"A", 8)

  val B2 = simple (#"B", 2)
  val B3 = simple (#"B", 3)
  val B4 = simple (#"B", 4)
  val B5 = simple (#"B", 5)
  val B6 = simple (#"B", 6)
  val B7 = simple (#"B", 7)
  val B8 = simple (#"B", 8)

  val C2 = simple (#"C", 2)
  val C3 = simple (#"C", 3)
  val C4 = simple (#"C", 4)
  val C5 = simple (#"C", 5)
  val C6 = simple (#"C", 6)
  val C7 = simple (#"C", 7)
  val C8 = simple (#"C", 8)

  val D2 = simple (#"D", 2)
  val D3 = simple (#"D", 3)
  val D4 = simple (#"D", 4)
  val D5 = simple (#"D", 5)
  val D6 = simple (#"D", 6)
  val D7 = simple (#"D", 7)
  val D8 = simple (#"D", 8)

  val E6 = simple (#"E", 6)
  val E7 = simple (#"E", 7)
  val E8 = simple (#"E", 8)
  val F4 = simple (#"F", 4)
  val G2 = simple (#"G", 2)

  (* Ordering used by `lietypes.at`: lexicographic by letter, then rank. *)
  fun leqSimpleFactor ((t0, r0): char * int, (t1, r1): char * int) : bool =
    if t0 <> t1 then Char.ord t0 <= Char.ord t1 else r0 <= r1

  fun eqSimpleFactor (a, b) = leqSimpleFactor (a, b) andalso leqSimpleFactor (b, a)

  (*
    `.at`: `central_torus_rank(lt)`

    Sum all `T`-factors. We allow multiple such factors defensively.
  *)
  fun central_torus_rank (lt: LieType) : int =
    List.foldl (fn ((ty, r), acc) => if ty = #"T" then acc + r else acc) 0 lt

  (*
    `.at`: `simple_factors(lt)` (semisimple factors only).
  *)
  fun simple_factors (lt: LieType) : (char * int) list =
    List.filter (fn (ty, _) => ty <> #"T") lt

  (*
    `.at`: `to_canonical(lt)`:
      - sort factors
      - map C2 -> B2
      - keep the central torus rank (if any) as a single `T`-factor at the end
  *)
  fun to_canonical (lt: LieType) : LieType =
    let
      fun canon ((ty, r): char * int) : char * int =
        if ty = #"C" andalso r = 2 then (#"B", 2) else (ty, r)

      val semisimple = Basic.sort leqSimpleFactor (List.map canon (simple_factors lt))
      val t = central_torus_rank lt
    in
      if t = 0 then semisimple else semisimple @ [(#"T", t)]
    end

  (* `.at`: `standardize_Lie_type` (backward compatibility). *)
  val standardize_Lie_type = to_canonical

  (* `.at`: `is_isomorphic(lt0,lt1)` compares canonical forms. *)
  fun is_isomorphic (lt0: LieType, lt1: LieType) : bool =
    to_canonical lt0 = to_canonical lt1

  (* `.at`: local isomorphism for complex groups via Lie-type isomorphism. *)
  fun is_locally_isomorphic (rd0: RootDatum, rd1: RootDatum) : bool =
    is_isomorphic (RootDatum.lieType rd0, RootDatum.lieType rd1)

  (*
    `.at`: `nice_format(lt)`

    Example outputs
    - `[(#"A",1),(#"A",1),(#"B",2)]` -> "2A1+B2"
    - `[(#"A",1),(#"T",3)]` -> "A1+3T1"
    - `[(#"T",2)]` -> "2T1"
    - `[]` -> "e"
  *)
  fun nice_format (lt: LieType) : string =
    let
      val ss = Basic.sort leqSimpleFactor (simple_factors lt)
      val t = central_torus_rank lt

      fun pick (ty, r) = String.str ty ^ Int.toString r

      fun groupEq [] = []
        | groupEq (x :: xs) =
            let
              fun take (cur, [], acc) = (List.rev acc, [])
                | take (cur, y :: ys, acc) =
                    if eqSimpleFactor (cur, y) then take (cur, ys, y :: acc) else (List.rev acc, y :: ys)
              val (ys, rest) = take (x, xs, [x])
            in
              ys :: groupEq rest
            end

      fun fmtGroup g =
        (case g of
           [] => raise Fail "LieTypes.nice_format: internal empty group"
         | x :: _ =>
             if length g = 1 then pick x else Int.toString (length g) ^ pick x)

      val semisimpleText =
        String.concatWith "+" (List.map fmtGroup (groupEq ss))

      val out =
        if t = 0 then
          semisimpleText
        else if semisimpleText = "" then
          Int.toString t ^ "T1"
        else
          semisimpleText ^ "+" ^ Int.toString t ^ "T1"
    in
      if semisimpleText = "" andalso t = 0 then "e" else out
    end

  (* `.at` overload: `nice_format(rd)` *)
  fun nice_format_rd (rd: RootDatum) : string =
    nice_format (RootDatum.lieType rd)

  (* `.at`: `Cartan_matrix_type(rd)` (delegates to Cartan-matrix helper). *)
  fun Cartan_matrix_type (rd: RootDatum) : LieType * int list =
    SimpleFactors.cartan_matrix_type (RootDatum.cartanMatrix rd)

  (*
    `.at`: `simple_type(lt)`

    Requires the semisimple part to be a single factor and the central torus
    rank to be 0.
  *)
  fun simple_type (lt: LieType) : char * int =
    let
      val ss = simple_factors lt
      val t = central_torus_rank lt
    in
      if length ss = 1 andalso t = 0 then List.hd ss
      else raise Fail "LieTypes.simple_type: LieType is not simple"
    end

  (* `.at` overload: `simple_type(rd)` *)
  fun simple_type_rd (rd: RootDatum) : char * int =
    simple_type (RootDatum.lieType rd)
end
