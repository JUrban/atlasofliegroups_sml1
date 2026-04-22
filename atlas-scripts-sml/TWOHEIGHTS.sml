use "atlas-scripts-sml/BigRat.sml";
use "atlas-scripts-sml/Coordinates.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/RootDatum.sml";

(*
  File: atlas-scripts-sml/TWOHEIGHTS.sml

  Purpose
  - SML translation of `atlas-scripts/TWOHEIGHTS.at`.
  - Provides a canonical “height” functional on rational weights, normalized so
    that for a positive root it agrees with twice Bourbaki's height.

  Background (Atlas correspondence)
  - The `.at` definition:
      HT(rd, v) = 2 * (rd.rho_check) * dominant(rd, v)
    where `rho_check` is the half-sum of positive coroots and `*` denotes the
    natural pairing `X_* ⊗ X^* -> Z`.
  - Since `2*rho_check` is exactly the sum of positive coroots, we compute:
      HT(rd, v) = <two_rho_check, dominant(rd, v)>.

  Types
  - `ratvec` is `Lattice.ratvec = {den, nums}` representing `nums/den`.
  - `HT` returns `BigRat.t` to avoid overflow in intermediate pairings.

  Scope note
  - `TWOHEIGHTS.at` also defines parameter-level functions `HTT/HTA`; those
    depend on additional `Param` operations and are not yet included here.
*)

structure TWOHEIGHTS = struct
  type ratvec = Lattice.ratvec
  type rootdatum = RootDatum.t

  fun sumVecs (vs: int list list) : int list =
    (case vs of
       [] => []
     | v0 :: rest =>
         let
           val n = length v0
           val () = if List.all (fn v => length v = n) rest then () else raise Fail "TWOHEIGHTS.sumVecs: ragged"
           fun add2 (xs, ys) = ListPair.mapEq (op +) (xs, ys)
         in
           List.foldl add2 v0 rest
         end)

  (* `two_rho_check` as an integer coweight: sum of positive coroots. *)
  fun two_rho_check (rd: rootdatum) : int list =
    sumVecs (RootDatum.posCorootsCols rd)

  (* Make a weight dominant, via C++ `make_dominant`. *)
  fun dominant (rd: rootdatum, v: ratvec) : ratvec =
    let
      val txt = RootDatum.ratvecToText v
      val out = RootDatum.makeDominantRatWeightText rd txt
      val p = Coordinates.parseRatWeightText out
    in
      Lattice.ratvecNormalize {den = #den p, nums = #nums p}
    end

  (* Pair an integer coweight with a rational weight, producing a rational scalar. *)
  fun pairing (coweight: int list, w: ratvec) : BigRat.t =
    let
      val nums = #nums w
      val den = #den w
      val () = if length coweight = length nums then () else raise Fail "TWOHEIGHTS.pairing: length mismatch"
      val s =
        List.foldl (op +) (0:IntInf.int)
          (ListPair.mapEq (fn (a, b) => IntInf.fromInt a * IntInf.fromInt b) (coweight, nums))
    in
      BigRat.normalize {num = s, den = IntInf.fromInt den}
    end

  (* Main height functional. *)
  fun HT (rd: rootdatum, v: ratvec) : BigRat.t =
    pairing (two_rho_check rd, dominant (rd, v))

  (* Convenience: return `SOME n` if `HT(rd,v)` is an integer fitting in `int`. *)
  fun HT_int (rd: rootdatum, v: ratvec) : int option =
    let
      val q = HT (rd, v)
      val q = BigRat.normalize q
    in
      if #den q <> 1 then NONE else SOME (IntInf.toInt (#num q)) handle _ => NONE
    end
end

