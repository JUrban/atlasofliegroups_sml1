use "atlas-scripts-sml/Rat.sml";

(*
  File: atlas-scripts-sml/log.sml

  Purpose
  - SML translation of `atlas-scripts/log.at`.
  - Implements a small collection of integer / rational “discrete log” helpers:
    `max_power` (largest `k` with `b^k <= n`), plus `rounded_log_2`.

  Notes
  - Atlas `.at` uses a built-in exact rational type `rat`. In this port we use
    `Rat.rat` for inputs, but compute floors via `IntInf` to avoid intermediate
    overflow where possible.
*)

structure Log = struct
  (* Largest `k` such that `b^k <= n`, with `n >= 0` and `b >= 2`. *)
  fun max_power_int (n: int, b: int) : int =
    if b <= 1 then
      raise Fail "Log.max_power_int: expected b>=2"
    else if n < 0 then
      raise Fail "Log.max_power_int: expected n>=0"
    else
      let
        fun loop (m, k) =
          if m < b then k else loop (m div b, k + 1)
      in
        loop (n, 0)
      end

  fun floorRatToIntInf (r: Rat.rat) : IntInf.int =
    let
      val r = Rat.normalize r
      val n = IntInf.fromInt (#num r)
      val d = IntInf.fromInt (#den r) (* positive *)
    in
      if n >= 0 then
        IntInf.div (n, d)
      else
        ~ (IntInf.div (~n + d - 1, d))
    end

  fun max_power_intinf (n: IntInf.int, b: int) : int =
    if b <= 1 then
      raise Fail "Log.max_power_intinf: expected b>=2"
    else if n < 0 then
      raise Fail "Log.max_power_intinf: expected n>=0"
    else
      let
        val bb = IntInf.fromInt b
        fun loop (m: IntInf.int, k: int) =
          if m < bb then k else loop (IntInf.div (m, bb), k + 1)
      in
        loop (n, 0)
      end

  (* Atlas: `max_power(rat r,int b) = max_power(floor(r),b)`. *)
  fun max_power_rat (r: Rat.rat, b: int) : int =
    max_power_intinf (floorRatToIntInf r, b)

  (* `.at` alias: `log_b_int = max_power@(rat,int)`. *)
  val log_b_int = max_power_rat

  (* Atlas: `rounded_log_2(r) = (max_power(r^2,2)+1)\\2`. *)
  fun rounded_log_2 (r: Rat.rat) : int =
    let
      val r = Rat.normalize r
      val n = IntInf.fromInt (#num r)
      val d = IntInf.fromInt (#den r)
      val n2 = n * n
      val d2 = d * d
      val floorSq = IntInf.div (n2, d2) (* `r^2` is nonnegative *)
      val k = max_power_intinf (floorSq, 2)
    in
      (k + 1) div 2
    end

  (* Compatibility names mirroring the `.at` script. *)
  val max_power = max_power_int
end
