use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/powerTwo.sml

  Purpose
  - SML translation of `atlas-scripts/powerTwo.at`.
  - Provides a small “power function” factory for fast repeated exponentiation
    by caching the powers `base^(2^j)` and multiplying those corresponding to
    the 1-bits of an exponent.

  API (mirrors the `.at` record-ish `Power_function`)
  - `to_binary n`:
      - returns the bits of `n` in least-significant-bit-first order
        (bit 0 at index 0), with no leading zeros; `to_binary 0 = []`.
  - `make_power_function base` returns a record with:
      - `base()`                     : the base
      - `largest_power_now_known()`  : number of cached `2^j` powers
      - `clear()`                    : drop cached powers (keeps ability to rebuild)
      - `power n`                    : compute `base^n` (for `n >= 0`)

  Notes
  - Uses `int` arithmetic like the original `.at` script; it may overflow.
  - Intended as a dependency shim for translated scripts; not performance-tuned.
*)

structure PowerTwo = struct
  (* Binary digits of a nonnegative integer, LSB-first. *)
  fun to_binary (n: int) : int list =
    if n < 0 then
      raise Fail "PowerTwo.to_binary: n < 0"
    else
      let
        fun loop (m, acc) =
          if m = 0 then acc
          else loop (m div 2, (m mod 2) :: acc)
        (* The `.at` version appends bits as it goes, yielding LSB-first order;
           we reproduce that by building MSB-first and reversing once. *)
        val msbFirst = loop (n, [])
      in
        List.rev msbFirst
      end

  type power_function =
    { base: unit -> int
    , largest_power_now_known: unit -> int
    , clear: unit -> unit
    , power: int -> int
    }

  fun make_power_function (base0: int) : power_function =
    let
      val base = base0
      val basetwopowers : int list ref = ref [base] (* jth entry is base^(2^j) *)

      fun ensureBase () =
        (case !basetwopowers of
           [] => basetwopowers := [base]
         | _ => ())

      fun add_power (j: int) : unit =
        let
          val () = ensureBase ()
          fun extend () =
            let
              val xs = !basetwopowers
            in
              (case List.rev xs of
                 [] => raise Fail "PowerTwo.add_power: impossible"
               | last :: _ => basetwopowers := xs @ [last * last])
            end
          fun loop () =
            if length (!basetwopowers) > j then () else (extend (); loop ())
        in
          if j < 0 then raise Fail "PowerTwo.add_power: j < 0" else loop ()
        end

      fun clear () : unit = basetwopowers := []

      fun power (n: int) : int =
        if n < 0 then
          raise Fail "PowerTwo.power: n < 0"
        else
          let
            val bits = to_binary n
            val log = length bits
            val () = add_power log
            val xs = !basetwopowers
            fun pick (bit, j, acc) =
              if bit = 0 then acc else List.nth (xs, j) * acc
            fun loop ([], _, acc) = acc
              | loop (b :: bs, j, acc) = loop (bs, j + 1, pick (b, j, acc))
          in
            loop (bits, 0, 1)
          end
    in
      { base = (fn () => base)
      , largest_power_now_known = (fn () => length (!basetwopowers))
      , clear = clear
      , power = power
      }
    end
end

