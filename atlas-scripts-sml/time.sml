use "atlas-scripts-sml/tabulate.sml";

(*
  File: atlas-scripts-sml/time.sml

  Purpose
  - SML analogue of `atlas-scripts/time.at`.
  - Provides:
      - `elapsed_ms()` since this module was loaded
      - `print_time_string(ms)` formatting
      - `general_time` and `time_verbose` globals (as refs)

  Notes
  - The `.at` interpreter’s `elapsed_ms()` is session-relative; we emulate this
    with a `Timer` started at module load.
  - We avoid naming the structure `Time` to not collide with the Basis `Time`.
*)

structure TimeAT = struct
  val timer0 = Timer.startRealTimer ()

  fun elapsed_ms () : int =
    let
      val dt = Timer.checkRealTimer timer0
      val ms = Time.toMilliseconds dt
    in
      LargeInt.toInt ms
    end

  fun pad3 (s: string) : string =
    if String.size s >= 3 then s
    else String.implode (List.tabulate (3 - String.size s, fn _ => #"0")) ^ s

  (* Convert elapsed ms to a human-readable `X.YYYsec` form, matching `time.at`. *)
  fun print_time_string (n: int) : string =
    let
      val secs = n div 1000
      val ms = n mod 1000
    in
      Int.toString secs ^ "." ^ pad3 (Int.toString ms) ^ "sec"
    end

  val general_time : int ref = ref 0
  val time_verbose : bool ref = ref true
end
