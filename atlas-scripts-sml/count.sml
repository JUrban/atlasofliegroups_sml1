(*
  File: atlas-scripts-sml/count.sml

  Purpose
  - SML analogue of `atlas-scripts/count.at`.
  - Provides simple mutable counters used to count invocations of expensive
    routines (e.g. character computations).
*)

structure Count = struct
  type counter = {use_count: unit -> int, use: unit -> unit, clear: unit -> unit}

  fun make_counter () : counter =
    let
      val ticker = ref 0
      fun use_count () = !ticker
      fun use () = ticker := !ticker + 1
      fun clear () = ticker := 0
    in
      {use_count = use_count, use = use, clear = clear}
    end

  val unitary_test_counter : counter = make_counter ()
  val char_counter : counter = make_counter ()
  val char_counter_to_ht : counter = make_counter ()
end

