use "atlas-scripts-sml/time.sml";

val () = if TimeAT.print_time_string 0 = "0.000sec" then () else raise Fail "fmt 0";
val () = if TimeAT.print_time_string 7 = "0.007sec" then () else raise Fail "fmt 7";
val () = if TimeAT.print_time_string 1234 = "1.234sec" then () else raise Fail "fmt 1234";

val t0 = TimeAT.elapsed_ms ();
val () = if t0 >= 0 then () else raise Fail "elapsed_ms negative";

val () = print "OK\n";

