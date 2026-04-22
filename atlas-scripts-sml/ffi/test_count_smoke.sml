use "atlas-scripts-sml/count.sml";

val c = Count.make_counter ();
val () = if #use_count c () = 0 then () else raise Fail "init";
val () = (#use c (); #use c (); #use c ());
val () = if #use_count c () = 3 then () else raise Fail "use_count";
val () = (#clear c (); if #use_count c () = 0 then () else raise Fail "clear");

val () = print "OK\n";

