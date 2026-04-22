use "atlas-scripts-sml/hash.sml";

(* Basic insertion + lookup + stable indexing. *)
val h = Hash.make_hash Hash.triv_hash_info;
val i0 = #match h 7;
val i1 = #match h 7;
val () = if i0 = 0 andalso i1 = 0 then () else raise Fail "match should be idempotent";
val () = if Hash.lookup h 7 = 0 then () else raise Fail "lookup failed";
val () = if Hash.lookup h 8 = ~1 then () else raise Fail "lookup nonmember failed";

val () = if #could_add h 9 then () else raise Fail "could_add new";
val () = if not (#could_add h 9) then () else raise Fail "could_add old";

val xs = #list h ();
val () = if xs = [7, 9] then () else raise Fail "list insertion order mismatch";
val () = if #index h 1 = 9 then () else raise Fail "index mismatch";

(* Iterator should see entries added during iteration. *)
val it = #iterator h ();
val a0 = #peek it ();
val () = if a0 = SOME 7 then () else raise Fail "iterator first";
val () = (#advance it (); ignore (#match h 11));
val a1 = #peek it ();
val () = if a1 = SOME 9 then () else raise Fail "iterator second";
val () = (#advance it ());
val a2 = #peek it ();
val () = if a2 = SOME 11 then () else raise Fail "iterator saw new entry";

(* Exhaust/orbit generation. *)
val h2 = Hash.make_hash Hash.triv_hash_info;
val () = ignore (#match h2 0);
fun act x = (x + 1) mod 5;
val orb = Hash.exhaust ([act], h2);
val () = if orb = [0, 1, 2, 3, 4] then () else raise Fail "exhaust orbit mismatch";

(* Vec hash helper. *)
val vh = Hash.make_vec_hash ();
val () = ignore (#match vh [1, 2, 3]);
val () = ignore (#match vh [1, 2, 3]);
val () = ignore (#match vh [1, 2, 4]);
val () = if #size vh () = 2 then () else raise Fail "vec hash size mismatch";

val () = print "OK\n";

