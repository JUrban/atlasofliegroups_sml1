use "atlas-scripts-sml/F4_s_unipdata.sml";

(*
  Smoke test for `atlas-scripts-sml/F4_s_unipdata.sml`.
*)

val () =
  if length F4_s_unipdata.data = 75 then
    ()
  else
    raise Fail "F4_s_unipdata: unexpected row count";

val () = TextIO.print "OK\n";

