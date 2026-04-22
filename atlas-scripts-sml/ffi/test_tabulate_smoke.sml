use "atlas-scripts-sml/tabulate.sml";

val data =
  [ ["name", "value"]
  , ["alpha", "1"]
  , ["beta", "10"]
  ];

val () = Tabulate.tabulate (data, "ll", 2, " ");
val () = print "OK\n";

