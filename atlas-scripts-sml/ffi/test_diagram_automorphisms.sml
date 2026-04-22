use "atlas-scripts-sml/diagram.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val a2 = LieType.parse "A2";
val autosA2 = Diagram.diagram_automorphisms a2;
val _ = assert "A2 has 2 automorphisms" (length autosA2 = 2);

val d4 = LieType.parse "D4";
val autosD4 = Diagram.diagram_automorphisms d4;
val _ = assert "D4 has 6 automorphisms" (length autosD4 = 6);

val a1a1 = LieType.parse "A1A1";
val autosA1A1 = Diagram.diagram_automorphisms a1a1;
val _ = assert "A1xA1 has 2 automorphisms (swap factors)" (length autosA1A1 = 2);

val _ = print "ok\n";

