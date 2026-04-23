use "atlas-scripts-sml/FaceClasses.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

(* Synthetic `[[FaceVertsKHash]]`-style table:

   dim0 faces (vertex lists length 1, plus tail datum):
     v0 = [0,7]
     v1 = [1,7]
     v2 = [2,8]

   dim1 faces (edge vertex lists length 2, plus tail datum):
     e01 = [0,1,7]  tail matches v0 and v1
     e12 = [1,2,8]  tail matches v2 only

   The resulting SCCs under `up_graph_gens_FDKH` should be:
     C1 = {v0,v1,e01}
     C2 = {v2,e12}
   with an edge C1 -> C2 (via v1 -> e12 closure edge).
*)

val fd : int list list list =
  [ [[0,7],[1,7],[2,8]]
  , [[0,1,7],[1,2,8]]
  ];

val gd = FaceClasses.up_data_FDKH fd;
val comps = FaceClasses.class_of gd;

val idx = FaceClasses.index_f fd;
val v0 = idx (0,0);
val v1 = idx (0,1);
val v2 = idx (0,2);
val e01 = idx (1,0);
val e12 = idx (1,1);

val c1 = Array.sub (comps, v0);
val _ = assert "v1 same class as v0" (Array.sub (comps, v1) = c1);
val _ = assert "e01 same class as v0" (Array.sub (comps, e01) = c1);

val c2 = Array.sub (comps, v2);
val _ = assert "e12 same class as v2" (Array.sub (comps, e12) = c2);
val _ = assert "classes distinct" (c1 <> c2);

fun mem xs x = List.exists (fn y => y = x) xs;
val _ = assert "edge C1 -> C2" (mem (List.nth (#covers gd, c1)) c2);

val _ = print "OK\n";

