use "atlas-scripts-sml/FaceClasses.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

(* Graph:
   0 <-> 1 -> 2 -> 3 <-> 4
   So SCCs are {0,1}, {2}, {3,4}. *)
val adj =
  [ [1]
  , [0,2]
  , [3]
  , [4]
  , [3]
  ];

val gd = FaceClasses.strong_components adj;
val classes = #classes gd;
val covers = #covers gd;

val _ = assert "num classes" (length classes = 3);
val _ = assert "classes content"
  (List.exists (fn c => c = [0,1]) classes andalso List.exists (fn c => c = [2]) classes andalso List.exists (fn c => c = [3,4]) classes);

(* covers must include an edge from {0,1} to {2} and from {2} to {3,4}. *)
val compOf = FaceClasses.class_of gd;
val c01 = Array.sub (compOf, 0);
val c2 = Array.sub (compOf, 2);
val c34 = Array.sub (compOf, 3);

fun mem xs x = List.exists (fn y => y = x) xs;
val _ = assert "edge 01->2" (mem (List.nth (covers, c01)) c2);
val _ = assert "edge 2->34" (mem (List.nth (covers, c2)) c34);

(* Downward closures. *)
val down = FaceClasses.full_down_classes gd;
val _ = assert "down(c01) includes itself" (mem (Array.sub (down, c01)) c01);
val _ = assert "down(c2) includes c01" (mem (Array.sub (down, c2)) c01);
val _ = assert "down(c34) includes c2" (mem (Array.sub (down, c34)) c2);

val _ = print "OK\n";

