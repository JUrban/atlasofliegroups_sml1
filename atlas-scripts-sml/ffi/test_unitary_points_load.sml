use "atlas-scripts-sml/unitary.sml";

fun assertEqInt (a: int, b: int, msg: string) =
  if a = b then () else raise Fail (msg ^ ": expected " ^ Int.toString b ^ " got " ^ Int.toString a);

val () = assertEqInt (length Unitary.F4_spherical_unitary, 59, "F4_spherical_unitary length");
val () = assertEqInt (length Unitary.D4_spherical_unitary, 33, "D4_spherical_unitary length");
val () = assertEqInt (length Unitary.E7_spherical_unitary, 918, "E7_spherical_unitary length");

val () = print "OK\n";

