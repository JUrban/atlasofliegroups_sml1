(* Smoke test for `character_table_E7.sml`. *)

use "atlas-scripts-sml/character_table_E7.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    open CharacterTable_E7
    val () = expect ("E7_positions length", length E7_positions, 30)
    val () = expect ("half_table rows", length e7_half_table, 30)
    val () = List.app (fn row => expect ("half_table width", length row, 60)) e7_half_table
    val () = expect ("characters rows", length e7_characters, 60)
    val () = List.app (fn row => expect ("characters width", length row, 60)) e7_characters
    val () = expect ("profile cols", length e7_profile_cols, 60)
    val () = List.app (fn col => expect ("profile width", length col, 4)) e7_profile_cols
    val () = expect ("to_special length", length to_special_E7_table, 60)

    (* sign ⊗ sign = trivial *)
    val sign = List.nth (e7_characters, sign_index)
    val triv = List.nth (e7_characters, 0)
    val () =
      if tensor (sign, sign) = triv then
        ()
      else
        raise Fail "sign tensor sign != trivial"
  in
    print "test_character_table_E7_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

