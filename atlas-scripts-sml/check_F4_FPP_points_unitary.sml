use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/check_F4_FPP_points_unitary.sml

  Purpose
  - Standalone consistency check: read `atlas-scripts-sml/data/F4_FPP_points.txt`
    and verify each listed parameter is unitary according to `atlas_param_is_unitary`.

  Notes
  - This is primarily a debugging/validation script; it is not imported by the
    main verifier.
*)
fun parseInts s =
  let
    fun toInt tok =
      case Int.fromString tok of
        SOME n => n
      | NONE => raise Fail ("bad int: " ^ tok)
  in
    List.map toInt (String.tokens Char.isSpace s)
  end

(* Strip trailing newline/CR characters from a line. *)
fun rstripNewlines s =
  let
    val n = String.size s
  in
    if n = 0 then s
    else
      case String.sub (s, n - 1) of
        #"\n" => rstripNewlines (String.substring (s, 0, n - 1))
      | #"\r" => rstripNewlines (String.substring (s, 0, n - 1))
      | _ => s
  end

(* Serialize an integer in C/Atlas format (no `~` negatives). *)
fun intToCText n =
  if n < 0 then "-" ^ Int.toString (~n) else Int.toString n

(* Serialize an integer list in C/Atlas format. *)
fun intsToText xs =
  String.concatWith " " (List.map intToCText xs)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val input = TextIO.openIn "atlas-scripts-sml/data/F4_FPP_points.txt";

val total = ref 0;
val bad = ref 0;

(* Parse one data row, build the parameter, and update counters. *)
fun checkRow row =
  let
    val ns = parseInts row
    val (x, lamDen, l1, l2, l3, l4, nuDen, n1, n2, n3, n4) =
      case ns of
        [a, b, c, d, e, f, g, h, i, j, k] => (a, b, c, d, e, f, g, h, i, j, k)
      | _ => raise Fail ("unexpected row shape: " ^ row)
    val p =
      AtlasFFI.atlas_param_new_from_lambda_nu_text
        (g, x, intsToText [l1, l2, l3, l4], lamDen, intsToText [n1, n2, n3, n4], nuDen)
    val () =
      if p = Foreign.Memory.null then
        raise Fail ("param construction failed: " ^ AtlasFFI.atlas_last_error ())
      else
        ()
    val u = AtlasFFI.atlas_param_is_unitary p
    val () = AtlasFFI.atlas_param_free p
  in
    total := !total + 1;
    if u = 1 then () else bad := !bad + 1;
    if (!total mod 100 = 0) then
      print ("checked " ^ Int.toString (!total) ^ " bad=" ^ Int.toString (!bad) ^ "\n")
    else
      ()
  end

(* Main file-processing loop. *)
fun loop () =
  case TextIO.inputLine input of
    NONE => ()
  | SOME line =>
      let
        val s = rstripNewlines line
      in
        if s = "" orelse (String.size s > 0 andalso String.sub (s, 0) = #"#")
        then loop ()
        else (checkRow s; loop ())
      end

val () = (loop (); TextIO.closeIn input) handle e => (TextIO.closeIn input; raise e);
val () = AtlasFFI.atlas_group_free g;

val () = print ("total=" ^ Int.toString (!total) ^ " bad=" ^ Int.toString (!bad) ^ "\n");
