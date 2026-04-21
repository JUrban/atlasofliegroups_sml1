use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun parseInts s =
  let
    fun toInt tok =
      case Int.fromString tok of
        SOME n => n
      | NONE => raise Fail ("bad int: " ^ tok)
  in
    List.map toInt (String.tokens Char.isSpace s)
  end

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

fun intToCText n =
  if n < 0 then "-" ^ Int.toString (~n) else Int.toString n

fun intsToText xs =
  String.concatWith " " (List.map intToCText xs)

fun takeDataRows path n =
  let
    val input = TextIO.openIn path
    fun loop (acc, k) =
      if k >= n then (TextIO.closeIn input; List.rev acc)
      else
        (case TextIO.inputLine input of
           NONE => (TextIO.closeIn input; List.rev acc)
         | SOME line =>
             let
               val s = rstripNewlines line
             in
               if s = "" orelse (String.size s > 0 andalso String.sub (s, 0) = #"#")
               then loop (acc, k)
               else loop (s :: acc, k + 1)
             end)
  in
    loop ([], 0) handle e => (TextIO.closeIn input; raise e)
  end

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val rows = takeDataRows "atlas-scripts-sml/data/F4_FPP_points.txt" 5;

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
    val u = AtlasFFI.atlas_param_is_unitary_c_form p
    val () = print ("unitary=" ^ Int.toString u ^ "  x=" ^ Int.toString (AtlasFFI.atlas_param_x p) ^ "\n")
  in
    AtlasFFI.atlas_param_free p
  end

val () = List.app checkRow rows;
val () = AtlasFFI.atlas_group_free g;

