use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ffi/Util.sml";

fun parseInts s =
  let
    fun toInt tok =
      case Int.fromString tok of
        SOME n => n
      | NONE => raise Fail ("bad int: " ^ tok)
  in
    List.map toInt (String.tokens Char.isSpace s)
  end

fun firstNonEmptyLine path =
  let
    val input = TextIO.openIn path
    fun loop () =
      case TextIO.inputLine input of
        NONE => raise Fail ("no data in " ^ path)
      | SOME line =>
          let
            val s =
              let
                fun rstrip t =
                  let val n = String.size t in
                    if n = 0 then t
                    else
                      case String.sub (t, n - 1) of
                        #"\n" => rstrip (String.substring (t, 0, n - 1))
                      | #"\r" => rstrip (String.substring (t, 0, n - 1))
                      | _ => t
                  end
              in
                rstrip line
              end
          in
            if s = "" orelse (String.size s > 0 andalso String.sub (s, 0) = #"#") then loop ()
            else (TextIO.closeIn input; s)
          end
  in
    loop () handle e => (TextIO.closeIn input; raise e)
  end

val dataLine = firstNonEmptyLine "atlas-scripts-sml/data/F4_FPP_points.txt";
val ns = parseInts dataLine;

val (x, lamDen, l1, l2, l3, l4, nuDen, n1, n2, n3, n4) =
  case ns of
    [a, b, c, d, e, f, g, h, i, j, k] => (a, b, c, d, e, f, g, h, i, j, k)
  | _ => raise Fail ("unexpected row shape: " ^ dataLine);

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

val p =
  FFIUtil.withInt32Array [l1, l2, l3, l4] (fn lamPtr =>
    FFIUtil.withInt32Array [n1, n2, n3, n4] (fn nuPtr =>
      AtlasFFI.atlas_param_new_from_lambda_nu (g, x, lamDen, lamPtr, nuDen, nuPtr)));

val hx = AtlasFFI.atlas_param_x p;
val hh = AtlasFFI.atlas_param_height p;
val _ = print ("param from points: x=" ^ Int.toString hx ^ " height=" ^ Int.toString hh ^ "\n");

val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ =
  if hx < 0 orelse hh < 0
  then print ("C++ error: " ^ AtlasFFI.atlas_last_error () ^ "\n")
  else ();

