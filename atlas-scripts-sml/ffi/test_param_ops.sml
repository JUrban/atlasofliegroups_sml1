use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun showParam tag p =
  print
    (tag ^ ": x=" ^ Int.toString (AtlasFFI.atlas_param_x p) ^ " lam=" ^ AtlasFFI.atlas_param_lambda_text p ^ " nu="
     ^ AtlasFFI.atlas_param_nu_text p ^ " ht=" ^ Int.toString (AtlasFFI.atlas_param_height p) ^ "\n")

fun parsePairs s =
  let
    fun toInt tok =
      case Int.fromString tok of
        SOME n => n
      | NONE => raise Fail ("bad int: " ^ tok)
    val ns = List.map toInt (String.tokens Char.isSpace s)
  in
    case ns of
      [] => raise Fail "empty"
    | k :: rest =>
        let
          fun loop (0, xs, acc) = (List.rev acc, xs)
            | loop (n, a :: b :: xs, acc) = loop (n - 1, xs, (a, b) :: acc)
            | loop _ = raise Fail "bad pair list"
          val (pairs, leftover) = loop (k, rest, [])
        in
          if null leftover then pairs else raise Fail "leftover ints"
        end
  end

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val p = AtlasFFI.atlas_param_trivial g;
val () = showParam "p" p;
val () = print ("gamma=" ^ AtlasFFI.atlas_param_gamma_text p ^ "\n");

val rpText = AtlasFFI.atlas_param_reducibility_points_text p;
val rps = parsePairs rpText;
val () = print ("reducibility_points: " ^ rpText ^ "\n");
val () = print ("rp_count=" ^ Int.toString (length rps) ^ "\n");

val p0 = AtlasFFI.atlas_param_scale (p, 0, 1);
val () =
  if p0 = Foreign.Memory.null then
    raise Fail ("scale failed: " ^ AtlasFFI.atlas_last_error ())
  else
    ();
val () = showParam "p0" p0;

val pCross = AtlasFFI.atlas_param_cross (p, 0);
val () =
  if pCross = Foreign.Memory.null then
    print ("cross failed: " ^ AtlasFFI.atlas_last_error () ^ "\n")
  else
    (showParam "cross0(p)" pCross; AtlasFFI.atlas_param_free pCross);

val pCay = AtlasFFI.atlas_param_cayley (p, 0);
val () =
  if pCay = Foreign.Memory.null then
    print ("cayley failed: " ^ AtlasFFI.atlas_last_error () ^ "\n")
  else
    (showParam "cayley0(p)" pCay; AtlasFFI.atlas_param_free pCay);

val () = AtlasFFI.atlas_param_free p0;
val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;
