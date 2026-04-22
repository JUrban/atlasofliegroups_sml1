use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/FromDominant.sml";

(*
  File: atlas-scripts-sml/ffi/test_from_dominant_smoke.sml

  Purpose
  - Smoke test for the `from_dominant` FFI shim:
      `atlas_group_from_dominant_ratweight_text`
    and the SML wrapper `FromDominant.fromDominantRatvec`.

  What it checks
  - The returned `v_dom` is dominant with respect to the group’s simple coroots,
    i.e. `alpha^\vee(v_dom) >= 0` for each simple coroot `alpha^\vee`.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_from_dominant_smoke.sml`
*)

fun parseInts (s: string) : int list =
  let
    fun toInt tok =
      case Int.fromString tok of
        SOME n => n
      | NONE => raise Fail ("bad int token: " ^ tok)
  in
    List.map toInt (String.tokens Char.isSpace s)
  end

(* Parse an `n m ...` row-major integer matrix. *)
fun parseIntMatText (s: string) : int list list =
  (case parseInts s of
     n :: m :: rest =>
       let
         val need = n * m
         val () = if length rest = need then () else raise Fail "matrix size mismatch"
         fun row i = List.take (List.drop (rest, i * m), m)
       in
         List.tabulate (n, row)
       end
   | _ => raise Fail "bad matrix text")

fun column (a: int list list, j: int) : int list =
  List.map (fn row => List.nth (row, j)) a

fun dot (xs: int list, ys: int list) : int =
  List.foldl op+ 0 (ListPair.mapEq (op * ) (xs, ys))

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val rank = AtlasFFI.atlas_group_rank g
val ssr = AtlasFFI.atlas_group_semisimple_rank g
val coroots = parseIntMatText (AtlasFFI.atlas_group_simple_coroots_text g)

(* Pick an obviously non-dominant weight (in the ambient coordinates). *)
val v = {den = 1, nums = ~1 :: List.tabulate (rank - 1, fn _ => 0)}
val (_, vDom) = FromDominant.fromDominantRatvec (g, v)

val vDomNum = #nums (Lattice.ratvecNormalize vDom)

val () =
  let
    fun check s =
      let
        val alphaV = column (coroots, s)
        val eval = dot (alphaV, vDomNum)
      in
        if eval < 0 then
          raise Fail ("not dominant at s=" ^ Int.toString s ^ " eval=" ^ Int.toString eval)
        else
          ()
      end
  in
    List.app check (List.tabulate (ssr, fn i => i))
  end

val () = AtlasFFI.atlas_group_free g
val () = TextIO.print "OK\n"

