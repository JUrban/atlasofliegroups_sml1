use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/WeylWord.sml

  Purpose
  - Minimal representation of Weyl group words (as simple reflection indices)
    for porting Atlas `.at` scripts that manipulate `WeylElt` values only via
    their reduced words.

  Atlas correspondence
  - In `.at`, `WeylElt` has a `word : [int]` field giving a simple-reflection
    word.
  - `basic.at` defines KGB cross actions for Weyl elements by iterating over
    those words (with a reversal convention):
      - `cross(WeylElt w, KGBElt x)` iterates over `w.word ~` (reverse order).
      - `cross(KGBElt x, WeylElt w)` iterates over `w.word` (forward order).

  What this module provides
  - A type `t = int list` representing a Weyl word `[s0,s1,...,sk-1]`.
  - Text encoding helpers compatible with the C++ shim:
      - `toText w` encodes as `"k s0 s1 ... sk-1"`.
      - `fromText` parses the same format.
  - `inverse` as word reversal (valid because simple reflections are involutions).
  - `kgbCrossLeft` which calls into the Atlas C++ library to compute
    `cross(WeylElt w, KGBElt x)` (the `.at` “left” convention above).

  Notes
  - This is intentionally *not* a full Weyl group implementation in SML.
    It is just enough structure to translate constructors like `discrete_series`
    that depend on `from_dominant` returning a witness word.
*)

structure WeylWord = struct
  type t = int list

  type ratvec = Lattice.ratvec

  (* Encode a word `[s0,...,s_{k-1}]` as `"k s0 ... s_{k-1}"`. *)
  fun toText (w: t) : string =
    String.concatWith " " (Int.toString (length w) :: List.map Int.toString w)

  (* Parse `"k s0 ... s_{k-1}"` into `[s0,...,s_{k-1}]`. *)
  fun fromText (text: string) : t =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("WeylWord.fromText: bad int token: " ^ tok)
      val xs = List.map toInt (String.tokens Char.isSpace text)
    in
      case xs of
        [] => raise Fail "WeylWord.fromText: empty"
      | k :: rest =>
          if k < 0 then raise Fail "WeylWord.fromText: negative length"
          else if length rest <> k then raise Fail "WeylWord.fromText: wrong arity"
          else rest
    end

  (* Inverse of a Weyl word (reverse order; generators are involutions). *)
  fun inverse (w: t) : t = List.rev w

  (* Convert SML `~` negatives to C-style `-` negatives. *)
  fun intToCText (n: int) : string =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  fun ratvecToText (u: ratvec) : string =
    let
      val u = Lattice.ratvecNormalize u
    in
      String.concatWith " " (intToCText (#den u) :: List.map intToCText (#nums u))
    end

  fun parseInts (s: string) : int list =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("WeylWord: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun parseRatvecText (s: string) : ratvec =
    (case parseInts s of
       den :: rest => Lattice.ratvecNormalize {den = den, nums = rest}
     | _ => raise Fail ("WeylWord: bad ratweight text: " ^ s))

  (* Weyl group action on rational weights:
       returns `w * v` for the Weyl element encoded by `w`.

     This is implemented by the C++ shim `atlas_group_weyl_word_act_ratweight_text`.
  *)
  fun actRatvec (g: AtlasFFI.group, w: t, v: ratvec) : ratvec =
    let
      val out = AtlasFFI.atlas_group_weyl_word_act_ratweight_text (g, toText w, ratvecToText v)
    in
      case parseInts out of
        ~1 :: _ => raise Fail ("WeylWord.actRatvec failed: " ^ AtlasFFI.atlas_last_error ())
      | _ => parseRatvecText out
    end

  (* Cross action `cross(WeylElt w, KGBElt x)` (as in `basic.at`).

     This uses the C++ shim’s `atlas_kgb_cross_word_text`, which matches the
     `.at` convention of iterating over `w.word ~` (reverse order). *)
  fun kgbCrossLeft (g: AtlasFFI.group, w: t, x: int) : int =
    let
      val y = AtlasFFI.atlas_kgb_cross_word_text (g, x, toText w)
    in
      if y < 0 then
        raise Fail ("WeylWord.kgbCrossLeft: cross failed: " ^ AtlasFFI.atlas_last_error ())
      else
        y
    end
end
