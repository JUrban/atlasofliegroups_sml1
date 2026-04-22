use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/FromDominant.sml

  Purpose
  - SML wrapper for the Atlas operation “make a weight dominant and return a
    Weyl-word witness for returning”.
  - This is the SML-facing analogue of the `.at` helper `from_dominant` from
    `basic.at`, but implemented via a small C++ shim so that we do not need to
    reimplement Weyl group / reflection logic in SML.

  Atlas correspondence
  - In `basic.at`, for an integral weight `v`:
      `from_dominant(rd,v) = (w, v_dom)`
    where `v_dom` is dominant and `w` is a Weyl element such that:
      `w * v_dom = v`.
  - On the C++ side this is exactly `RootDatum::factor_dominant`, whose return
    value is documented as “the Weyl word that will convert it back”.

  API
  - `fromDominantRatvec(g, v)` returns `(witnessWord, v_dom)` where:
      - `witnessWord` is a `WeylWord.t`-compatible list of generator indices
        *for the witness element* `w` above.
      - `v_dom` is the dominant representative.

  Notes
  - This module is used to implement `discrete_series`-style constructors in
    `representations.sml`, which need `cross(inverse(w),x)` for a KGB element.
*)

structure FromDominant = struct
  type ratvec = Lattice.ratvec

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
        | NONE => raise Fail ("FromDominant: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun parseRatvecText (s: string) : ratvec =
    (case parseInts s of
       den :: rest => Lattice.ratvecNormalize {den = den, nums = rest}
     | _ => raise Fail ("FromDominant: bad ratvec text: " ^ s))

  (* Split `atlas_group_from_dominant_ratweight_text` output into two lines. *)
  fun split2Lines (s: string) : string * string =
    let
      fun trimCR t =
        if String.size t > 0 andalso String.sub (t, String.size t - 1) = #"\r" then
          String.substring (t, 0, String.size t - 1)
        else
          t
      val parts = List.map trimCR (String.tokens (fn c => c = #"\n") s)
    in
      case parts of
        [a, b] => (a, b)
      | _ => raise Fail ("FromDominant: expected 2 lines, got: " ^ Int.toString (length parts))
    end

  (* Parse a word line `k s0 ... s_{k-1}` into `[s0,...]`. *)
  fun parseWordLine (line: string) : int list =
    (case parseInts line of
       [] => raise Fail "FromDominant: empty word line"
     | k :: rest =>
         if k = ~1 then
           raise Fail ("FromDominant: from_dominant failed: " ^ AtlasFFI.atlas_last_error ())
         else if k < 0 then
           raise Fail "FromDominant: negative word length"
         else if length rest <> k then
           raise Fail "FromDominant: word arity mismatch"
         else
           rest)

  (* Main entry point: return `(witnessWord, dominantWeight)`. *)
  fun fromDominantRatvec (g: AtlasFFI.group, v: ratvec) : int list * ratvec =
    let
      val out = AtlasFFI.atlas_group_from_dominant_ratweight_text (g, ratvecToText v)
      val (wordLine, domLine) = split2Lines out
      val word = parseWordLine wordLine
      val dom = parseRatvecText domLine
    in
      (word, dom)
    end
end

