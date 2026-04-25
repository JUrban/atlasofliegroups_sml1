use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/ParamPol.sml";

(*
  File: atlas-scripts-sml/writeFiles.sml

  Purpose
  - SML translation of `atlas-scripts/writeFiles.at`.
  - The original `.at` file is a collection of “writers” that *print code* for
    later reloading. In the `.at` interpreter this primarily means emitting
    Atlas-script (`.at`) snippets that reconstruct large lists without
    allocating a single enormous expression.

  What this SML port does
  - Provides a small, practical subset of those writers, focusing on:
      - emitting Atlas-script snippets for `[vec]` and `[int]` lists; and
      - emitting SML snippets for `Param` lists and `KTypePol` lists, using the
        Poly/ML FFI layer instead of the Atlas interpreter.

  Design notes
  - The “`.at` emitters” write strings like `set ...` / `void:...` which can be
    loaded by the Atlas interpreter.
  - The “SML emitters” write code that reconstructs values using:
      - `AtlasFFI.atlas_param_new_from_lambda_nu_text` for parameters
      - `AtlasFFI.atlas_ktype_new_from_x_lambda_rho_text` +
        `KTypePol.singleton` for K-type polynomials
    These outputs do not depend on any `.at` scripts.

  Scope / limitations
  - This module does not attempt to serialize full Atlas `RealForm` objects,
    because the current FFI does not expose a general “construct group from
    root datum + inner class” API matching `real_form(...)` in `.at`.
  - The SML emitters therefore take an explicit *group variable name* (e.g.
    `"g"`) assumed to exist in the generated file.
*)

structure WriteFiles = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ktypepol = AtlasFFI.ktypepol

  val default_vecs_name = "vecs_saved"

  (* ---------- Small pretty-printers ---------- *)

  fun joinWith (sep: string, xs: string list) : string =
    String.concatWith sep xs

  fun ppInt (n: int) : string = Int.toString n

  fun ppIntList (xs: int list) : string =
    "[" ^ joinWith (",", List.map ppInt xs) ^ "]"

  fun ppIntListSpaced (xs: int list) : string =
    joinWith (" ", List.map ppInt xs)

  (* Parse `atlas_param_lambda_text`/`atlas_param_nu_text` etc: "den n1 n2 ...". *)
  fun parseRatWeightText (s: string) : {den: int, nums: int list} =
    let
      val toks = String.tokens Char.isSpace s
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("WriteFiles.parseRatWeightText: bad int token: " ^ tok)
    in
      case toks of
        [] => raise Fail "WriteFiles.parseRatWeightText: empty string"
      | denTok :: rest => {den = toInt denTok, nums = List.map toInt rest}
    end

  (* ---------- Emit Atlas-script (`.at`) snippets ---------- *)

  fun emitLine (out: TextIO.outstream, s: string) : unit =
    TextIO.output (out, s ^ "\n")

  fun write_vecs_at (out: TextIO.outstream, vecs: int list list, vecsName: string) : unit =
    let
      val n = length vecs
      val () = emitLine (out, "set " ^ vecsName ^ " = [vec]: for i:" ^ Int.toString n ^ " do [] od")
      fun one (v, i) =
        emitLine
          ( out
          , "void:" ^ vecsName ^ "[" ^ Int.toString i ^ "]:=" ^ "[int]:" ^ ppIntList v
          )
    in
      List.app one (ListPair.zip (vecs, List.tabulate (n, fn i => i)))
    end

  fun write_vecs_at_default (out: TextIO.outstream, vecs: int list list) : unit =
    write_vecs_at (out, vecs, default_vecs_name)

  fun write_append_at (out: TextIO.outstream, xs: int list, listName: string) : unit =
    List.app (fn x => emitLine (out, "void:" ^ listName ^ "#:=" ^ Int.toString x)) xs

  (* Assume the `[int] listName` exists: append by concatenation (as in `.at`). *)
  fun write_cat_int_list_at (out: TextIO.outstream, xs: int list, listName: string) : unit =
    List.app (fn x => emitLine (out, "then () = " ^ listName ^ "#:=" ^ Int.toString x)) xs

  (* ---------- Emit SML snippets (FFI-based; no `.at` dependency) ---------- *)

  (* Emit a single SML string literal (with minimal escaping). *)
  fun smlStringLit (s: string) : string =
    let
      fun esc #"\"" = "\\\""
        | esc #"\n" = "\\n"
        | esc #"\t" = "\\t"
        | esc #"\\" = "\\\\"
        | esc c = str c
    in
      "\"" ^ String.concat (List.map esc (String.explode s)) ^ "\""
    end

  (* Emit code that constructs a param using `atlas_param_new_from_lambda_nu_text`.
     The caller must ensure `groupVar` is a variable bound to an `AtlasFFI.group`. *)
  fun emitParamCtorSml (groupVar: string, p: param) : string =
    let
      val x = AtlasFFI.atlas_param_x p
      val lamText = AtlasFFI.atlas_param_lambda_text p
      val nuText = AtlasFFI.atlas_param_nu_text p
      val {den = lamDen, nums = lamNums} = parseRatWeightText lamText
      val {den = nuDen, nums = nuNums} = parseRatWeightText nuText
      val lamNumsText = joinWith (" ", List.map ppInt lamNums)
      val nuNumsText = joinWith (" ", List.map ppInt nuNums)
    in
      "let val p = AtlasFFI.atlas_param_new_from_lambda_nu_text (" ^
      groupVar ^ ", " ^ Int.toString x ^ ", " ^ smlStringLit lamNumsText ^ ", " ^
      Int.toString lamDen ^ ", " ^ smlStringLit nuNumsText ^ ", " ^ Int.toString nuDen ^ ")\n" ^
      " in if p = Foreign.Memory.null then raise Fail (\"param ctor failed: \" ^ AtlasFFI.atlas_last_error ()) else p end"
    end

  (* Emit a `val <listName> : AtlasFFI.param list = [...]` block. *)
  fun write_param_list_sml
    (out: TextIO.outstream, groupVar: string, ps: param list, listName: string) : unit =
    let
      val ctors = List.map (emitParamCtorSml groupVar) ps
      val body = "[\n" ^ joinWith (",\n", ctors) ^ "\n]"
      val () = emitLine (out, "use \"atlas-scripts-sml/ffi/AtlasFFI.sml\";")
      val () = emitLine (out, "(* Requires: val " ^ groupVar ^ " : AtlasFFI.group = ... *)")
      val () = emitLine (out, "val " ^ listName ^ " : AtlasFFI.param list = " ^ body ^ ";")
    in
      ()
    end

  (* Emit code to reconstruct a `KTypePol` from decoded term data.
     Requires `groupVar : AtlasFFI.group`. *)
  fun emitKTypePolCtorSml (groupVar: string, polVar: string, terms: KTypePol.term list) : string =
    let
      fun oneTerm t =
        let
          val lamText = ppIntListSpaced (#lambdaRho t)
        in
          "let val mu = AtlasFFI.atlas_ktype_new_from_x_lambda_rho_text (" ^
          groupVar ^ ", " ^ Int.toString (#x t) ^ ", " ^ smlStringLit lamText ^ ")\n" ^
          " in if mu = Foreign.Memory.null then raise Fail (\"ktype ctor failed: \" ^ AtlasFFI.atlas_last_error ())\n" ^
          "    else let val q = KTypePol.singleton (mu, " ^ Int.toString (#e t) ^ ", " ^
          Int.toString (#s t) ^ ")\n" ^
          "         in AtlasFFI.atlas_ktype_free mu; q end\n" ^
          " end"
        end
    in
      case terms of
        [] => polVar ^ " := KTypePol.null " ^ groupVar ^ ";"
      | t0 :: rest =>
          polVar ^ " := " ^ oneTerm t0 ^ ";\n" ^
          joinWith
            ("\n"
            , List.map
                (fn t =>
                   "let val q = " ^ oneTerm t ^ "\n" ^
                   "    val r = KTypePol.add (!" ^ polVar ^ ", q)\n" ^
                   "in KTypePol.free (!" ^ polVar ^ "); KTypePol.free q; " ^ polVar ^ " := r end;")
                rest)
    end

  (* Emit a `val <name> : AtlasFFI.ktypepol list = ...` block.
     The generated code constructs each `KTypePol` by summing singleton terms.

     Ownership note: the resulting list owns the `ktypepol` handles; callers
     should free them with `KTypePol.free` when done. *)
  fun write_K_type_pols_sml
    (out: TextIO.outstream, groupVar: string, rank: int, pols: ktypepol list, listName: string) : unit =
    let
      val decoded = List.map (fn p => KTypePol.terms (p, rank)) pols
      fun one (ts, i) =
        let
          val r = "r" ^ Int.toString i
        in
          "let val " ^ r ^ " : AtlasFFI.ktypepol ref = ref (KTypePol.null " ^ groupVar ^ ")\n" ^
          "    val () = (" ^ emitKTypePolCtorSml (groupVar, r, ts) ^ ")\n" ^
          "in !" ^ r ^ " end"
        end
      val ctors2 =
        List.map
          one
          (ListPair.zip (decoded, List.tabulate (length decoded, fn i => i)))
      val body = "[\n" ^ joinWith (",\n", ctors2) ^ "\n]"
      val () = emitLine (out, "use \"atlas-scripts-sml/ffi/AtlasFFI.sml\";")
      val () = emitLine (out, "use \"atlas-scripts-sml/KTypePol.sml\";")
      val () = emitLine (out, "(* Requires: val " ^ groupVar ^ " : AtlasFFI.group = ... *)")
      val () = emitLine (out, "val " ^ listName ^ " : AtlasFFI.ktypepol list = " ^ body ^ ";")
    in
      ()
    end
end
