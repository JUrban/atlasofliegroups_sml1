use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/sort.sml";

(*
  File: atlas-scripts-sml/K_norm.sml

  Purpose
  - Incremental Standard ML port of `atlas-scripts/K_norm.at`.
  - Provides the core “K-norm / height” utilities used by many Atlas scripts
    to sort and bound K-types and K-type polynomials.

  Atlas correspondence (ported subset)
  - In `K_norm.at`, the default definition is:
      `K_norm(rd) = height@KType`
    i.e. the K-norm is just the Atlas K-type `height`.

  Design notes
  - The `.at` file defines many overloaded `K_norm`/`K_norms` variants over
    `KType`, `Param`, `Split`, `KTypePol`, etc. Standard ML has no ad-hoc
    overloading, so this port exposes a small family of explicitly named
    functions:
      - `K_norm_ktype`     : `RootDatum.t -> KType.ktype -> int`
      - `K_norm_param`     : `RootDatum.t -> AtlasFFI.param -> int`
      - `K_norm_ktypepol`  : `RootDatum.t -> AtlasFFI.ktypepol -> int`
      - `K_norms_ktypepol` : `RootDatum.t -> AtlasFFI.ktypepol -> int list`
    The `RootDatum.t` argument is currently ignored, matching the default `.at`
    behaviour.

  Unported parts
  - The enumerators and cone/Levi-based routines from the latter half of
    `K_norm.at` are not yet ported (they depend on additional `.at`-level
    infrastructure such as `cone`, `solve`, `no_Cminus_roots`, and more).
*)

structure K_norm = struct
  type rootdatum = RootDatum.t
  type param = AtlasFFI.param
  type ktype = KType.ktype
  type ktypepol = AtlasFFI.ktypepol

  fun K_norm_ktype (_: rootdatum) (t: ktype) : int = KType.height t

  fun K_norm_param (rd: rootdatum) (p: param) : int =
    let
      val t = KType.ofParam p
      val h = K_norm_ktype rd t
      val () = KType.free t
    in
      h
    end

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("K_norm: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Extract the `height` field from `atlas_ktypepol_term_text`.
     Format: `e s x height <lambda_rho...>` (we ignore the tail). *)
  fun termHeightText (s: string) : int =
    (case parseInts s of
       _ :: _ :: _ :: h :: _ => h
     | _ => raise Fail "K_norm: truncated ktypepol term")

  fun K_norms_ktypepol (_: rootdatum) (pol: ktypepol) : int list =
    let
      val n = AtlasFFI.atlas_ktypepol_num_terms pol
      val () =
        if n < 0 then
          raise Fail ("K_norm.K_norms_ktypepol: num_terms failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      fun one i =
        let
          val s = AtlasFFI.atlas_ktypepol_term_text (pol, i)
          val () =
            if s = "-1" then
              raise Fail ("K_norm.K_norms_ktypepol: term_text failed: " ^ AtlasFFI.atlas_last_error ())
            else
              ()
        in
          termHeightText s
        end
    in
      List.tabulate (n, one)
    end

  fun K_norm_ktypepol (rd: rootdatum) (pol: ktypepol) : int =
    List.foldl Int.max 0 (K_norms_ktypepol rd pol)

  fun all_K_norms_ktypepol (rd: rootdatum) (pol: ktypepol) : int list =
    Sort.sort (op <=) (K_norms_ktypepol rd pol)

  fun sort_by_K_norm (rd: rootdatum) (ts: ktype list) : ktype list =
    let
      fun leq (a: ktype, b: ktype) = K_norm_ktype rd a <= K_norm_ktype rd b
    in
      Sort.sort leq ts
    end
end
