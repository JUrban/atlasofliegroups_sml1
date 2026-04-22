use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/unitary.sml";

(*
  File: atlas-scripts-sml/test_unitarity.sml

  Purpose
  - Partial SML analogue of `atlas-scripts/test_unitarity.at`.
  - Provides a small test harness for checking Atlas unitarity predicates on
    explicit parameter lists, and a convenience entry point for the spherical
    unitary point tables in `atlas-scripts-sml/unitary.sml`.

  Background / Atlas correspondence
  - In `test_unitarity.at`, the main driver constructs parameters (typically
    minimal spherical principal series) and checks unitarity via:
      `hf = hermitian_form_irreducible(p)` and `is_pure(hf)` (for delta-fixed p).
  - In this SML port we primarily use the C++-side predicate
      `AtlasFFI.atlas_param_is_unitary`
    which is implemented to match Atlas’s equal-rank hermitian-form purity test
    (see `atlas-scripts-sml/ffi/atlas_smlffi.cpp`).

  What is (and is not) implemented here
  - Implemented:
      - formatting of predicted/computed outcomes (as in `.at`)
      - reducibility point counts via `atlas_param_reducibility_points_text`
      - optional “c-form purity” reporting via `atlas_param_c_form_irreducible`
  - Not implemented (yet):
      - the full `.at` API surface (e.g. `is_fixed(delta,p)` and the full
        hermitian-form construction as an explicit `KTypePol` value).

  Ownership
  - All `AtlasFFI.group`, `AtlasFFI.param`, and `AtlasFFI.ktypepol` handles are
    owned heap objects and must be freed by the caller. This module’s drivers
    free intermediate objects they allocate.
*)
structure TestUnitarity = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec

  (* Render `predicted` as in `test_unitarity.at`: 1,0,-1 -> unitary/none/non-unitary. *)
  fun uflagPred (predicted: int) : string =
    (case predicted + 1 of
       ~1 => "non-unitary"
     | 0 => "none"
     | 1 => "unitary"
     | _ => raise Fail "TestUnitarity.uflagPred: bad predicted value")

  fun uflagBool (b: bool) : string = if b then "unitary" else "non-unitary"

  (* `.at`: if unitary then predicted>=0 else predicted<=0. *)
  fun asPredicted (predicted: int, unitary: bool) : bool =
    if unitary then predicted >= 0 else predicted <= 0

  fun passedFlag (b: bool) : string = if b then "passed" else "failed"

  (* Parse the `atlas_param_reducibility_points_text` format:
     `k a1 b1 ... ak bk` representing rationals `ai/bi`. *)
  fun parseReducibilityPairs (s: string) : (int * int) list =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("TestUnitarity: bad int token: " ^ tok)
      val ns = List.map toInt (String.tokens Char.isSpace s)
    in
      case ns of
        [] => raise Fail "TestUnitarity: reducibility_points_text empty"
      | k :: rest =>
          let
            fun loop (0, xs, acc) = (List.rev acc, xs)
              | loop (n, a :: b :: xs, acc) = loop (n - 1, xs, (a, b) :: acc)
              | loop _ = raise Fail "TestUnitarity: reducibility_points_text truncated"
            val (pairs, leftover) = loop (k, rest, [])
          in
            if null leftover then pairs else raise Fail "TestUnitarity: reducibility_points_text leftover ints"
          end
    end

  fun reducibilityPointCount (p: param) : int =
    length (parseReducibilityPairs (AtlasFFI.atlas_param_reducibility_points_text p))

  (* Optional reporting: compute purity of the (equal-rank) c-form `KTypePol`. *)
  fun cFormPurityString (g: group, p: param) : string option =
    let
      val pol = AtlasFFI.atlas_param_c_form_irreducible p
    in
      if pol = Foreign.Memory.null then
        NONE
      else
        let
          val r = AtlasFFI.atlas_group_rank g
          val s = KTypePol.purityString (pol, r)
          val () = KTypePol.free pol
        in
          SOME s
        end
    end

  (* Test a list of parameters from the same group.
     Each entry is `(p,predicted)` where `predicted` is in {1,0,~1}.

     Returns `true` iff every tested parameter matches `asPredicted`. *)
  fun test (g: group, parameters: (param * int) list, verbose: bool) : bool =
    let
      val () =
        if verbose then
          TextIO.print
            ("Testing " ^ Int.toString (length parameters) ^ " parameters\n"
             ^ "index, parameter hermitian unitary predicted/computed rp_count c_form_purity result\n")
        else
          ()

      fun step (((p, predicted), i), passed) =
        let
          val herm = AtlasFFI.atlas_param_is_hermitian p = 1
          val unitary = AtlasFFI.atlas_param_is_unitary p = 1
          val ok = asPredicted (predicted, unitary)
          val rpCount = reducibilityPointCount p
          val purityOpt = cFormPurityString (g, p)
          val purityTxt = case purityOpt of NONE => "-" | SOME s => s
          val () =
            if verbose then
              TextIO.print
                (Int.toString i ^ ": "
                 ^ "ht=" ^ Int.toString (AtlasFFI.atlas_param_height p)
                 ^ " hermitian=" ^ (if herm then "1" else "0")
                 ^ " unitary=" ^ (if unitary then "1" else "0")
                 ^ " " ^ uflagPred predicted ^ "/" ^ uflagBool unitary
                 ^ " rp=" ^ Int.toString rpCount
                 ^ " cform_purity=" ^ purityTxt
                 ^ " " ^ passedFlag ok ^ "\n")
            else
              ()
        in
          passed andalso ok
        end
    in
      List.foldl step true (ListPair.zip (parameters, List.tabulate (length parameters, fn i => i)))
    end

  (* Convenience: build minimal spherical principal series parameters for a list
     of `nu` points, pairing each with `predicted=1` (unitary), then test them. *)
  fun test_minimal_spherical_unitary_points
    (g: group, nus: ratvec list, verbose: bool, maxN: int option) : bool =
    let
      val nus =
        case maxN of
          NONE => nus
        | SOME n => List.take (nus, Int.min (n, length nus))

      fun mk nu =
        let
          val p = Representations.minimal_spherical_principal_series (g, nu)
        in
          (p, 1)
        end

      val ps = List.map mk nus
      val ok = test (g, ps, verbose)
      val () = List.app (fn (p, _) => AtlasFFI.atlas_param_free p) ps
    in
      ok
    end
end

