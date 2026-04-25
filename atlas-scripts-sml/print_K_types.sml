use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/K_types.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/tabulate.sml";

(*
  File: atlas-scripts-sml/print_K_types.sml

  Purpose
  - Partial SML translation of `atlas-scripts/print_K_types.at`.
  - The `.at` file is mostly printing helpers for K-types and K-signatures.

  Scope of this port
  - Implemented:
      - `print_branch_std(P,bound)` for `KTypePol` (prints the branched terms).
      - `print_K_signature_irr(p,bound)` using `K_types.K_signature_irr`.
  - Not implemented:
      - `branch_irr` and the ParamPol-based printing helpers (require KL/character
        formula infrastructure).
      - “long” variants that depend on `highest_weights` are not yet available.

  Notes
  - Atlas split coefficients are printed in a minimal `e+s*s` form.
*)

structure Print_K_types = struct
  type param = AtlasFFI.param
  type ktypepol = AtlasFFI.ktypepol

  fun splitToString (e: int, s: int) : string =
    if s = 0 then
      Int.toString e
    else if e = 0 then
      Int.toString s ^ "*s"
    else
      Int.toString e ^ " + " ^ Int.toString s ^ "*s"

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("Print_K_types: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Infer the ambient rank from the first term text (length - 4).
     Returns 0 for empty polynomials. *)
  fun inferRank (pol: ktypepol) : int =
    let
      val n = AtlasFFI.atlas_ktypepol_num_terms pol
    in
      if n <= 0 then 0
      else
        let
          val s = AtlasFFI.atlas_ktypepol_term_text (pol, 0)
          val ns = parseInts s
          val r = length ns - 4
        in
          if r < 0 then 0 else r
        end
    end

  fun print_branch_std (P: ktypepol, bound: int) : unit =
    let
      val bran = K_types.branch_std_ktypepol (P, bound)
      val rank = inferRank bran
      val terms = KTypePol.terms (bran, rank)
      val () = KTypePol.free bran
      val rows =
        List.map
          (fn t =>
             [ splitToString (#e t, #s t)
             , Int.toString (#x t)
             , "[" ^ String.concatWith "," (List.map Int.toString (#lambdaRho t)) ^ "]"
             , Int.toString (#height t)
             ])
          terms
      val data = ["m", "x", "lambda_rho", "height"] :: rows
    in
      Tabulate.tabulate (data, "llll", 2, " ")
    end

  fun print_branch_std_long (P: ktypepol, bound: int) : unit =
    let
      val _ = (P, bound)
    in
      raise Fail "Print_K_types.print_branch_std_long: not yet ported (requires highest_weights)"
    end

  fun print_branch_irr (P: unit, bound: int) : unit =
    let
      val _ = (P, bound)
    in
      raise Fail "Print_K_types.print_branch_irr: not yet ported (requires branch_irr/character formulas)"
    end

  fun print_branch_irr_long (P: unit, bound: int) : unit =
    let
      val _ = (P, bound)
    in
      raise Fail "Print_K_types.print_branch_irr_long: not yet ported (requires branch_irr/character formulas)"
    end

  fun print_K_signature_irr (p: param, bound: int) : unit =
    let
      val (pos, neg) = K_types.K_signature_irr (p, bound)
      val () =
        if AtlasFFI.atlas_ktypepol_num_terms pos = 0 then
          print "Positive part is empty\n"
        else
          (print "Positive part:\n"; print_branch_std (pos, bound))
      val () =
        if AtlasFFI.atlas_ktypepol_num_terms neg = 0 then
          print "Negative part is empty\n"
        else
          (print "Negative part:\n"; print_branch_std (neg, bound))
      val () = KTypePol.free pos
      val () = KTypePol.free neg
    in
      ()
    end
end
