use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/Hermitian.sml";

(*
  File: atlas-scripts-sml/K_types.sml

  Purpose
  - Incremental Standard ML translation of `atlas-scripts/K_types.at`.
  - Provides the core branching and K-signature utilities used to inspect
    (truncated) K-type multiplicities and hermitian signatures.

  Background / Atlas correspondence
  - Atlas `.at` provides a built-in operation:
        `branch : (KTypePol, int) -> KTypePol`
    which performs a long-division style triangular inversion using the
    `K_type_formula` to convert between “standardrepK” expressions and actual
    K-types (up to a height cutoff).

    In the C++ library this is `repr::Rep_context::branch`.
    This port accesses it via the FFI export `atlas_ktypepol_branch` and the
    wrapper `KTypePol.branch`.

  Ported subset
  - `branch_std` variants:
      - `branch_std_ktypepol(P, cutoff)`
      - `branch_std_ktype(mu, cutoff)` (treats `mu` as a singleton term)
  - `mult_std(mu, P)`:
      multiplicity (integer part) of `mu` in `branch_std(P, height(mu))`.
  - `K_signature_irr(p, bound)`:
      compute the hermitian form of an irreducible parameter, branch it, and
      return `(int_part, s_part)` as `KTypePol` handles (integer-coefficient
      polynomials encoded as split coefficients with `s=0`).
  - `signed_mult(mu, p)`:
      return `(m_plus, m_minus)` from the branched hermitian form at
      `height(mu)` (mirrors `signed_mult` in `K_types.at`).

  Not yet ported
  - The `.at` file also defines `branch_irr`, `mult_irr`, `print_K_types`,
    and several helper list operations that depend on `ParamPol` character
    formulas and `K_highest_weights.at`. Those are not included yet.

  Ownership
  - Functions returning `AtlasFFI.ktypepol` allocate new handles; caller must
    free them with `KTypePol.free`.
*)

structure K_types = struct
  type param = AtlasFFI.param
  type ktype = AtlasFFI.ktype
  type ktypepol = AtlasFFI.ktypepol

  fun fail where' msg = raise Fail ("K_types." ^ where' ^ ": " ^ msg)

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => fail "parseInts" ("bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Parse `KType.lambdaRhoText` format: `n x1 ... xn`. *)
  fun parseVecTextWithRankHeader s : int list =
    (case parseInts s of
       n :: rest =>
         if length rest <> n then fail "parseVecTextWithRankHeader" "bad length header" else rest
     | _ => fail "parseVecTextWithRankHeader" "empty")

  (* Determine rank from a `KType` handle. *)
  fun rankOfKType (t: ktype) : int =
    (case parseInts (KType.lambdaRhoText t) of
       n :: _ => n
     | _ => fail "rankOfKType" "bad lambda_rho text")

  fun branch_std_ktypepol (pol: ktypepol, cutoff: int) : ktypepol =
    KTypePol.branch (pol, cutoff)

  fun branch_std_ktype (mu: ktype, cutoff: int) : ktypepol =
    let
      val single = KTypePol.singleton (mu, 1, 0)
      val out =
        (branch_std_ktypepol (single, cutoff)
         handle e => (KTypePol.free single; raise e))
      val () = KTypePol.free single
    in
      out
    end

  type ktype_key = {x: int, height: int, lambdaRho: int list}

  fun keyOfKType (t: ktype) : ktype_key =
    { x = KType.x t
    , height = KType.height t
    , lambdaRho = parseVecTextWithRankHeader (KType.lambdaRhoText t)
    }

  fun termMatchesKey (term: KTypePol.term, k: ktype_key) : bool =
    (#x term = #x k) andalso (#height term = #height k) andalso (#lambdaRho term = #lambdaRho k)

  fun lookupCoef (pol: ktypepol, rank: int, t: ktype) : (int * int) option =
    let
      val k = keyOfKType t
      fun loop ts =
        (case ts of
           [] => NONE
         | u :: rest => if termMatchesKey (u, k) then SOME (#e u, #s u) else loop rest)
    in
      loop (KTypePol.terms (pol, rank))
    end

  fun lookupCoefOrZero (pol: ktypepol, rank: int, t: ktype) : int * int =
    case lookupCoef (pol, rank, t) of
      NONE => (0, 0)
    | SOME z => z

  (* `.at`: `mult_std(p_K,P) = (branch_std(P,height(p_K))[p_K]).int_part`. *)
  fun mult_std (p_K: ktype, P: ktypepol) : int =
    let
      val cutoff = KType.height p_K
      val rank = rankOfKType p_K
      val bran = branch_std_ktypepol (P, cutoff)
      val (e, _) = lookupCoefOrZero (bran, rank, p_K)
      val () = KTypePol.free bran
    in
      e
    end

  (* `.at`: `K_signature_irr(p,bound)` but restricted to the equal-rank
     hermitian-form implementation exposed by `Hermitian`.

     Returns `(int_part(ans), s_part(ans))` where `ans = branch(HI,bound)` and
     each part is encoded as a `KTypePol` with integer coefficients (stored as
     split coefficients with `s=0`). *)
  fun K_signature_irr (p: param, bound: int) : ktypepol * ktypepol =
    let
      val HI = Hermitian.hermitian_form_irreducible p
      val ans =
        (branch_std_ktypepol (HI, bound)
         handle e => (KTypePol.free HI; raise e))
      val () = KTypePol.free HI
      val P = KTypePol.intPart ans
      val Q = KTypePol.sPart ans
      val () = KTypePol.free ans
    in
      (P, Q)
    end

  (* `.at`: `signed_mult(p_K,p)` (equal-rank hermitian-form path). *)
  fun signed_mult (p_K: ktype, p: param) : int * int =
    let
      val bound = KType.height p_K
      val rank = rankOfKType p_K
      val (P, Q) = K_signature_irr (p, bound)
      val (mP, _) = lookupCoefOrZero (P, rank, p_K)
      val (mQ, _) = lookupCoefOrZero (Q, rank, p_K)
      val () = KTypePol.free Q
      val () = KTypePol.free P
    in
      (mP, mQ)
    end
end
