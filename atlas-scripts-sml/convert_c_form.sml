use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigRat.sml";
use "atlas-scripts-sml/KType.sml";

(*
  File: atlas-scripts-sml/convert_c_form.sml

  Purpose
  - Partial SML translation of `atlas-scripts/convert_c_form.at`.
  - Exposes the scalar invariant `mu` for `KType`/`Param` used to relate
    hermitian forms and c-forms by parity shifts.

  Atlas correspondence
  - Implements the `.at` function `mu(KType t)` from `convert_c_form.at`:
      mu(t) = < torus_factor(x)+rho_check , (1+theta_x)(lambda+rho) / 2 >
    where `t = (x, lambda_minus_rho)` and `lambda+rho = lambda_minus_rho + 2rho`.

  Implementation approach
  - We compute `mu` via a dedicated C++ shim export:
      `atlas_ktype_mu_simple_text : KType -> "num den"`
    returning an exact rational number in lowest terms.

  Ownership
  - `muParam` allocates a temporary `KType` handle via `KType.ofParam` and frees
    it before returning.
*)
structure Convert_c_form = struct
  type ktype = AtlasFFI.ktype
  type param = AtlasFFI.param

  fun parseIntInf tok =
    case IntInf.fromString tok of
      SOME n => n
    | NONE => raise Fail ("Convert_c_form: bad integer token: " ^ tok)

  (* Parse `"num den"` into a normalized `BigRat.t`. *)
  fun parseRatNumText (s: string) : BigRat.t =
    (case String.tokens Char.isSpace s of
       [numTok, denTok] =>
         BigRat.normalize {num = parseIntInf numTok, den = parseIntInf denTok}
     | _ => raise Fail ("Convert_c_form: expected \"num den\", got: " ^ s))

  fun muKType (t: ktype) : BigRat.t =
    let
      val s = AtlasFFI.atlas_ktype_mu_simple_text t
      val () =
        if String.size s = 0 then
          raise Fail ("Convert_c_form.muKType: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      parseRatNumText s
    end

  (* In `.at` the value depends only on the K-type, so we define `mu(Param p)`
     as `mu(K_type(p))` using the C++-side `Param -> KType` projection. *)
  fun muParam (p: param) : BigRat.t =
    let
      val t = KType.ofParam p
      val q = muKType t
      val () = KType.free t
    in
      q
    end
end

