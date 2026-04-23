use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/parameters.sml

  Purpose
  - Partial SML analogue of `atlas-scripts/parameters.at`.
  - Provides a small collection of parameter- and KGB-related utilities that
    frequently appear in `.at` scripts, but implemented in terms of the Poly/ML
    FFI over the Atlas C++ library rather than the Atlas interpreter.

  Scope (incremental)
  - Implemented:
      - `torus_factor(g,x)` for a KGB element index `x`
      - `rho_check(g)` and `base_grading_vector(g)` for a real group `g`
      - `rho_check_rootdatum(rd)` for a complex `RootDatum`
      - `square(g)` (the `parameters.at` `square(RealForm)` definition)
      - `contragredient(p)` via the Atlas C++ routine
  - Not yet implemented:
      - Most of the duality / integrality-datum helpers in `parameters.at`
        (`dual_inner_class`, `choose_g`, `y`, `tits`, ...)
      - InnerClass/RealForm typed overloads (the SML port uses `AtlasFFI.group`
        handles and explicit arguments instead).

  Types / conventions
  - We use `int` for KGB elements (the Atlas C++ KGB index).
  - Rational vectors are `Lattice.ratvec = {den:int, nums:int list}` and are
    normalized by `Lattice.ratvecNormalize` where appropriate.
*)

structure Parameters = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type rootdatum = AtlasFFI.rootdatum
  type kgbelt = int
  type ratvec = Lattice.ratvec

  (* Parse `den n1 n2 ... nk`. *)
  fun parseRatvecText (s: string) : ratvec =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("Parameters.parseRatvecText: bad int token: " ^ tok)
      val ns = List.map toInt (String.tokens Char.isSpace s)
    in
      case ns of
        den :: nums => Lattice.ratvecNormalize {den = den, nums = nums}
      | _ => raise Fail "Parameters.parseRatvecText: empty"
    end

  (* Rational addition. *)
  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec =
    let
      val du = #den u
      val dv = #den v
      val numsU = #nums u
      val numsV = #nums v
      val () = if du = 0 orelse dv = 0 then raise Fail "Parameters.ratvecAdd: zero denom" else ()
      val () = if length numsU = length numsV then () else raise Fail "Parameters.ratvecAdd: length mismatch"
      val g = Lattice.gcd (du, dv)
      val aMul = dv div g
      val bMul = du div g
      val den = du * aMul
      val nums = ListPair.mapEq (fn (a, b) => a * aMul + b * bMul) (numsU, numsV)
    in
      Lattice.ratvecNormalize {den = den, nums = nums}
    end

  (* Coordinatewise reduction modulo an integer `m` (as in `.at` for `%m`). *)
  fun ratvecMod (u: ratvec, m: int) : ratvec =
    if m <= 0 then
      raise Fail "Parameters.ratvecMod: expected positive modulus"
    else
      let
        val u = Lattice.ratvecNormalize u
        val den = #den u
        val modBase = m * den
        fun modPos a =
          let
            val r = a mod modBase
          in
            if r < 0 then r + modBase else r
          end
      in
        {den = den, nums = List.map modPos (#nums u)}
      end

  (* `torus_factor(x)` for KGB element `x`, but made explicit in `g`. *)
  fun torus_factor (g: group, x: kgbelt) : ratvec =
    parseRatvecText (AtlasFFI.atlas_kgb_torus_factor_text (g, x))

  (* `rho_check(G)` for a real group `G`. *)
  fun rho_check (g: group) : ratvec =
    parseRatvecText (AtlasFFI.atlas_group_rho_check_text g)

  (* `base_grading_vector(G)` for a real group `G` (Atlas: `g_rho_check`). *)
  fun base_grading_vector (g: group) : ratvec =
    parseRatvecText (AtlasFFI.atlas_group_base_grading_vector_text g)

  (* `rho_check(rd)` for a complex root datum. *)
  fun rho_check_rootdatum (rd: rootdatum) : ratvec =
    parseRatvecText (AtlasFFI.atlas_rootdatum_rho_check_text rd)

  (* `.at`: `square(RealForm G) = (base_grading_vector(G)+rho_check(G))%1`. *)
  fun square (g: group) : ratvec =
    ratvecMod (ratvecAdd (base_grading_vector g, rho_check g), 1)

  (* Contragredient of a parameter. Returns a new owned parameter handle. *)
  fun contragredient (p: param) : param =
    let
      val q = AtlasFFI.atlas_param_contragredient p
    in
      if q = Foreign.Memory.null then
        raise Fail ("Parameters.contragredient: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q
    end
end

