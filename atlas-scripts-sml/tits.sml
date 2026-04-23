use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/weylgroup_at.sml";
use "atlas-scripts-sml/WeylWord.sml";

(*
  File: atlas-scripts-sml/tits.sml

  Purpose
  - SML translation of `atlas-scripts/tits.at` (core group law).
  - Implements the (possibly δ-twisted) Tits group element representation:
      `(rd, torus_part, theta)`
    where:
      - `rd` is the ambient complex `RootDatum`
      - `torus_part` is a rational coweight, taken modulo 1 (coordinatewise)
      - `theta` is an integral invertible matrix giving the action on X^*
        (not necessarily an involution)

  Scope (incremental port)
  - Implemented:
      - identity / delta constructors
      - left/right multiplication by simple generators (σ_s)
      - multiplication `*` and inverse (the “bicycle lemma” formula)
      - multiply by Weyl words (canonical lifts)
      - integer powers
  - Not implemented:
      - `order` computation (needs matrix order utilities and is not used by
        current SML ports)

  Dependencies / correspondence
  - Uses `WeylgroupAT.lengthens` / `lengthens_left` and reflection matrices from
    `atlas-scripts-sml/weylgroup_at.sml`.
  - Uses `RootDatum.posCorootsCols` and sign checks via coroot indices to
    implement the cocycle term in `multiply` (as in `tits.at`).

  Ownership
  - `rd` is a borrowed `RootDatum.t` handle (not owned by `Tits.t`).
  - Callers must keep `rd` alive for the lifetime of any `Tits.t` values.
*)

structure Tits = struct
  type rootdatum = RootDatum.t
  type mat = IntMatrix.mat
  type ratvec = Lattice.ratvec
  type word = WeylWord.t

  type t = {rd: rootdatum, torus_part: ratvec, theta: mat}

  fun fail where' msg = raise Fail ("Tits." ^ where' ^ ": " ^ msg)

  (* Parse `den n1 n2 ... nk`. *)
  fun parseRatvecText (s: string) : ratvec =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("Tits.parseRatvecText: bad int token: " ^ tok)
      val ns = List.map toInt (String.tokens Char.isSpace s)
    in
      case ns of
        den :: nums => Lattice.ratvecNormalize {den = den, nums = nums}
      | _ => raise Fail "Tits.parseRatvecText: empty"
    end

  fun rho_check (rd: rootdatum) : ratvec =
    parseRatvecText (AtlasFFI.atlas_rootdatum_rho_check_text rd)

  (* Coordinatewise reduction modulo 1 (in the `.at` sense). *)
  fun mod1 (u: ratvec) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
      val d = #den u
      fun modPos a =
        let
          val r = a mod d
        in
          if r < 0 then r + d else r
        end
    in
      {den = d, nums = List.map modPos (#nums u)}
    end

  fun ratvecNeg (u: ratvec) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
    in
      {den = #den u, nums = List.map (fn a => ~a) (#nums u)}
    end

  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
      val v = Lattice.ratvecNormalize v
      val du = #den u
      val dv = #den v
      val g = Lattice.gcd (du, dv)
      val aMul = dv div g
      val bMul = du div g
      val den = du * aMul
      val nums = ListPair.mapEq (fn (a, b) => a * aMul + b * bMul) (#nums u, #nums v)
    in
      Lattice.ratvecNormalize {den = den, nums = nums}
    end

  fun ratvecSub (u: ratvec, v: ratvec) : ratvec = ratvecAdd (u, ratvecNeg v)

  fun ratvecScaleInt (u: ratvec, k: int) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
    in
      Lattice.ratvecNormalize {den = #den u, nums = List.map (fn a => k * a) (#nums u)}
    end

  fun ratvecDivInt (u: ratvec, k: int) : ratvec =
    if k = 0 then fail "ratvecDivInt" "divide by zero"
    else
      let
        val u = Lattice.ratvecNormalize u
      in
        Lattice.ratvecNormalize {den = #den u * k, nums = #nums u}
      end

  (* Multiply a column vector by a matrix: `m * v`. *)
  fun ratvecLeftMulMat (m: mat, u: ratvec) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
      val nums = IntMatrix.matVecMul (m, #nums u)
    in
      Lattice.ratvecNormalize {den = #den u, nums = nums}
    end

  (* Multiply a row vector by a matrix: `u * m`. *)
  fun ratvecRightMulMat (u: ratvec, m: mat) : ratvec =
    ratvecLeftMulMat (IntMatrix.transpose m, u)

  (* Torus part addition and mod 1. *)
  fun torus_add_mod1 (u: ratvec, v: ratvec) : ratvec = mod1 (ratvecAdd (u, v))

  fun invUnimod (m: mat) : mat =
    LatticeAT.matInverseUnimodular m

  (* Sign of a coroot vector via its coroot index. *)
  fun is_positive_coroot (rd: rootdatum, alphav: int list) : bool =
    let
      val npr = length (RootDatum.posCorootsCols rd)
      val i = RootDatum.corootIndex (rd, alphav)
    in
      if i = npr then fail "is_positive_coroot" "vector is not a coroot" else i >= 0
    end

  fun is_negative_coroot (rd: rootdatum, alphav: int list) : bool =
    let
      val npr = length (RootDatum.posCorootsCols rd)
      val i = RootDatum.corootIndex (rd, alphav)
    in
      if i = npr then fail "is_negative_coroot" "vector is not a coroot" else i < 0
    end

  fun vecAdd (xs: int list, ys: int list) : int list = ListPair.mapEq (op +) (xs, ys)

  fun vecSum (cols: int list list) : int list =
    (case cols of
       [] => []
     | c0 :: cs => List.foldl vecAdd c0 cs)

  fun tits_identity (rd: rootdatum) : t =
    let
      val r = RootDatum.rank rd
    in
      {rd = rd, torus_part = {den = 1, nums = List.tabulate (r, fn _ => 0)}, theta = IntMatrix.identity r}
    end

  fun tits_delta (rd: rootdatum, delta: mat) : t =
    let
      val r = RootDatum.rank rd
      val () =
        if IntMatrix.matShape delta = (r, r) then () else fail "tits_delta" "delta has wrong shape"
    in
      {rd = rd, torus_part = {den = 1, nums = List.tabulate (r, fn _ => 0)}, theta = delta}
    end

  (*
    Right multiplication by σ_s:
      (v,theta) * σ_s = (v or v + coroot_s*theta^{-1}/2, theta*r_s)
    where the extra term is present exactly when the multiplication shortens.
  *)
  fun right_simple ({rd, torus_part, theta}: t, s: int) : t =
    let
      val invTheta = invUnimod theta
      val theta' = WeylgroupAT.right_reflect (rd, theta, s)
      val v' =
        if WeylgroupAT.lengthens (rd, theta, s) then
          torus_part
        else
          let
            val alphav = WeylgroupAT.simple_coroot (rd, s)
            val addv = ratvecDivInt (ratvecRightMulMat ({den = 1, nums = alphav}, invTheta), 2)
          in
            ratvecAdd (torus_part, addv)
          end
    in
      {rd = rd, torus_part = mod1 v', theta = theta'}
    end

  (*
    Left multiplication by σ_s:
      σ_s * (v,theta) = (s(v) or s(v)+coroot_s/2, r_s*theta)
  *)
  fun left_simple (s: int, ({rd, torus_part, theta}: t)) : t =
    let
      val theta' = WeylgroupAT.left_reflect (rd, s, theta)
      val v1 = {den = #den torus_part, nums = WeylgroupAT.coreflect_simple (rd, #nums torus_part, s)}
      val v' =
        if WeylgroupAT.lengthens_left (rd, s, theta) then
          v1
        else
          ratvecAdd (v1, ratvecDivInt ({den = 1, nums = WeylgroupAT.simple_coroot (rd, s)}, 2))
    in
      {rd = rd, torus_part = mod1 v', theta = theta'}
    end

  (* Left multiply by a Weyl word (simple reflections), matching `tits.at`. *)
  fun left_word (w: word, xi: t) : t =
    List.foldl (fn (s, acc) => left_simple (s, acc)) xi (List.rev w)

  (* Right multiply by a Weyl word, matching `tits.at`. *)
  fun right_word (xi: t, w: word) : t =
    List.foldl (fn (s, acc) => right_simple (acc, s)) xi w

  (* Right multiply by exp(v)σ_s where v is a torus part. *)
  fun right_torus_simple (xi: t, v: ratvec, s: int) : t =
    let
      val xi1 = right_simple (xi, s)
      val invTheta = invUnimod (#theta xi) (* uses original theta as in `tits.at` *)
      val vAct = ratvecRightMulMat (v, invTheta)
    in
      {rd = #rd xi1, torus_part = torus_add_mod1 (vAct, #torus_part xi1), theta = #theta xi1}
    end

  (* Left multiply by exp(v)σ_s. *)
  fun left_torus_simple (v: ratvec, s: int, xi: t) : t =
    let
      val xi1 = left_simple (s, xi)
    in
      {rd = #rd xi1, torus_part = torus_add_mod1 (#torus_part xi1, v), theta = #theta xi1}
    end

  (*
    General multiplication, as in `tits.at`.

    cocycle term:
      sum( columns_with(is_positive_coroot, (^theta^{-1}) * columns_with(is_negative_coroot, (^eta^{-1})*poscoroots) ) ) / 2
  *)
  fun multiply ({rd, torus_part = v, theta}: t, {rd = rd2, torus_part = u, theta = eta}: t) : t =
    let
      val () = if rd = rd2 then () else fail "multiply" "root data mismatch (pointer inequality)"
      val invTheta = invUnimod theta
      val invEta = invUnimod eta
      val tt1 = IntMatrix.transpose invTheta
      val te1 = IntMatrix.transpose invEta

      val poscrs = RootDatum.posCorootsCols rd
      fun act (m: mat) (c: int list) = IntMatrix.matVecMul (m, c)

      val neg_crs = List.map (act te1) (List.filter (fn c => is_negative_coroot (rd, act te1 c)) poscrs)
      val tt1_neg = List.map (act tt1) neg_crs
      val pos_after = List.filter (fn c => is_positive_coroot (rd, c)) tt1_neg

      val cocycle =
        if null pos_after then {den = 1, nums = List.tabulate (RootDatum.rank rd, fn _ => 0)}
        else ratvecDivInt ({den = 1, nums = vecSum pos_after}, 2)

      val torus = mod1 (ratvecAdd (v, ratvecAdd (ratvecLeftMulMat (tt1, u), cocycle)))
      val th = IntMatrix.matMul (theta, eta)
    in
      {rd = rd, torus_part = torus, theta = th}
    end

  infix 7 *
  val op * = multiply

  (*
    Inverse from `tits.at`:
      inverse(v,theta) = (rho_check*(1-theta)/2 - v*theta, theta^{-1}) mod 1
  *)
  fun inverse ({rd, torus_part = v, theta}: t) : t =
    let
      val invTheta = invUnimod theta
      val rch = rho_check rd
      val oneMinusTheta = IntMatrix.sub (IntMatrix.identity (RootDatum.rank rd), theta)
      val term1 = ratvecDivInt (ratvecRightMulMat (rch, oneMinusTheta), 2)
      val term2 = ratvecRightMulMat (v, theta)
      val torus = mod1 (ratvecSub (term1, term2))
    in
      {rd = rd, torus_part = torus, theta = invTheta}
    end

  (* Canonical lift of a Weyl word to the Tits group. *)
  fun lift (rd: rootdatum, w: word) : t =
    left_word (w, tits_identity rd)

  (* Integer powers. *)
  fun pow (xi: t, n: int) : t =
    if n > 0 then
      let
        fun loop (0, acc) = acc
          | loop (k, acc) = loop (k - 1, acc * xi)
      in
        loop (n - 1, xi)
      end
    else if n < 0 then
      pow (inverse xi, ~n)
    else
      tits_identity (#rd xi)
end

