use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/parameters.sml";

(*
  File: atlas-scripts-sml/complex.sml

  Purpose
  - Partial Standard ML translation of `atlas-scripts/complex.at`.
  - The original `.at` file is a mixed collection of:
      - basic vector/matrix slicing utilities for the “complex group as a real
        group” situation (rank = 2m),
      - predicates detecting the “complex” condition on a (RootDatum, delta),
      - and a larger set of routines that depend on many other interpreter-side
        modules (cells, W-reps, integrality data, etc.).
  - This SML port focuses on the small, broadly reusable pieces that are
    dependency-light and useful for other translations.

  What is implemented here
  - Slicing helpers:
      `left_vec`, `right_vec`, `left_ratvec`, `right_ratvec`
      `concatenate_vec`, `concatenate_ratvec`
      `up_right_corner`, `up_left_corner`
  - “Complex” test:
      `is_complex_rootdatum(rd, delta)`
      `is_complex_group(g)` using `distinguished_involution(g)` as `delta`
  - KGB helper:
      `left_w(g,x)` (the `w` block extracted from the KGB involution matrix)
  - Parameter projections for a complex group (as in `complex.at`):
      `mu_C(p)`, `nu_C(p)`, `gamma_L(p)`, `gamma_R(p)`

  Not implemented (yet)
  - `is_strictly_complex`, `K_int`, `left_W`, `diag_W`, `parameter_w`, and the
    cell/Weyl-group-algebra constructions from the latter part of `complex.at`.

  Conventions / representations
  - Integer matrices/vectors are SML lists in row-major form (as elsewhere in
    `atlas-scripts-sml/`).
  - A rational vector (`ratvec`) is `{den:int, nums:int list}`.
*)

structure Complex = struct
  type vec = int list
  type mat = Lattice.mat
  type ratvec = Lattice.ratvec
  type rootdatum = RootDatum.t
  type group = AtlasFFI.group
  type param = AtlasFFI.param

  fun fail where' msg = raise Fail ("Complex." ^ where' ^ ": " ^ msg)

  fun left_vec (v: vec) : vec = List.take (v, length v div 2)
  fun right_vec (v: vec) : vec = List.drop (v, length v div 2)

  fun left_ratvec (u: ratvec) : ratvec =
    {den = #den u, nums = left_vec (#nums u)}

  fun right_ratvec (u: ratvec) : ratvec =
    {den = #den u, nums = right_vec (#nums u)}

  fun concatenate_vec (a: vec, b: vec) : vec = a @ b

  (* Concatenate two rational vectors, allowing different denominators by
     scaling to a common denominator. *)
  fun concatenate_ratvec (a: ratvec, b: ratvec) : ratvec =
    let
      val a = Lattice.ratvecNormalize a
      val b = Lattice.ratvecNormalize b
      val da = #den a
      val db = #den b
      val g = Lattice.gcd (da, db)
      val lcm = da * (db div g)
      val mulA = lcm div da
      val mulB = lcm div db
      val nums = (List.map (fn x => x * mulA) (#nums a)) @ (List.map (fn x => x * mulB) (#nums b))
    in
      Lattice.ratvecNormalize {den = lcm, nums = nums}
    end

  fun submatrix (m: mat, r0: int, rLen: int, c0: int, cLen: int) : mat =
    let
      val (nRows, nCols) = Lattice.matShape m
      val () =
        if r0 < 0 orelse c0 < 0 orelse rLen < 0 orelse cLen < 0 then
          fail "submatrix" "negative slice"
        else if r0 + rLen > nRows orelse c0 + cLen > nCols then
          fail "submatrix" "slice out of bounds"
        else
          ()
      fun row i =
        List.take (List.drop (List.nth (m, r0 + i), c0), cLen)
    in
      List.tabulate (rLen, row)
    end

  (* Upper right-hand corner of an even square matrix:
       up_right_corner(M) = M[:h, h:] where h = n/2. *)
  fun up_right_corner (m: mat) : mat =
    let
      val (r, c) = Lattice.matShape m
      val () = if r = c then () else fail "up_right_corner" "matrix is not square"
      val () = if r mod 2 = 0 then () else fail "up_right_corner" "odd size matrix"
      val h = r div 2
    in
      submatrix (m, 0, h, h, h)
    end

  fun up_left_corner (m: mat) : mat =
    let
      val (r, c) = Lattice.matShape m
      val () = if r = c then () else fail "up_left_corner" "matrix is not square"
      val () = if r mod 2 = 0 then () else fail "up_left_corner" "odd size matrix"
      val h = r div 2
    in
      submatrix (m, 0, h, 0, h)
    end

  (* `complex.at`: is_complex(ic) <=> <alpha^vee_i, delta(alpha_i)> = 0 for all simple roots. *)
  fun is_complex_rootdatum (rd: rootdatum, delta: mat) : bool =
    let
      val simpleRoots = RootDatum.simpleRootsCols rd
      val simpleCoroots = RootDatum.simpleCorootsCols rd
      val () =
        if length simpleRoots = length simpleCoroots then ()
        else fail "is_complex_rootdatum" "simple roots/coroots mismatch"
      fun ok (alpha, alphav) =
        let
          val dAlpha = Lattice.matVecMulInt delta alpha
        in
          RootDatum.dot (alphav, dAlpha) = 0
        end
    in
      List.all ok (ListPair.zipEq (simpleRoots, simpleCoroots))
    end

  (* Interpret the group’s distinguished involution as the delta for the complex test. *)
  fun is_complex_group (g: group) : bool =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          fail "is_complex_group" ("rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      (* The distinguished involution is printed in Atlas “int matrix text”
         format: `n m ...` (row-major). *)
      fun parseSquareMatText s : mat =
        let
          val ns = Lattice.parseInts s
        in
          case ns of
            n :: m :: rest =>
              let
                val need = n * m
                val () =
                  if n < 0 orelse m < 0 then fail "is_complex_group" "negative delta shape"
                  else if n <> m then fail "is_complex_group" "delta not square"
                  else if length rest <> need then fail "is_complex_group" "delta truncated"
                  else ()
                fun row i = List.take (List.drop (rest, i * m), m)
              in
                List.tabulate (n, row)
              end
          | _ => fail "is_complex_group" "bad delta text"
        end
      val delta = parseSquareMatText (AtlasFFI.atlas_group_distinguished_involution_text g)
      val ans =
        (is_complex_rootdatum (rd, delta)
         handle e => (AtlasFFI.atlas_rootdatum_free rd; raise e))
      val () = AtlasFFI.atlas_rootdatum_free rd
    in
      ans
    end

  (* `complex.at`: for a complex inner class, `left_w(x) = up_right_corner(involution(x))`. *)
  fun left_w (g: group, x: int) : mat =
    up_right_corner (Parameters.theta_matrix (g, x))

  fun parseRatvecText (s: string) : ratvec = Parameters.parseRatvecText s

  fun ratvecNeg (u: ratvec) : ratvec =
    let
      val u = Lattice.ratvecNormalize u
    in
      {den = #den u, nums = List.map (fn a => ~a) (#nums u)}
    end

  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec = Parameters.ratvecAdd (u, v)
  fun ratvecSub (u: ratvec, v: ratvec) : ratvec = ratvecAdd (u, ratvecNeg v)

  fun ratvecAsVec (u: ratvec) : vec =
    (case Lattice.ratvecToIntegral u of
       SOME v => v
     | NONE => fail "ratvecAsVec" "expected integral ratvec")

  fun ratvecFromVec (v: vec) : ratvec = {den = 1, nums = v}

  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  fun intsToCText xs = String.concatWith " " (List.map intToCText xs)

  fun paramDominant (p: param) : param =
    let
      val q = AtlasFFI.atlas_param_normalise p
      val () = AtlasFFI.atlas_param_free p
    in
      if q = Foreign.Memory.null then
        fail "paramDominant" ("normalise failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q
    end

  (* Extremal weight of LKT of parameter `p` for a complex group:
       mu_C(p) = left(lambda(p)) + w*right(lambda(p)) *)
  fun mu_C (p: param) : vec =
    let
      val g = AtlasFFI.atlas_param_group_handle p
      val x = AtlasFFI.atlas_param_x p
      val w = left_w (g, x)
      val lam = ratvecAsVec (parseRatvecText (AtlasFFI.atlas_param_lambda_text p))
      val leftLam = left_vec lam
      val rightLam = right_vec lam
      val wRight = Lattice.matVecMulInt w rightLam
    in
      ListPair.mapEq (op +) (leftLam, wRight)
    end

  (* A-parameter part:
       nu_C(p) = left(nu(p)) - w*right(nu(p)) *)
  fun nu_C (p: param) : ratvec =
    let
      val g = AtlasFFI.atlas_param_group_handle p
      val x = AtlasFFI.atlas_param_x p
      val w = left_w (g, x)
      val nu = parseRatvecText (AtlasFFI.atlas_param_nu_text p)
      val leftNu = left_ratvec nu
      val rightNu = right_ratvec nu
      val wRight = Lattice.matVecMulRatvec w rightNu
    in
      ratvecSub (leftNu, wRight)
    end

  fun gamma_L (p: param) : ratvec =
    let
      val mu = ratvecFromVec (mu_C p)
      val nu = nu_C p
    in
      Lattice.ratvecScale (ratvecAdd (mu, nu), 1, 2)
    end

  fun gamma_R (p: param) : ratvec =
    let
      val mu = ratvecFromVec (mu_C p)
      val nu = nu_C p
    in
      Lattice.ratvecScale (ratvecSub (mu, nu), 1, 2)
    end

  (* Construct a parameter of a complex group by (gamma_L, gamma_R), following `complex.at`.
     This is a convenience for scripts; it assumes `g` is the complex-group real form and
     uses `x=0` (the distinguished KGB element). *)
  fun parameter_g (g: group, gammaL: ratvec, gammaR: ratvec) : param =
    let
      val gammaSum = ratvecAdd (gammaL, gammaR)
      val leftRank = length (#nums (Lattice.ratvecNormalize gammaL))
      val lambda0 = concatenate_vec (ratvecAsVec gammaSum, List.tabulate (leftRank, fn _ => 0))

      val rho = parseRatvecText (AtlasFFI.atlas_group_rho_text g)
      val rhoL = ratvecAsVec (left_ratvec rho)
      val rhoR = ratvecAsVec (right_ratvec rho)
      val rhoAdjust = concatenate_vec (rhoL, List.map (fn a => ~a) rhoR)

      val lambda = ListPair.mapEq (op +) (lambda0, rhoAdjust)

      val diff = ratvecSub (gammaL, gammaR)
      val nuL = Lattice.ratvecScale (diff, 1, 2)
      val nu = concatenate_ratvec (nuL, ratvecNeg nuL)

      val p0 =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          (g, 0, intsToCText lambda, 1, intsToCText (#nums nu), #den (Lattice.ratvecNormalize nu))
      val () =
        if p0 = Foreign.Memory.null then
          fail "parameter_g" ("param construction failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      paramDominant p0
    end

  (* Construct a parameter of a complex group by (mu,nu), following `complex.at`. *)
  fun parameter_m (g: group, mu: vec, nu: ratvec) : param =
    let
      val n = length mu
      val nu2 = Lattice.ratvecScale (nu, 1, 2)
      val nuFull = concatenate_ratvec (nu2, ratvecNeg nu2)
      val lambda = concatenate_vec (mu, List.tabulate (n, fn _ => 0))

      val p0 =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          (g, 0, intsToCText lambda, 1, intsToCText (#nums nuFull), #den (Lattice.ratvecNormalize nuFull))
      val () =
        if p0 = Foreign.Memory.null then
          fail "parameter_m" ("param construction failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      paramDominant p0
    end

  fun g_parameter (p: param) : group * ratvec * ratvec = (AtlasFFI.atlas_param_group_handle p, gamma_L p, gamma_R p)
  fun m_parameter (p: param) : group * vec * ratvec = (AtlasFFI.atlas_param_group_handle p, mu_C p, nu_C p)
end
