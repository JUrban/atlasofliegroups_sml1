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
      - `unnormalized_torus_factor(g,x)` as in `parameters.at`
      - `theta_matrix(g,x)` (KGB involution matrix)
      - `rho_check(g)` and `base_grading_vector(g)` for a real group `g`
      - `rho_check_rootdatum(rd)` for a complex `RootDatum`
      - `square(g)` (the `parameters.at` `square(RealForm)` definition)
      - `square_unnormalized`, `square_normalized`, `square2`
      - `square_is_central(g,x)` (sanity check predicate)
      - `nu(gamma,theta)` and `nu(gamma,g,x)` helpers
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
  type mat = Lattice.mat

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

  (* Parse a `rank` followed by `rank*rank` row-major entries. *)
  fun parseInvolutionMatrixText s : mat =
    let
      val ns = Lattice.parseInts s
    in
      case ns of
        rank :: rest =>
          let
            val need = rank * rank
            val () =
              if rank < 0 then
                raise Fail "Parameters.parseInvolutionMatrixText: negative rank"
              else if length rest <> need then
                raise Fail "Parameters.parseInvolutionMatrixText: bad size"
              else
                ()
            fun row i = List.take (List.drop (rest, i * rank), rank)
          in
            List.tabulate (rank, row)
          end
      | _ => raise Fail "Parameters.parseInvolutionMatrixText: empty"
    end

  (* Parse Atlas column-vector text: `count dim ...` (column-major data). *)
  fun parseColumnVectorsText s : int list list =
    let
      val ns = Lattice.parseInts s
    in
      case ns of
        count :: dim :: rest =>
          let
            val need = count * dim
            val () =
              if count < 0 orelse dim < 0 then raise Fail "Parameters.parseColumnVectorsText: negative header"
              else if length rest <> need then raise Fail "Parameters.parseColumnVectorsText: truncated vectors"
              else ()
            fun vec j = List.take (List.drop (rest, j * dim), dim)
          in
            List.tabulate (count, vec)
          end
      | _ => raise Fail "Parameters.parseColumnVectorsText: truncated header"
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

  fun matSub (a: mat, b: mat) : mat =
    ListPair.mapEq (fn (ra, rb) => ListPair.mapEq (op -) (ra, rb)) (a, b)

  fun matTranspose (a: mat) : mat =
    let
      val (n, m) = Lattice.matShape a
      fun entry (i, j) = List.nth (List.nth (a, i), j)
      fun row j = List.tabulate (n, fn i => entry (i, j))
    in
      List.tabulate (m, row)
    end

  (* Right-multiply a rational vector (as a row vector) by an integer matrix:
       u * m  =  (m^T) * u   (as a column multiplication).
     This matches `ratvec * mat` expressions in the `.at` scripts. *)
  fun ratvecRightMulMat (u: ratvec, m: mat) : ratvec =
    Lattice.matVecMulRatvec (matTranspose m) u

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

  (* `theta(x)` for KGB element `x`, made explicit in `g`.
     Returns the involution matrix as a row-major integer matrix. *)
  fun theta_matrix (g: group, x: kgbelt) : mat =
    parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))

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

  (* `.at`: `unnormalized_torus_factor(x) = (torus_factor(x)+rho_check(G))/2`. *)
  fun unnormalized_torus_factor (g: group, x: kgbelt) : ratvec =
    Lattice.ratvecScale (ratvecAdd (torus_factor (g, x), rho_check g), 1, 2)

  (* Port of `square_unnormalized` from `parameters.at`:
       (v*(1+theta)+rho_check*(1-theta)/2)%1
     where `v` is the *unnormalized* torus factor. *)
  fun square_unnormalized (rhoCheck: ratvec, theta: mat, v: ratvec) : ratvec =
    let
      val rhoCheck = Lattice.ratvecNormalize rhoCheck
      val v = Lattice.ratvecNormalize v
      val rank = length (#nums v)
      val () =
        if length (#nums rhoCheck) = rank then ()
        else raise Fail "Parameters.square_unnormalized: length mismatch"
      val id = Lattice.identity rank
      val onePlus = Lattice.matAdd (id, theta)
      val oneMinus = matSub (id, theta)
      val term1 = ratvecRightMulMat (v, onePlus)
      val term2 = Lattice.ratvecScale (ratvecRightMulMat (rhoCheck, oneMinus), 1, 2)
    in
      ratvecMod (ratvecAdd (term1, term2), 1)
    end

  (* Port of `square_normalized` from `parameters.at`:
       square_unnormalized(ic,theta,(tf+rho_check(ic))/2) *)
  fun square_normalized (rhoCheck: ratvec, theta: mat, torusFactor: ratvec) : ratvec =
    square_unnormalized (rhoCheck, theta, Lattice.ratvecScale (ratvecAdd (torusFactor, rhoCheck), 1, 2))

  (* Port of `square2` from `parameters.at`:
       (tf*(1+theta)+2*rho_check(ic))%2 *)
  fun square2 (rhoCheck: ratvec, theta: mat, torusFactor: ratvec) : ratvec =
    let
      val torusFactor = Lattice.ratvecNormalize torusFactor
      val rank = length (#nums torusFactor)
      val id = Lattice.identity rank
      val onePlus = Lattice.matAdd (id, theta)
      val term1 = ratvecRightMulMat (torusFactor, onePlus)
      val term2 = Lattice.ratvecScale (Lattice.ratvecNormalize rhoCheck, 2, 1)
    in
      ratvecMod (ratvecAdd (term1, term2), 2)
    end

  (* `.at`: `nu(gamma,x) = (1-theta(x))*gamma/2`. *)
  fun nu (gamma: ratvec, theta: mat) : ratvec =
    let
      val gamma = Lattice.ratvecNormalize gamma
      val rank = length (#nums gamma)
      val id = Lattice.identity rank
      val oneMinus = matSub (id, theta)
    in
      Lattice.ratvecScale (Lattice.matVecMulRatvec oneMinus gamma, 1, 2)
    end

  fun nu_kgb (gamma: ratvec, g: group, x: kgbelt) : ratvec = nu (gamma, theta_matrix (g, x))

  (* `.at`: `square_is_central(ic,theta,tf)` sanity check.
     Uses the group’s simple roots and tests integrality of `sq*alpha_i`. *)
  fun square_is_central (g: group, x: kgbelt) : bool =
    let
      val theta = theta_matrix (g, x)
      val tf = torus_factor (g, x)
      val sq = square_normalized (rho_check g, theta, tf)
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rd = Foreign.Memory.null then
          raise Fail ("Parameters.square_is_central: rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val simpleRoots =
        (parseColumnVectorsText (AtlasFFI.atlas_rootdatum_simple_roots_text rd)
         handle e => (AtlasFFI.atlas_rootdatum_free rd; raise e))
      val () = AtlasFFI.atlas_rootdatum_free rd
      val sq = Lattice.ratvecNormalize sq
      val den = #den sq
      fun isIntDot alpha =
        let
          val n = Lattice.dot (#nums sq, alpha)
        in
          n mod den = 0
        end
    in
      List.all isIntDot simpleRoots
    end

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
