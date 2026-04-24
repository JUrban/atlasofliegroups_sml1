use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigRat.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/Hermitian.sml";
use "atlas-scripts-sml/convert_c_form.sml";

(*
  File: atlas-scripts-sml/c_form_branch.sml

  Purpose
  - Partial SML translation of `atlas-scripts/c_form_branch.at`.
  - Provides helpers for branching hermitian forms and converting them to
    c-forms via parity shifts controlled by `mu`.

  What is implemented
  - `h_form_branch_irr(p,N)`:
      compute `branch(hermitian_form_irreducible(p), N)`.
  - `c_form_branch_irr(p,N)`:
      convert the branched hermitian form to the c-form by multiplying each
      coefficient by `s^(mu(lkt) - mu(term))` (parity only), exactly as in
      `c_form_branch.at`.

  What is NOT implemented (yet)
  - The `.at` functions `hermitian_form_std`, `h_form_branch_std`, and the
    `ParamPol`/`ParamPol`-summing overloads. Those depend on additional script
    infrastructure not yet ported or exposed via FFI.

  Ownership
  - Functions returning `KTypePol.ktypepol` allocate new handles; callers must
    free them with `KTypePol.free`.
*)
structure C_form_branch = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ktype = AtlasFFI.ktype
  type ktypepol = AtlasFFI.ktypepol

  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then "-" ^ String.extract (s, 1, NONE) else s
    end

  fun vecTextWithHeader (xs: int list) : string =
    Int.toString (length xs) ^ " " ^ String.concatWith " " (List.map intToCText xs)

  fun ktypeOfTerm (g: group, t: KTypePol.term) : ktype =
    KType.newFromXAndLambdaRhoText (g, #x t, vecTextWithHeader (#lambdaRho t))

  fun minHeightTerm (a: KTypePol.term, b: KTypePol.term) : KTypePol.term =
    if #height a <= #height b then a else b

  (* Multiply a split coefficient by `s` (i.e. swap its integer and `s` parts). *)
  fun timesS (e: int, s: int) : int * int = (s, e)

  fun parityDiff (a: BigRat.t, b: BigRat.t) : int =
    let
      val d = BigRat.normalize (BigRat.sub (a, b))
      val () =
        if #den d = 1 then () else raise Fail "C_form_branch: mu difference not integral"
    in
      IntInf.toInt (IntInf.mod (#num d, 2))
    end

  (* Convert a branched hermitian form polynomial into a c-form polynomial by
     per-term parity twisting as in `convert_c_form.at` / `c_form_branch.at`. *)
  fun hermitian_to_c_form (g: group, hf_branch: ktypepol) : ktypepol =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val ts = KTypePol.terms (hf_branch, rank)
    in
      case ts of
        [] => KTypePol.null g
      | t0 :: rest =>
          let
            val lktTerm = List.foldl minHeightTerm t0 rest
            val lkt = ktypeOfTerm (g, lktTerm)
            val a = Convert_c_form.muKType lkt
            val () = KType.free lkt

            fun addOne (term: KTypePol.term, acc: ktypepol) : ktypepol =
              let
                val kt = ktypeOfTerm (g, term)
                val muKt = Convert_c_form.muKType kt
                val odd = (parityDiff (a, muKt) <> 0)
                val (e2, s2) = if odd then timesS (#e term, #s term) else (#e term, #s term)
                val one = KTypePol.singleton (kt, e2, s2)
                val acc2 = KTypePol.add (acc, one)
                val () = KTypePol.free one
                val () = KTypePol.free acc
                val () = KType.free kt
              in
                acc2
              end

            val acc0 = KTypePol.null g
          in
            List.foldl addOne acc0 ts
          end
    end

  fun h_form_branch_irr (p: param, n: int) : ktypepol =
    let
      val hf = Hermitian.hermitian_form_irreducible p
      val bran = (KTypePol.branch (hf, n) handle e => (KTypePol.free hf; raise e))
      val () = KTypePol.free hf
    in
      bran
    end

  fun c_form_branch_irr (p: param, n: int) : ktypepol =
    let
      val g = AtlasFFI.atlas_param_group_handle p
      val hfBran = h_form_branch_irr (p, n)
      val cf = (hermitian_to_c_form (g, hfBran) handle e => (KTypePol.free hfBran; raise e))
      val () = KTypePol.free hfBran
    in
      cf
    end
end
