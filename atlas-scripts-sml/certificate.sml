use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/Hermitian.sml";
use "atlas-scripts-sml/to_ht.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/FPP_vertices.sml";
use "atlas-scripts-sml/FPP_barycenters_fold.sml";

(*
  File: atlas-scripts-sml/certificate.sml

  Purpose
  - SML translation of `atlas-scripts/certificate.at`.
  - Provides “nonunitarity certificate” routines: for a hermitian (standard,
    final) parameter `p`, decide unitarity, and if nonunitary, return K-types of
    minimal height on which the invariant form is negative.

  Atlas background / intended meaning
  - For a hermitian irreducible parameter `p`, Atlas can compute an invariant
    form on the irreducible (e.g. `hermitian_form_irreducible(p)`), expressed as
    a `KTypePol` with coefficients in split integers `a + s*b`.
  - When the representation is unitary, the form has a definite sign on each
    K-type; in the `.at` ecosystem this is typically detected by a “purity”
    condition on coefficients. When nonunitary, one can often exhibit a low
    height K-type where the form has negative contribution; this serves as a
    certificate of nonunitarity.

  Translation notes
  - The original `.at` file predates the current FPP_global workflows and was
    written for interactive use; this port focuses on correctness and explicit
    resource management (FFI handle ownership).
  - Unlike `.at`, SML does not implicitly carry the group handle inside a
    `param`. We therefore use the FFI helper `atlas_param_group_handle` to
    recover the group for K-type construction.

  Ownership / lifetime
  - Functions that return `KType.ktype list` transfer ownership of those K-type
    handles to the caller; the caller must free them with `KType.free`.
  - All intermediate `param` and `KTypePol` handles are freed internally.
*)

structure Certificate = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec
  type ktype = KType.ktype
  type ktypepol = KTypePol.ktypepol

  fun boolTo01 b = if b then 1 else 0

  fun ratvecToCText (u: ratvec) : string =
    Int.toString (#den u) ^ " " ^ AllParameters.intsToCText (#nums u)

  fun ratvecAdd (u: ratvec, v: ratvec) : ratvec =
    Lattice.ratvecSub (u, Lattice.ratvecScale (v, ~1, 1))

  fun paramRank (p: param) : int =
    let
      val toks = String.tokens Char.isSpace (AtlasFFI.atlas_param_lambda_text p)
    in
      Int.max (0, length toks - 1)
    end

  fun paramGroup (p: param) : group =
    let
      val g = AtlasFFI.atlas_param_group_handle p
    in
      if g = Foreign.Memory.null then
        raise Fail ("Certificate: paramGroup failed: " ^ AtlasFFI.atlas_last_error ())
      else
        g
    end

  (* Build a `KType` handle from a decoded `KTypePol` term. *)
  fun ktypeOfTerm (g: group, t: KTypePol.term) : ktype =
    let
      val coords = #lambdaRho t
      val hdr = Int.toString (length coords)
      val txt = hdr ^ " " ^ AllParameters.intsToCText coords
    in
      KType.newFromXAndLambdaRhoText (g, #x t, txt)
    end

  (* Extract the certificate terms at minimal height, for a given form `pol`.
     Returns `(isUnitary, certificate)` where `certificate` contains K-types of
     minimal height witnessing nonunitarity. *)
  fun nonunitarity_certificate_from_pol (p: param, pol: ktypepol) : bool * ktype list =
    let
      val g = paramGroup p
      val rank = paramRank p
      val ht_p = AtlasFFI.atlas_param_height p
      val ts = KTypePol.terms (pol, rank)

      val (a, b) =
        (case ts of
           [] => (1, 0) (* degenerate; treat as pure integer *)
         | t0 :: _ => (boolTo01 (#e t0 <> 0), boolTo01 (#s t0 <> 0)))

      val () = if a = 1 andalso b = 1 then TextIO.print "The LKT is indefinite.\n" else ()

      fun npart (t: KTypePol.term) : int = b * #e t + a * #s t

      (* First pass: compute the minimal height where negativity appears. We
         mirror the `.at` logic which uses a monotone `ht_lim` guard. *)
      fun minHeight () : int option =
        let
          val ht_lim = ref ht_p
          fun step (t: KTypePol.term, acc: int option) =
            if npart t = 0 then
              acc
            else
              let
                val ht_q = #height t
                val okHeight =
                  if ht_q = ht_p then
                    true
                  else if ht_q > ht_p andalso ht_q >= !ht_lim then
                    (ht_lim := ht_q; true)
                  else
                    false
              in
                if okHeight then
                  case acc of
                    NONE => SOME ht_q
                  | SOME h => SOME (Int.min (h, ht_q))
                else
                  acc
              end
        in
          List.foldl step NONE ts
        end

      val mhOpt = minHeight ()

      fun collectAt (mh: int) : ktype list =
        let
          val ht_lim = ref ht_p
          fun step (t: KTypePol.term, acc: ktype list) =
            if npart t = 0 then
              acc
            else
              let
                val ht_q = #height t
                val okHeight =
                  if ht_q = ht_p then
                    true
                  else if ht_q > ht_p andalso ht_q >= !ht_lim then
                    (ht_lim := ht_q; true)
                  else
                    false
              in
                if okHeight andalso ht_q = mh then
                  ktypeOfTerm (g, t) :: acc
                else
                  acc
              end
        in
          List.rev (List.foldl step [] ts)
        end
    in
      case mhOpt of
        NONE => (true, [])
      | SOME mh => (false, collectAt mh)
    end

  fun nonunitarity_certificate_with_ht_from_pol (p: param, pol: ktypepol) : bool * (ktype * int) list =
    let
      val (ok, ks) = nonunitarity_certificate_from_pol (p, pol)
      val ksHt = List.map (fn t => (t, KType.height t)) ks
    in
      (ok, ksHt)
    end

  (* Public API: compute certificate using the full invariant form. *)
  fun nonunitarity_certificate (p: param) : bool * ktype list =
    let
      val pol = Hermitian.hermitian_form_irreducible p
      val (ok, cert) = nonunitarity_certificate_from_pol (p, pol)
      val () = KTypePol.free pol
    in
      (ok, cert)
    end

  fun nonunitarity_certificate_with_ht (p: param) : bool * (ktype * int) list =
    let
      val pol = Hermitian.hermitian_form_irreducible p
      val (ok, cert) = nonunitarity_certificate_with_ht_from_pol (p, pol)
      val () = KTypePol.free pol
    in
      (ok, cert)
    end

  fun nonunitarity_certificate_to_ht (p: param, ht: int) : bool * ktype list =
    let
      val pol = ToHT.hermitian_form_irreducible_to_ht (p, ht)
      val (ok, cert) = nonunitarity_certificate_from_pol (p, pol)
      val () = KTypePol.free pol
    in
      (ok, cert)
    end

  (* Convenience: show certificates for principal series at FPP vertices. *)
  fun show_certificates_ps (g: group, lam: int list) : unit =
    let
      val ics = FPP_vertices.vertices g
      val xOpen = AtlasFFI.atlas_group_kgb_size g - 1
      val rho = Representations.rho g
      val lambda = ratvecAdd (rho, {den = 1, nums = lam})

      fun showOne (v: ratvec) : unit =
        let
          val p = Representations.parameter (g, xOpen, lambda, v)
          val () =
            if AtlasFFI.atlas_param_is_hermitian p <> 1 then
              ()
            else if AtlasFFI.atlas_param_is_final p <> 1 then
              ()
            else
              let
                val (ok, cert) = nonunitarity_certificate_with_ht p
                fun pr (t, h) =
                  TextIO.print
                    ("  x=" ^ Int.toString (KType.x t) ^ " lambda_rho=" ^ KType.lambdaRhoText t ^ " ht=" ^ Int.toString h ^ "\n")
                val () =
                  if ok then
                    ()
                  else
                    (TextIO.print ("\n" ^ ratvecToCText v ^ "\n"); List.app pr cert)
                val () = List.app (fn (t, _) => KType.free t) cert
              in
                ()
              end
          val () = AtlasFFI.atlas_param_free p
        in
          ()
        end
    in
      List.app showOne ics
    end

  fun show_certificates_spherical (g: group) : unit =
    show_certificates_ps (g, List.tabulate (AtlasFFI.atlas_group_rank g, fn _ => 0))

  (* Accumulate unique K-types from certificates. Duplicate K-types are freed. *)
  fun addUniqueKType (t: ktype, acc: ktype list) : ktype list =
    if List.exists (fn u => AtlasFFI.atlas_ktype_equal (t, u) = 1) acc then
      (KType.free t; acc)
    else
      t :: acc

  fun certificate_list_ps (g: group, lam: int list) : ktype list =
    let
      val ics = FPP_vertices.vertices g
      val xOpen = AtlasFFI.atlas_group_kgb_size g - 1
      val rho = Representations.rho g
      val lambda = ratvecAdd (rho, {den = 1, nums = lam})

      fun step (v: ratvec, acc: ktype list) : ktype list =
        let
          val p = Representations.parameter (g, xOpen, lambda, v)
          val acc2 =
            if AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_final p = 1 then
              let
                val (ok, cert) = nonunitarity_certificate p
                val acc3 = if ok then acc else List.foldl addUniqueKType acc cert
              in
                acc3
              end
            else
              acc
          val () = AtlasFFI.atlas_param_free p
        in
          acc2
        end
    in
      List.rev (List.foldl step [] ics)
    end

  fun certificate_list_spherical (g: group) : ktype list =
    certificate_list_ps (g, List.tabulate (AtlasFFI.atlas_group_rank g, fn _ => 0))

  (* Certificate list for a supplied list of infinitesimal characters. *)
  fun certificate_list (g: group, ics: ratvec list) : ktype list =
    let
      val xOpen = AtlasFFI.atlas_group_kgb_size g - 1
      val rho = Representations.rho g
      fun step (v: ratvec, acc: ktype list) : ktype list =
        let
          val p = Representations.parameter (g, xOpen, rho, v)
          val acc2 =
            if AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_final p = 1 then
              let
                val (ok, cert) = nonunitarity_certificate p
                val acc3 = if ok then acc else List.foldl addUniqueKType acc cert
              in
                acc3
              end
            else
              acc
          val () = AtlasFFI.atlas_param_free p
        in
          acc2
        end
    in
      List.rev (List.foldl step [] ics)
    end

  (* Certificates at all folded-FPP barycenters (flattened). *)
  fun certificate_list_facets (g: group) : ktype list =
    certificate_list (g, FPP_barycenters_fold.barycenters_all g)

  fun show_certificates_facets (g: group) : unit =
    let
      val ics = FPP_barycenters_fold.barycenters_all g
      val xOpen = AtlasFFI.atlas_group_kgb_size g - 1
      val rho = Representations.rho g

      fun showOne (v: ratvec) : unit =
        let
          val p = Representations.parameter (g, xOpen, rho, v)
          val () =
            if AtlasFFI.atlas_param_is_hermitian p <> 1 then
              ()
            else if AtlasFFI.atlas_param_is_final p <> 1 then
              ()
            else
              let
                val (ok, cert) = nonunitarity_certificate_with_ht p
                fun pr (t, h) =
                  TextIO.print
                    ("  x=" ^ Int.toString (KType.x t) ^ " lambda_rho=" ^ KType.lambdaRhoText t ^ " ht=" ^ Int.toString h ^ "\n")
                val () =
                  if ok then
                    ()
                  else
                    (TextIO.print ("\n" ^ ratvecToCText v ^ "\n"); List.app pr cert)
                val () = List.app (fn (t, _) => KType.free t) cert
              in
                ()
              end
          val () = AtlasFFI.atlas_param_free p
        in
          ()
        end
    in
      List.app showOne ics
    end

  (* As `show_certificates_facets`, but only show certificates with minimal
     witness height >= `bd`. *)
  fun show_certificates_facets_bd (g: group, bd: int) : unit =
    let
      val ics = FPP_barycenters_fold.barycenters_all g
      val xOpen = AtlasFFI.atlas_group_kgb_size g - 1
      val rho = Representations.rho g

      fun showOne (v: ratvec) : unit =
        let
          val p = Representations.parameter (g, xOpen, rho, v)
          val () =
            if AtlasFFI.atlas_param_is_hermitian p <> 1 then
              ()
            else if AtlasFFI.atlas_param_is_final p <> 1 then
              ()
            else
              let
                val (ok, cert) = nonunitarity_certificate_with_ht p
                fun pr (t, h) =
                  TextIO.print
                    ("  x=" ^ Int.toString (KType.x t) ^ " lambda_rho=" ^ KType.lambdaRhoText t ^ " ht=" ^ Int.toString h ^ "\n")
                val () =
                  if ok then
                    ()
                  else
                    (case cert of
                       [] => ()
                     | (_, h0) :: _ =>
                         if h0 >= bd then
                           (TextIO.print ("\n" ^ ratvecToCText v ^ "\n"); List.app pr cert)
                         else
                           ())
                val () = List.app (fn (t, _) => KType.free t) cert
              in
                ()
              end
          val () = AtlasFFI.atlas_param_free p
        in
          ()
        end
    in
      List.app showOne ics
    end

  (* `ic_list` from the `.at` script is a combinatorial generator for Sp(2n,R)
     examples; it depends on additional `.at` utilities (e.g. `choices_from`)
     that are not currently ported. Provide a placeholder for now. *)
  fun ic_list (_: int, _: int) : ratvec list =
    raise Fail "Certificate.ic_list: not yet ported (depends on choices_from)"
end
