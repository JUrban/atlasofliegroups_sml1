use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/parameters.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/Hermitian.sml";
use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/complementary_series.sml

  Purpose
  - Standard ML translation of `atlas-scripts/complementary_series.at`.
  - Computes a canonical “endpoint” on a complementary-series line by:
      - first projecting to `nu=0`,
      - then considering the 1-parameter family scaling `nu` along `rho`,
      - and selecting the smallest reducibility point (if any).

  Atlas correspondence
  - The `.at` code (roughly) does:
      p0 = first_param(finalize(p*0))         (* final parameter at nu=0 *)
      q  = parameter(x(p0), lambda(p0), rho(G))
      rp = reducibility_points(q)
      if #rp=0 then (false,p0) else (true, q*rp[0])

  Notes on this port
  - We use the C++ shim’s `atlas_param_scale(p,num,den)` to implement `p*t`.
  - We normalize after construction/scale with `atlas_param_normalise`.
  - “Finalize”/`first_param` is implemented by extracting the first nonzero
    final term via `ParamFinals.finals` if needed.

  Ownership
  - All functions returning a `param` return an owned handle; callers must free
    it with `AtlasFFI.atlas_param_free`.
*)

structure ComplementarySeries = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec

  fun fail where' msg = raise Fail ("ComplementarySeries." ^ where' ^ ": " ^ msg)

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

  fun ratvecToText (u: ratvec) : string =
    let
      val u = Lattice.ratvecNormalize u
    in
      Int.toString (#den u) ^ " " ^ String.concatWith " " (List.map Int.toString (#nums u))
    end

  fun normalizeMove (p: param) : param =
    let
      val q = AtlasFFI.atlas_param_normalise p
      val () = AtlasFFI.atlas_param_free p
    in
      if q = Foreign.Memory.null then
        fail "normalizeMove" ("normalise failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q
    end

  (* Choose `first_param(finalize(p))` analogue: if `p` is final return it,
     otherwise compute `finals(p)` and return the first term with nonzero mult. *)
  fun firstFinalTermMove (p: param) : param =
    if AtlasFFI.atlas_param_is_final p = 1 then
      p
    else
      let
        val finals = ParamFinals.finals p
        val () = AtlasFFI.atlas_param_free p
        fun pick [] = fail "firstFinalTermMove" "no final terms"
          | pick ((q, mult) :: rest) =
              if mult = 0 then (AtlasFFI.atlas_param_free q; pick rest)
              else
                (List.app (fn (r, _) => AtlasFFI.atlas_param_free r) rest; q)
      in
        pick finals
      end

  (* Parse `reducibility_points` text: `k n1 d1 n2 d2 ...`. *)
  fun parseReducibilityPointsText (s: string) : (int * int) list =
    let
      val ns = Lattice.parseInts s
    in
      case ns of
        k :: rest =>
          if k < 0 orelse length rest <> 2 * k then
            fail "parseReducibilityPointsText" "bad reducibility_points encoding"
          else
            let
              fun one i =
                let
                  val num = List.nth (rest, 2 * i)
                  val den = List.nth (rest, 2 * i + 1)
                in
                  (num, den)
                end
            in
              List.tabulate (k, one)
            end
      | _ => fail "parseReducibilityPointsText" "truncated reducibility_points"
    end

  (* `end_of_complementary_series(p)` from `complementary_series.at`. *)
  fun end_of_complementary_series (pIn: param) : bool * param =
    let
      val g = AtlasFFI.atlas_param_group_handle pIn

      (* Step 1: project to `nu=0`, normalize, and pick a final term. *)
      val p0Scaled = AtlasFFI.atlas_param_scale (pIn, 0, 1)
      val () =
        if p0Scaled = Foreign.Memory.null then
          fail "end_of_complementary_series" (AtlasFFI.atlas_last_error ())
        else
          ()
      val p0 = firstFinalTermMove (normalizeMove p0Scaled)

      (* Step 2: build q with the same `(x,lambda)` but `nu=rho(G)`. *)
      val x = AtlasFFI.atlas_param_x p0
      val lambda = Parameters.parseRatvecText (AtlasFFI.atlas_param_lambda_text p0)
      val rho = Parameters.parseRatvecText (AtlasFFI.atlas_group_rho_text g)

      val pQ0 =
        AtlasFFI.atlas_param_new_from_lambda_nu_text
          ( g
          , x
          , intsToCText (#nums (Lattice.ratvecNormalize lambda))
          , #den (Lattice.ratvecNormalize lambda)
          , intsToCText (#nums (Lattice.ratvecNormalize rho))
          , #den (Lattice.ratvecNormalize rho)
          )
      val () =
        if pQ0 = Foreign.Memory.null then
          (AtlasFFI.atlas_param_free p0;
           fail "end_of_complementary_series" ("param construction failed: " ^ AtlasFFI.atlas_last_error ()))
        else
          ()
      val q = normalizeMove pQ0

      val rp = parseReducibilityPointsText (AtlasFFI.atlas_param_reducibility_points_text q)
    in
      case rp of
        [] => (AtlasFFI.atlas_param_free q; (false, p0))
      | (num, den) :: _ =>
          let
            val qScaled = AtlasFFI.atlas_param_scale (q, num, den)
            val () =
              if qScaled = Foreign.Memory.null then
                fail "end_of_complementary_series" (AtlasFFI.atlas_last_error ())
              else
                ()
            val q2 = normalizeMove qScaled
            val () = AtlasFFI.atlas_param_free q
            val () = AtlasFFI.atlas_param_free p0
          in
            (true, q2)
          end
    end

  (* Port of `test_endpoint_complementary_series` (prints and returns purity data).
     Returns owned `Param` handles in the output list; callers must free them. *)
  fun test_endpoint_complementary_series (ps: param list) : (param * (int * int * int)) list =
    let
      fun one p =
        let
          val (deformable, q) = end_of_complementary_series p
        in
          if not deformable then
            (AtlasFFI.atlas_param_free q; NONE)
          else
            let
              val g = AtlasFFI.atlas_param_group_handle q
              val pol = Hermitian.hermitian_form_irreducible q
              val r = AtlasFFI.atlas_group_rank g
              val purity = KTypePol.purityCounts (pol, r)
              val () = KTypePol.free pol
            in
              SOME (q, purity)
            end
        end
    in
      List.mapPartial one ps
    end
end
