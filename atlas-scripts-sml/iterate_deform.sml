use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/ParamPol.sml";

(*
  File: atlas-scripts-sml/iterate_deform.sml

  Purpose
  - SML translation of `atlas-scripts/iterate_deform.at`.
  - Provides utilities for iteratively applying Atlas deformation formulas
    (via KL polynomials) to slide parameters toward `nu = 0`.

  Key primitive: `deform`
  - In the Atlas interpreter, `deform(p) : ParamPol` returns the “split-off”
    virtual module in the deformation unit of `p`.
  - This SML port uses the C++ shim export `atlas_param_deform(p)` which
    mirrors the interpreter’s `deform_wrapper` in `sources/interpreter/atlas-types.w`.
  - The returned coefficients are encoded as an integer `c`, representing the
    split integer `Split_integer(c,~c)` i.e. `c*(1-s)`.

  API overview (ported subset)
  - `weak_lower p` / `lower p`:
      move `p` down to the last reducibility point (or to the last < 1 point).
  - `deform p`:
      compute deformation terms as a `ParamPol.t` with `Split` coefficients.
  - `iterate_deform p`:
      perform the iterative deformation-to-`nu=0` loop from `iterate_deform.at`.

  Ownership
  - All returned `AtlasFFI.param` handles are newly allocated; callers must
    free them with `AtlasFFI.atlas_param_free`.
  - Returned `ParamPol.t` values own their stored params; free with
    `ParamPol.free`.
*)

structure IterateDeform = struct
  type param = AtlasFFI.param
  type poly = ParamPol.t
  type coef = Split.t

  val iterate_deform_debug = ref false

  fun failFFI (where': string) : 'a =
    raise Fail ("IterateDeform." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("IterateDeform: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Parse `den n1 .. nk` as used by `atlas_param_nu_text`. *)
  fun parseRatWeightText (s: string) : {den: int, nums: int list} =
    (case parseInts s of
       den :: rest => {den = den, nums = rest}
     | _ => raise Fail ("IterateDeform: bad ratweight text: " ^ s))

  fun isZeroRatWeight (rw: {den: int, nums: int list}) : bool =
    List.all (fn x => x = 0) (#nums rw)

  fun has_nu0 (p: param) : bool = isZeroRatWeight (parseRatWeightText (AtlasFFI.atlas_param_nu_text p))

  (* Parse `atlas_param_reducibility_points_text`:
       `k a1 b1 ... ak bk` giving rationals `ai/bi`. *)
  fun reducibility_points (p: param) : (int * int) list =
    let
      val ns = parseInts (AtlasFFI.atlas_param_reducibility_points_text p)
    in
      case ns of
        [] => raise Fail "IterateDeform: reducibility_points_text empty"
      | k :: rest =>
          let
            fun loop (0, xs, acc) = (List.rev acc, xs)
              | loop (n, a :: b :: xs, acc) = loop (n - 1, xs, (a, b) :: acc)
              | loop _ = raise Fail "IterateDeform: reducibility_points_text truncated"
            val (pairs, leftover) = loop (k, rest, [])
          in
            if null leftover then pairs else raise Fail "IterateDeform: reducibility_points_text leftover ints"
          end
    end

  fun scale (p: param, num: int, den: int) : param =
    let
      val q = AtlasFFI.atlas_param_scale (p, num, den)
    in
      if q = Foreign.Memory.null then failFFI "scale" else q
    end

  (* `weak_lower` from `.at`: snap to the last reducibility point, or to 0. *)
  fun weak_lower (p: param) : param =
    let
      val rp = reducibility_points p
    in
      case rp of
        [] => scale (p, 0, 1)
      | xs => let val (a, b) = List.last xs in scale (p, a, b) end
    end

  (* `lower` from `.at`: snap to last reducibility point < 1, else to 0. *)
  fun lower (p: param) : param =
    let
      val rp = reducibility_points p
      val rp' =
        (case List.rev rp of
           (1, 1) :: rest => List.rev rest
         | _ => rp)
    in
      case rp' of
        [] => scale (p, 0, 1)
      | xs => let val (a, b) = List.last xs in scale (p, a, b) end
    end

  (* Compute deformation terms as a `ParamPol` with split coefficients `c*(1-s)`. *)
  fun deform (p: param) : poly =
    let
      val h = AtlasFFI.atlas_param_deform p
      val () = if h = Foreign.Memory.null then failFFI "deform" else ()
      val n = AtlasFFI.atlas_paramlist_size h
      val () =
        if n < 0 then
          (AtlasFFI.atlas_paramlist_free h; failFFI "deform/paramlist_size")
        else
          ()

      val out = ParamPol.create ()
      fun one i =
        let
          val c = AtlasFFI.atlas_paramlist_mult (h, i)
          val q = AtlasFFI.atlas_paramlist_get_param_clone (h, i)
          val () = if q = Foreign.Memory.null then failFFI "deform/get_param_clone" else ()
          val coef : coef =
            Split.fromParts (IntInf.fromInt c, IntInf.fromInt (~c))
        in
          ParamPol.addTermMove (out, coef, q)
        end

      val () = List.app one (List.tabulate (n, fn i => i))
      val () = AtlasFFI.atlas_paramlist_free h
    in
      out
    end

  (* `deformation(p) = (lower(p), deform(p))`. *)
  fun deformation (p: param) : param * poly = (lower p, deform p)

  (* Move all terms of `src` into `dst`, scaling coefficients by `a`.
     After the call, `src` is empty and owns no params. *)
  fun addScaledIntoMove (dst: poly, a: coef, src: poly) : unit =
    let
      val ts = ParamPol.terms src
      val () = src := []
      fun one (c, p) = ParamPol.addTermMove (dst, Split.mul (a, c), p)
    in
      List.app one ts
    end

  (*
    iterate_deform

    Port of `iterate_deform (Param p)` from `iterate_deform.at`.

    Returns:
      - `(p0, d_done)` where `p0` has `nu=0` and `d_done` is the accumulated
        polynomial of terms that have reached `nu=0`.
      - `count`: number of deformation steps performed (as in `.at`).
  *)
  fun iterate_deform (p0: param) : (param * poly) * int =
    let
      val pRef = ref (AtlasFFI.atlas_param_clone p0)
      val () = if !pRef = Foreign.Memory.null then failFFI "iterate_deform/clone" else ()

      val d = ParamPol.create ()
      val d_done = ParamPol.create ()
      val count = ref 0

      val p1 = weak_lower (!pRef)
      val () = AtlasFFI.atlas_param_free (!pRef)
      val () = pRef := p1

      fun loop () =
        if ParamPol.isEmpty d andalso has_nu0 (!pRef) then
          ()
        else
          let
            val (new_p, d_new) = deformation (!pRef)
            val () = count := !count + 1
            val () =
              if !iterate_deform_debug then
                print ("[iterate_deform] deforming at nu=" ^ AtlasFFI.atlas_param_nu_text (!pRef) ^ "\n")
              else
                ()
            val () = AtlasFFI.atlas_param_free (!pRef)
            val () = pRef := new_p

            val oldTerms = ParamPol.terms d
            val () = d := []

            fun processTerm ((k, q): coef * param) : unit =
              if has_nu0 q then
                ParamPol.addTermMove (d_done, k, q)
              else
                let
                  val (qLower, qDef) = deformation q
                  val () = count := !count + 1
                  val () = AtlasFFI.atlas_param_free q
                  val () = ParamPol.addTermMove (d_new, k, qLower)
                  val () = addScaledIntoMove (d_new, k, qDef)
                in
                  ()
                end

            val () = List.app processTerm oldTerms
            val () = d := !d_new
            val () = d_new := []
          in
            loop ()
          end

      val () = loop ()
    in
      ((!pRef, d_done), !count)
    end

  (*
    recursive_deform

    Port of `recursive_deform (Param p)` from `iterate_deform.at`.

    Implementation notes
    - This version mirrors the `.at` recursion but is written in an explicit
      ownership style: the recursive worker consumes its parameter handle.
    - The returned polynomial contains only `nu=0` parameters; the returned
      parameter is `p` scaled to `nu=0`.
  *)
  fun recursive_deform (p0: param) : param * poly =
    let
      val empty = ParamPol.create ()

      fun recd (coef: coef, p: param, backtrace: unit -> unit) : param * poly =
        if has_nu0 p then
          (p, ParamPol.create ())
        else if AtlasFFI.atlas_param_is_standard p <> 1 then
          (backtrace ();
           AtlasFFI.atlas_param_free p;
           raise Fail "IterateDeform.recursive_deform: encountered non-standard parameter")
        else
          let
            val rp = reducibility_points p
            val at_nu0 = ParamPol.create ()

            fun processAt (a: int, b: int) : unit =
              let
                val p_at = scale (p, a, b)
                val terms = deform p_at
                val () = AtlasFFI.atlas_param_free p_at

                val ts = ParamPol.terms terms
                val () = terms := []

                fun oneTerm (k, q) =
                  let
                    val c = Split.mul (coef, k)
                    val (q0, nu0_terms) = recd (c, q, backtrace)
                    val () = ParamPol.addIntoMove (at_nu0, nu0_terms)
                    val () = ParamPol.addTermMove (at_nu0, c, q0)
                  in
                    ()
                  end
              in
                List.app oneTerm ts
              end

            val () = List.app processAt (List.rev rp)
            val p_at_0 = scale (p, 0, 1)
            val () = AtlasFFI.atlas_param_free p
          in
            (p_at_0, at_nu0)
          end

      val p = AtlasFFI.atlas_param_clone p0
      val () = if p = Foreign.Memory.null then failFFI "recursive_deform/clone" else ()
    in
      recd (Split.one, p, fn () => ())
    end

  (* `rec_def` from `.at` is not yet ported; add it when a downstream script needs it. *)
  fun rec_def (_: param) : param * poly =
    raise Fail "IterateDeform.rec_def: not yet ported"
end
