use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Split.sml";

(*  File: atlas-scripts-sml/ParamPol.sml

    Purpose
    - SML-side replacement for the Atlas interpreter’s `ParamPol` module type:
      finite formal sums of Atlas parameters with split-integer coefficients.
    - This is a foundational datatype for ports of scripts such as
      `K_type_formula.at`, `extParamPol.at`, and bottom-layer pipelines.

    Data model
    - A `ParamPol.t` is a mutable collection of terms:
        Σ_i c_i * p_i
      where `c_i : Split.t` and `p_i : AtlasFFI.param`.
    - Terms with equal parameters are combined using `atlas_param_equal`.
    - Zero coefficients are dropped.

    Ownership
    - The polynomial *owns* all stored `param` handles.
    - `addTermMove` consumes its `param` handle: it either stores it or frees it.
    - `addTermClone` clones the input param, leaving the input handle untouched.
    - Call `free` to free all stored params.

    Performance
    - This initial implementation uses a linear term list and is intended as a
      correctness-first building block. If/when we need large `ParamPol` values,
      we can replace the internal representation with a hashed one.
*)

structure ParamPol = struct
  type param = AtlasFFI.param
  type coef = Split.t
  type term = coef * param

  type t = term list ref

  fun create () : t = ref []

  fun isEmpty (p: t) : bool = null (!p)

  fun terms (p: t) : term list = !p

  fun free (p: t) : unit =
    (List.app (fn (_, q) => AtlasFFI.atlas_param_free q) (!p); p := [])

  fun cloneParam (q: param) : param =
    let
      val q2 = AtlasFFI.atlas_param_clone q
    in
      if q2 = Foreign.Memory.null then
        raise Fail ("ParamPol: param clone failed: " ^ AtlasFFI.atlas_last_error ())
      else
        q2
    end

  fun clone (p: t) : t =
    let
      val q = create ()
      fun addOne (c, r) = q := (c, cloneParam r) :: (!q)
    in
      List.app addOne (!p);
      q
    end

  fun addTermMove (poly: t, c: coef, p: param) : unit =
    if Split.isZero c then
      AtlasFFI.atlas_param_free p
    else
      let
        fun loop ([], acc) =
              (poly := (c, p) :: List.rev acc)
          | loop ((c0, p0) :: rest, acc) =
              if AtlasFFI.atlas_param_equal (p, p0) = 1 then
                let
                  val c1 = Split.add (c0, c)
                  val () = AtlasFFI.atlas_param_free p
                in
                  if Split.isZero c1 then
                    (AtlasFFI.atlas_param_free p0; poly := List.rev acc @ rest)
                  else
                    poly := List.rev acc @ ((c1, p0) :: rest)
                end
              else
                loop (rest, (c0, p0) :: acc)
      in
        loop (!poly, [])
      end

  fun addTermClone (poly: t, c: coef, p: param) : unit =
    addTermMove (poly, c, cloneParam p)

  fun addIntoClone (dst: t, src: t) : unit =
    List.app (fn (c, p) => addTermClone (dst, c, p)) (!src)

  (* Move all terms from `src` into `dst`. After the call, `src` is empty and
     should not be freed (it owns no params anymore). *)
  fun addIntoMove (dst: t, src: t) : unit =
    (List.app (fn (c, p) => addTermMove (dst, c, p)) (!src); src := [])

  fun scaleInPlace (poly: t, a: coef) : unit =
    if Split.isZero a then
      free poly
    else
      let
        fun step ((c, p), acc) =
          let
            val c2 = Split.mul (a, c)
          in
            if Split.isZero c2 then (AtlasFFI.atlas_param_free p; acc) else (c2, p) :: acc
          end
      in
        poly := List.rev (List.foldl step [] (!poly))
      end

  fun subIntoClone (dst: t, src: t) : unit =
    List.app (fn (c, p) => addTermClone (dst, Split.neg c, p)) (!src)

  fun toString (poly: t) : string =
    let
      fun one (c, p) =
        let
          val x = AtlasFFI.atlas_param_x p
          val lam = AtlasFFI.atlas_param_lambda_text p
          val nu = AtlasFFI.atlas_param_nu_text p
        in
          Split.split_factor_format c ^ " * " ^ "{x=" ^ Int.toString x ^ ", lambda=" ^ lam ^ ", nu=" ^ nu ^ "}"
        end
    in
      case !poly of
        [] => "0"
      | ts => String.concatWith " + " (List.map one ts)
    end
end

