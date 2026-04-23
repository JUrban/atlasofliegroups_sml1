use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/ParamPol.sml";

(*
  File: atlas-scripts-sml/extParamPol.sml

  Purpose
  - Partial SML translation of `atlas-scripts/extParamPol.at`.
  - The `.at` file defines `ExtParamPol` as a 3-way refinement of `ParamPol`,
    splitting contributions into:
      - “plus”   (delta-fixed, sign = +1)
      - “minus”  (delta-fixed, sign = -1)
      - “induced” (not delta-fixed, or “type = 0” bucket)

  What is implemented here
  - The core datatype `ExtParamPol.t` (three `ParamPol.t` values).
  - Basic algebra: scaling, addition, subtraction, display.
  - Construction from a plain `(Param,type)` triple as in the `.at` overload:
      `extParamPol(p,type)`.

  What is NOT implemented yet
  - The parts of `extParamPol.at` that depend on the Atlas interpreter’s
    extended-parameter (`ExtParam`) infrastructure and the full extended
    deformation pipeline (`deform`, `recursive_deform`, `change_nu`, …).
  - Those require additional SML-side representations and/or C++ shim exports
    beyond the current FFI surface.

  Ownership
  - Each `ParamPol.t` owns the parameters stored in it. Free an `ExtParamPol.t`
    with `ExtParamPol.free`.
*)

structure ExtParamPol = struct
  type param = AtlasFFI.param
  type split = Split.t
  type mat = IntMatrix.mat

  type t = {plus: ParamPol.t, minus: ParamPol.t, ind: ParamPol.t}

  fun null () : t = {plus = ParamPol.create (), minus = ParamPol.create (), ind = ParamPol.create ()}

  fun free (p: t) : unit =
    (ParamPol.free (#plus p); ParamPol.free (#minus p); ParamPol.free (#ind p))

  fun clone (p: t) : t =
    {plus = ParamPol.clone (#plus p), minus = ParamPol.clone (#minus p), ind = ParamPol.clone (#ind p)}

  fun scaleInPlace (p: t, a: split) : unit =
    (ParamPol.scaleInPlace (#plus p, a); ParamPol.scaleInPlace (#minus p, a); ParamPol.scaleInPlace (#ind p, a))

  fun addInPlace (a: t, b: t) : unit =
    (ParamPol.addIntoClone (#plus a, #plus b);
     ParamPol.addIntoClone (#minus a, #minus b);
     ParamPol.addIntoClone (#ind a, #ind b))

  fun subInPlace (a: t, b: t) : unit =
    (ParamPol.subIntoClone (#plus a, #plus b);
     ParamPol.subIntoClone (#minus a, #minus b);
     ParamPol.subIntoClone (#ind a, #ind b))

  fun add (a: t, b: t) : t = let val r = clone a in addInPlace (r, b); r end
  fun sub (a: t, b: t) : t = let val r = clone a in subInPlace (r, b); r end

  (* `.at` overload:
       extParamPol(p,type) = (N+p,N,N) or (N,N+p,N) or (N,N,N+p)
     where `type ∈ {1,~1,0}`.

     SML note: we keep it independent of any RealForm/null_module notion. *)
  fun extParamPol_of_param_type_clone (p: param, ty: int) : t =
    let
      val r = null ()
      val one = Split.one
    in
      if ty = 1 then ParamPol.addTermClone (#plus r, one, p)
      else if ty = ~1 then ParamPol.addTermClone (#minus r, one, p)
      else ParamPol.addTermClone (#ind r, one, p);
      r
    end

  fun extParamPol_of_param_type_move (p: param, ty: int) : t =
    let
      val r = null ()
      val one = Split.one
    in
      if ty = 1 then ParamPol.addTermMove (#plus r, one, p)
      else if ty = ~1 then ParamPol.addTermMove (#minus r, one, p)
      else ParamPol.addTermMove (#ind r, one, p);
      r
    end

  (* Decompose into explicit triples `(coef,param,type)`. Returns fresh param
     clones (caller owns and must free them). *)
  fun toTriplesClone (p: t) : (split * param * int) list =
    let
      fun cloneOne (c, q) = (c, ParamPol.cloneParam q)
      val plus = List.map (fn (c, q) => let val (_, q2) = cloneOne (c, q) in (c, q2, 1) end) (ParamPol.terms (#plus p))
      val minus =
        List.map (fn (c, q) => let val (_, q2) = cloneOne (c, q) in (c, q2, ~1) end) (ParamPol.terms (#minus p))
      val ind = List.map (fn (c, q) => let val (_, q2) = cloneOne (c, q) in (c, q2, 0) end) (ParamPol.terms (#ind p))
    in
      plus @ minus @ ind
    end

  fun display (p: t) : unit =
    (TextIO.print ("plus: " ^ ParamPol.toString (#plus p) ^ "\n");
     TextIO.print ("minus: " ^ ParamPol.toString (#minus p) ^ "\n");
     TextIO.print ("induced: " ^ ParamPol.toString (#ind p) ^ "\n"))

  (*
    Stubs for the deformation-facing API from `extParamPol.at`.
    These are provided so that other ports can typecheck against the names,
    but they currently raise with a clear message.
  *)

  fun deform_unreduced (_: param, _: mat, _: int) : t =
    raise Fail "ExtParamPol.deform_unreduced: not yet ported (requires extended deformation pipeline)"

  fun deform (_: param, _: mat, _: int) : t =
    raise Fail "ExtParamPol.deform: not yet ported (requires extended deformation pipeline)"
end
