use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/KType.sml

  Purpose
  - Thin SML wrapper around the Atlas C++ `KType` handle and related operations.

  Ownership
  - `type ktype = AtlasFFI.ktype` is an opaque handle; free with `KType.free`.
  - Functions returning new `param`/`ktypepol` handles transfer ownership to the caller.
*)
structure KType = struct
  type ktype = AtlasFFI.ktype
  type param = AtlasFFI.param
  type group = AtlasFFI.group
  type ktypepol = AtlasFFI.ktypepol

  (* Free a K-type handle. *)
  fun free (t: ktype) : unit = AtlasFFI.atlas_ktype_free t

  (* Construct the (lowest) K-type attached to a parameter, via Atlas. *)
  fun ofParam (p: param) : ktype =
    let
      val t = AtlasFFI.atlas_param_K_type p
      val () =
        if t = Foreign.Memory.null then
          raise Fail ("KType.ofParam: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      t
    end

  (* Convert a K-type back to its defining parameter (caller owns result). *)
  fun parameter (t: ktype) : param =
    let
      val p = AtlasFFI.atlas_ktype_parameter t
      val () =
        if p = Foreign.Memory.null then
          raise Fail ("KType.parameter: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      p
    end

  (* Whether the K-type is final. *)
  fun isFinal (t: ktype) : bool = (AtlasFFI.atlas_ktype_is_final t = 1)

  (* KGB index associated to the K-type. *)
  fun x (t: ktype) : int =
    let
      val n = AtlasFFI.atlas_ktype_x t
    in
      if n < 0 then raise Fail ("KType.x: failed: " ^ AtlasFFI.atlas_last_error ()) else n
    end

  (* Height of the K-type. *)
  fun height (t: ktype) : int =
    let
      val h = AtlasFFI.atlas_ktype_height t
    in
      if h < 0 then raise Fail ("KType.height: failed: " ^ AtlasFFI.atlas_last_error ()) else h
    end

  (* Lambda+rho text attached to the K-type (Atlas format). *)
  fun lambdaRhoText (t: ktype) : string =
    let
      val s = AtlasFFI.atlas_ktype_lambda_rho_text t
    in
      if s = "-1" then raise Fail ("KType.lambdaRhoText: failed: " ^ AtlasFFI.atlas_last_error ()) else s
    end

  (* Construct a K-type given its KGB index and lambda+rho text. *)
  fun newFromXAndLambdaRhoText (g: group, x: int, lambdaRhoText: string) : ktype =
    let
      val t = AtlasFFI.atlas_ktype_new_from_x_lambda_rho_text (g, x, lambdaRhoText)
      val () =
        if t = Foreign.Memory.null then
          raise Fail ("KType.newFromXAndLambdaRhoText: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      t
    end

  (* Compute the K-type formula polynomial up to a cutoff. *)
  fun K_type_formula (t: ktype, cutoff: int) : ktypepol =
    let
      val pol = AtlasFFI.atlas_ktype_K_type_formula (t, cutoff)
      val () =
        if pol = Foreign.Memory.null then
          raise Fail ("KType.K_type_formula: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      pol
    end

  (*
    Next-to-lowest K-type in the standard module with lowest K-type `t`.

    Atlas correspondence
    - Mirrors `next_to_lowest(KType mu)` from `FPP_faces_herm.at`:
      search for the first cutoff where `K_type_formula_to_ht(mu,cutoff)-mu`
      is nonzero, and return the lowest-height term above `mu`.

    Ownership
    - Returns a fresh `ktype` handle; caller must free it with `free`.
  *)
  fun nextToLowest (t: ktype) : ktype =
    let
      val u = AtlasFFI.atlas_ktype_next_to_lowest t
      val () =
        if u = Foreign.Memory.null then
          raise Fail ("KType.nextToLowest: failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
    in
      u
    end
end
