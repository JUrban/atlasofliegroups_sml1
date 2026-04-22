use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure KType = struct
  type ktype = AtlasFFI.ktype
  type param = AtlasFFI.param
  type group = AtlasFFI.group

  fun free (t: ktype) : unit = AtlasFFI.atlas_ktype_free t

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

  fun isFinal (t: ktype) : bool = (AtlasFFI.atlas_ktype_is_final t = 1)

  fun x (t: ktype) : int =
    let
      val n = AtlasFFI.atlas_ktype_x t
    in
      if n < 0 then raise Fail ("KType.x: failed: " ^ AtlasFFI.atlas_last_error ()) else n
    end

  fun lambdaRhoText (t: ktype) : string =
    let
      val s = AtlasFFI.atlas_ktype_lambda_rho_text t
    in
      if s = "-1" then raise Fail ("KType.lambdaRhoText: failed: " ^ AtlasFFI.atlas_last_error ()) else s
    end

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
end

