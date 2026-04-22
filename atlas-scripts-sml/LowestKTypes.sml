use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypePol.sml";

structure LowestKTypes = struct
  fun intsToText xs = String.concatWith " " (List.map Int.toString xs)

  fun LKTs_of_full_deform (g: AtlasFFI.group, pol: AtlasFFI.ktypepol) : KType.ktype list =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val ts = KTypePol.terms (pol, rank)

      fun minHeight [] = raise Fail "LowestKTypes: empty full_deform"
        | minHeight (t :: rest) =
            List.foldl (fn (u, acc) => Int.min (#height u, acc)) (#height t) rest

      val m = minHeight ts
      val lows = List.filter (fn t => #height t = m) ts

      fun mk t =
        KType.newFromXAndLambdaRhoText (g, #x t, intsToText (#lambdaRho t))
    in
      List.map mk lows
    end

  fun LKTs_param (g: AtlasFFI.group, p: AtlasFFI.param) : KType.ktype list =
    let
      val pol = AtlasFFI.atlas_param_full_deform p
      val () =
        if pol = Foreign.Memory.null then
          raise Fail ("LowestKTypes.LKTs_param: full_deform failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val lows = LKTs_of_full_deform (g, pol)
      val () = KTypePol.free pol
    in
      lows
    end

  fun LKT_param (g: AtlasFFI.group, p: AtlasFFI.param) : KType.ktype =
    (case LKTs_param (g, p) of
       [t] => t
     | ts =>
         (List.app KType.free ts;
          raise Fail ("LowestKTypes.LKT_param: no unique lowest K-type; count=" ^ Int.toString (length ts))))

  fun LKTs_ktype (g: AtlasFFI.group, t: KType.ktype) : KType.ktype list =
    let
      val p = KType.parameter t
      val lows = LKTs_param (g, p)
      val () = AtlasFFI.atlas_param_free p
    in
      lows
    end

  fun LKT_ktype (g: AtlasFFI.group, t: KType.ktype) : KType.ktype =
    let
      val p = KType.parameter t
      val low = LKT_param (g, p)
      val () = AtlasFFI.atlas_param_free p
    in
      low
    end
end

