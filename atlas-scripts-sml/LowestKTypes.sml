use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";
use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/LowestKTypes.sml

  Purpose
  - Helpers to extract lowest K-types from Atlas deformation polynomials.

  Atlas correspondence
  - Mirrors the `.at`-side usage of `full_deform` and the “height” ordering of
    K-types, returning the terms of minimal height.
*)
structure LowestKTypes = struct
  (* Convert SML `~` negatives to C-style `-` negatives. *)
  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  (* Serialize an int list in the Atlas C++ parser format. *)
  fun intsToCText xs = String.concatWith " " (List.map intToCText xs)

  (* Extract all minimal-height K-types from a full deformation polynomial. *)
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
        KType.newFromXAndLambdaRhoText (g, #x t, intsToCText (#lambdaRho t))
    in
      List.map mk lows
    end

  (* Lowest K-types of a parameter, computed via `atlas_param_full_deform`. *)
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

  (* Unique lowest K-type of a parameter; raises if not unique. *)
  fun LKT_param (g: AtlasFFI.group, p: AtlasFFI.param) : KType.ktype =
    (case LKTs_param (g, p) of
       [t] => t
     | ts =>
         (List.app KType.free ts;
          raise Fail ("LowestKTypes.LKT_param: no unique lowest K-type; count=" ^ Int.toString (length ts))))

  (* Lowest K-types of a K-type (via its attached parameter). *)
  fun LKTs_ktype (g: AtlasFFI.group, t: KType.ktype) : KType.ktype list =
    let
      val p = KType.parameter t
      val lows = LKTs_param (g, p)
      val () = AtlasFFI.atlas_param_free p
    in
      lows
    end

  (* Unique lowest K-type of a K-type (via its attached parameter). *)
  fun LKT_ktype (g: AtlasFFI.group, t: KType.ktype) : KType.ktype =
    let
      val p = KType.parameter t
      val low = LKT_param (g, p)
      val () = AtlasFFI.atlas_param_free p
    in
      low
    end
end
