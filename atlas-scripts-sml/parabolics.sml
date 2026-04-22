use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Partial SML analogue of `atlas-scripts/parabolics.at`.
   This starts with the KGB-graph primitives needed for induction-related scripts. *)
structure Parabolics = struct
  type group = AtlasFFI.group
  type kgbelt = int
  type parabolic = int list * kgbelt (* (S, x) *)

  fun ascents (g: group) (S: int list, x: kgbelt) : kgbelt list =
    let
      fun one s =
        case AtlasFFI.atlas_kgb_status (g, s, x) of
          3 => [AtlasFFI.atlas_kgb_cayley (g, s, x)] (* nc *)
        | 4 => [AtlasFFI.atlas_kgb_cross (g, s, x)] (* C+ *)
        | _ => [] (* descents / not used *)
    in
      List.concat (List.map one S)
    end

  fun down_neighbors (g: group) (S: int list, x: kgbelt) : kgbelt list =
    let
      fun one s =
        case AtlasFFI.atlas_kgb_status (g, s, x) of
          0 => [AtlasFFI.atlas_kgb_cross (g, s, x)] (* C- *)
        | 2 =>
            let
              val y = AtlasFFI.atlas_kgb_cayley (g, s, x)
            in
              [y, AtlasFFI.atlas_kgb_cross (g, s, y)]
            end
        | _ => []
    in
      List.concat (List.map one S)
    end

  fun is_maximal_for (g: group) (S: int list, x: kgbelt) : bool =
    null (ascents g (S, x))

  fun maximal (g: group) (S: int list, x: kgbelt) : kgbelt =
    let
      fun loop x =
        case ascents g (S, x) of
          [] => x
        | y :: _ => loop y
    in
      loop x
    end

  fun x_min (g: group) (S: int list, x: kgbelt) : kgbelt =
    let
      fun loop x =
        case down_neighbors g (S, x) of
          [] => x
        | y :: _ => loop y
    in
      loop x
    end

  fun representative (g: group) (P: parabolic) : kgbelt =
    maximal g P

  fun is_closed (g: group) (P as (S, x): parabolic) : bool =
    AtlasFFI.atlas_kgb_length (g, x_min g (S, x)) = 0
end

