use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/weylgroup_at.sml";

(*
  File: atlas-scripts-sml/WeylElt.sml

  Purpose
  - Provide a small SML analogue of the Atlas `.at` type `WeylElt` for a
    `RootDatum`.
  - This is intentionally minimal but sufficient for ports that need:
      - constructing Weyl elements from words in simple generators (`W_elt`)
      - multiplying / conjugating elements
      - computing length and basic Bruhat descents

  Representation
  - `type t` stores:
      - the ambient `RootDatum.t` handle
      - the action matrix on weights in X^* (row-major `IntMatrix.mat`)
      - a (not necessarily reduced) word in simple reflections

  Conventions
  - A word `[s0,s1,...]` represents the product `s0*s1*...` (left-to-right).
  - The action on weight vectors is by left multiplication of the matrix:
      `w * v = matVecMul(w.matrix, v)`.
*)

structure WeylElt = struct
  type rootdatum = RootDatum.t
  type word = int list
  type mat = IntMatrix.mat

  type t = {rd: rootdatum, mat: mat, word: word}

  fun fail where' msg = raise Fail ("WeylElt." ^ where' ^ ": " ^ msg)

  fun root_datum (w: t) : rootdatum = #rd w
  fun matrix (w: t) : mat = #mat w
  fun word (w: t) : word = #word w

  fun eq (a: t, b: t) : bool = #rd a = #rd b andalso #mat a = #mat b

  fun id_W (rd: rootdatum) : t =
    {rd = rd, mat = IntMatrix.identity (RootDatum.rank rd), word = []}

  fun from_word (rd: rootdatum, w: word) : t =
    let
      val n = RootDatum.rank rd
      val id = IntMatrix.identity n
      fun step (s, acc) = IntMatrix.matMul (WeylgroupAT.reflection_matrix_simple (rd, s), acc)
      val m = List.foldl step id w
    in
      {rd = rd, mat = m, word = w}
    end

  fun simple (rd: rootdatum, s: int) : t = from_word (rd, [s])

  fun mul (a: t, b: t) : t =
    if #rd a <> #rd b then
      fail "mul" "root data don't match"
    else
      { rd = #rd a
      , mat = IntMatrix.matMul (#mat a, #mat b)
      , word = #word a @ #word b
      }

  fun inverse (w: t) : t =
    (* Simple reflections are involutions, so inverse is reverse word. *)
    from_word (#rd w, List.rev (#word w))

  fun act (w: t, v: int list) : int list =
    IntMatrix.matVecMul (#mat w, v)

  (* Weyl length: number of positive roots mapped to negative roots. *)
  fun length (w: t) : int =
    let
      val rd = #rd w
      val pos = RootDatum.posRootsCols rd
      fun isPosRoot alpha = WeylgroupAT.is_positive_root (rd, alpha)
      fun step (alpha, acc) =
        let
          val img = act (w, alpha)
        in
          if isPosRoot img then acc else acc + 1
        end
    in
      List.foldl step 0 pos
    end
end

