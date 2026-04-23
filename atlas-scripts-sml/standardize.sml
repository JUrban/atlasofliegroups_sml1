use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Split.sml";
use "atlas-scripts-sml/ParamPol.sml";
use "atlas-scripts-sml/WeylWord.sml";

(*
  File: atlas-scripts-sml/standardize.sml

  Purpose
  - SML translation of the coherent-continuation helpers in
    `atlas-scripts/standardize.at`.
  - This script is used by several downstream `.at` scripts to “standardize”
    parameters (or parameter polynomials) via coherent continuation and to
    compute a certain KGB subset (`KGP_set`).

  What is implemented
  - `coherent_simple_reflect`:
      A single coherent simple reflection acting on a `ParamPol.t`, using the
      KGB root status of `x(p)` and the Atlas primitives `cross` / `Cayley`.
  - `coherent_act`:
      Iterates `coherent_simple_reflect` over a Weyl word.
  - `coherent_act_K`:
      Coherent action followed by scaling each parameter by 0 (`p*0` in `.at`),
      implemented via `atlas_param_scale(p,0,1)`.
  - `KGP_set`:
      The BFS-based computation of the KGP set for a given starting KGB index.

  What is not implemented yet
  - `restandard` and `finals` from `standardize.at` are not ported here yet;
    they depend on additional weight/RootDatum plumbing and are not currently
    required by the SML verification pipelines.

  Ownership and memory
  - `ParamPol.t` owns its stored `AtlasFFI.param` handles.
  - The functions here return freshly-allocated `ParamPol.t` values; callers
    should free them with `ParamPol.free` when finished.
*)

structure Standardize = struct
  type param = AtlasFFI.param
  type group = AtlasFFI.group
  type coef = Split.t
  type poly = ParamPol.t
  type weyl_word = WeylWord.t

  fun failFFI (where': string) : 'a =
    raise Fail ("Standardize." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun checkNonNeg (where': string, x: int) : int =
    if x < 0 then failFFI where' else x

  fun checkNonNullParam (where': string, p: param) : param =
    if p = Foreign.Memory.null then failFFI where' else p

  fun paramGroup (p: param) : group =
    let
      val g = AtlasFFI.atlas_param_group_handle p
    in
      if g = Foreign.Memory.null then failFFI "paramGroup" else g
    end

  fun kgbStatus (g: group, s: int, x: int) : int =
    checkNonNeg ("kgbStatus", AtlasFFI.atlas_kgb_status (g, s, x))

  fun kgbCross (g: group, s: int, x: int) : int =
    checkNonNeg ("kgbCross", AtlasFFI.atlas_kgb_cross (g, s, x))

  fun kgbCayley (g: group, s: int, x: int) : int =
    checkNonNeg ("kgbCayley", AtlasFFI.atlas_kgb_cayley (g, s, x))

  fun paramCross (p: param, s: int) : param =
    checkNonNullParam ("paramCross", AtlasFFI.atlas_param_cross (p, s))

  fun paramCayley (p: param, s: int) : param =
    checkNonNullParam ("paramCayley", AtlasFFI.atlas_param_cayley (p, s))

  fun paramScale (p: param, num: int, den: int) : param =
    checkNonNullParam ("paramScale", AtlasFFI.atlas_param_scale (p, num, den))

  (*
    coherent_simple_reflect(s,P) : ParamPol.t

    Atlas `.at`:
      coherent_simple_reflect (int s,ParamPol P)
  *)
  fun coherent_simple_reflect (s: int, P: poly) : poly =
    let
      val out = ParamPol.create ()
      fun add (c: coef, p: param) = ParamPol.addTermMove (out, c, p)

      fun oneTerm (c: coef, p: param) : unit =
        let
          val g = paramGroup p
          val x = checkNonNeg ("param_x", AtlasFFI.atlas_param_x p)
          val st = kgbStatus (g, s, x)
        in
          case st of
            0 => add (c, paramCross (p, s)) (* C- *)
          | 1 => add (Split.neg c, ParamPol.cloneParam p) (* ic *)
          | 2 => add (c, paramCross (p, s)) (* real *)
          | 3 => (* nc: Cayley + cross combination *)
              let
                val c1 = paramCayley (p, s)
                val cp = paramCross (p, s)
                val c2 = paramCross (c1, s)
              in
                if AtlasFFI.atlas_param_equal (c1, c2) = 1 then
                  (add (c, c1); add (Split.neg c, cp); AtlasFFI.atlas_param_free c2)
                else
                  (add (c, c1); add (c, c2); add (Split.neg c, cp))
              end
          | 4 => add (c, paramCross (p, s)) (* C+ *)
          | _ => raise Fail ("Standardize.coherent_simple_reflect: illegal KGB status " ^ Int.toString st)
        end
    in
      List.app oneTerm (ParamPol.terms P);
      out
    end

  fun coherent_act (w: weyl_word, P: poly) : poly =
    (case w of
       [] => ParamPol.clone P
     | s0 :: ss =>
         let
           fun loop (cur: poly, rest: int list) : poly =
             (case rest of
                [] => cur
              | s :: rs =>
                  let
                    val next = coherent_simple_reflect (s, cur)
                    val () = ParamPol.free cur
                  in
                    loop (next, rs)
                  end)

           val first = coherent_simple_reflect (s0, P)
         in
           loop (first, ss)
         end)

  (* Consume `P` and return a new polynomial with parameters scaled by `num/den`.
     On return, `P` is empty and owns no params. *)
  fun scale_params_move (P: poly, num: int, den: int) : poly =
    let
      val out = ParamPol.create ()
      fun step (c, p) =
        let
          val p2 = paramScale (p, num, den)
          val () = AtlasFFI.atlas_param_free p
        in
          ParamPol.addTermMove (out, c, p2)
        end
    in
      (List.app step (ParamPol.terms P); P := []; out)
      handle e => (ParamPol.free out; raise e)
    end

  fun coherent_act_K (w: weyl_word, P: poly) : poly =
    let
      val Q = coherent_act (w, P)
    in
      scale_params_move (Q, 0, 1)
    end

  (*
    KGP_set

    Atlas `.at`:
      set KGP_set (KGBElt x) = [KGBElt]: ...

    SML:
      `KGP_set (g,x0)` returns a list of KGB indices in the computed set.
  *)
  fun KGP_set (g: group, x0: int) : int list =
    let
      val r = checkNonNeg ("semisimple_rank", AtlasFFI.atlas_group_semisimple_rank g)
      val n = checkNonNeg ("kgb_size", AtlasFFI.atlas_group_kgb_size g)
      val () = if 0 <= x0 andalso x0 < n then () else raise Fail "Standardize.KGP_set: x0 out of range"

      fun lastComplexDescent (x: int) : int option =
        let
          fun loop s =
            if s < 0 then NONE
            else if kgbStatus (g, s, x) = 0 then SOME s
            else loop (s - 1)
        in
          loop (r - 1)
        end

      fun descendComplex (x: int) : int =
        (case lastComplexDescent x of
           NONE => x
         | SOME s => descendComplex (kgbCross (g, s, x)))

      val xStart = descendComplex x0
      val realGens = List.filter (fn s => kgbStatus (g, s, xStart) = 2) (List.tabulate (r, fn i => i))

      val seen = Array.array (n, false)
      val queue : int list ref = ref [xStart]
      val () = Array.update (seen, xStart, true)

      fun push y =
        if y < 0 orelse y >= n then ()
        else if Array.sub (seen, y) then ()
        else (Array.update (seen, y, true); queue := (!queue) @ [y])

      fun neighbors (s: int, x: int) : int list =
        case kgbStatus (g, s, x) of
          0 => [kgbCross (g, s, x)]
        | 2 => [kgbCayley (g, s, x), kgbCross (g, s, x)]
        | 3 => [kgbCross (g, s, x)]
        | _ => []

      fun loop () =
        (case !queue of
           [] => ()
         | x :: xs =>
             (queue := xs;
              List.app (fn s => List.app push (neighbors (s, x))) realGens;
              loop ()))

      val () = loop ()

      fun collect i acc =
        if i = n then List.rev acc else collect (i + 1) (if Array.sub (seen, i) then i :: acc else acc)
    in
      collect 0 []
    end
end
