use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";

(*
  File: atlas-scripts-sml/Weylgroup.sml

  Purpose
  - Partial SML translation of selected utilities from `atlas-scripts/Weylgroup.at`.
  - This module focuses on “transport” routines used throughout the Atlas script
    ecosystem for simplifying KGB elements and parameters by eliminating complex
    descents/ascents.

  Implemented ports
  - `from_no_Cminus` (KGBElt): descend through complex descents until none remain.
    Returns `(w, x0)` such that the original `x` satisfies:
      `x = cross(w, x0)`
    with `cross(WeylElt w, KGBElt x0)` in the `.at` sense (left cross action).
  - `from_no_Cplus` (KGBElt): similarly eliminate complex ascents.
  - `to_no_Cminus` / `to_no_Cplus`: return just the simplified KGB element.
  - Parameter variants mirroring `.at`:
      `from_no_Cminus (Param p)` and `from_no_Cplus (Param p)`
    which transport `(x,lambda,nu)` by the inverse witness word to the new KGB
    element.

  Atlas correspondence and conventions
  - The C++ shim function `atlas_kgb_status(g,s,x)` matches the `.at` KGB status
    codes:
      0 = complex descent (C-)
      4 = complex ascent  (C+)
    We use these integers directly.
  - The left cross action `cross(WeylElt w, KGBElt x)` iterates over `w.word ~`
    (reverse order), which matches `WeylWord.kgbCrossLeft`.

  Ownership
  - Parameter-returning functions allocate new `AtlasFFI.param` handles; callers
    must free them with `AtlasFFI.atlas_param_free`.
*)

structure Weylgroup = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec
  type weyl_word = WeylWord.t

  (* Extract `(lambda,nu)` from a parameter as normalized rational vectors. *)
  fun lambda_nu (p: param) : ratvec * ratvec =
    let
      val lam = Lattice.ratvecNormalize (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_lambda_text p))
      val nu = Lattice.ratvecNormalize (AllParameters.parseRatWeightText (AtlasFFI.atlas_param_nu_text p))
    in
      (lam, nu)
    end

  (* KGB: eliminate complex descents (C-) by applying cross(s, x) repeatedly.

     Returns `(w, x0)` where `w` is recorded as the list of generators applied
     during the descent, in the order they were applied.

     This matches the `.at` convention that `x = cross(w, x0)` (left cross). *)
  fun from_no_Cminus_kgb (g: group, x: int) : weyl_word * int =
    let
      val r = AtlasFFI.atlas_group_semisimple_rank g
      fun findLastCminus (x: int) : int option =
        let
          fun loop i best =
            if i = r then best
            else
              let
                val st = AtlasFFI.atlas_kgb_status (g, i, x)
                val () =
                  if st < 0 then
                    raise Fail ("Weylgroup.from_no_Cminus_kgb: kgb_status failed: " ^ AtlasFFI.atlas_last_error ())
                  else
                    ()
              in
                if st = 0 then loop (i + 1) (SOME i) else loop (i + 1) best
              end
        in
          loop 0 NONE
        end

      fun loop (x, wordRev) =
        (case findLastCminus x of
           NONE => (List.rev wordRev, x)
         | SOME s =>
             let
               val x2 = AtlasFFI.atlas_kgb_cross (g, s, x)
               val () =
                 if x2 < 0 then
                   raise Fail ("Weylgroup.from_no_Cminus_kgb: kgb_cross failed: " ^ AtlasFFI.atlas_last_error ())
                 else
                   ()
             in
               loop (x2, s :: wordRev)
             end)
    in
      loop (x, [])
    end

  (* KGB: eliminate complex ascents (C+). *)
  fun from_no_Cplus_kgb (g: group, x: int) : weyl_word * int =
    let
      val r = AtlasFFI.atlas_group_semisimple_rank g
      fun findLastCplus (x: int) : int option =
        let
          fun loop i best =
            if i = r then best
            else
              let
                val st = AtlasFFI.atlas_kgb_status (g, i, x)
                val () =
                  if st < 0 then
                    raise Fail ("Weylgroup.from_no_Cplus_kgb: kgb_status failed: " ^ AtlasFFI.atlas_last_error ())
                  else
                    ()
              in
                if st = 4 then loop (i + 1) (SOME i) else loop (i + 1) best
              end
        in
          loop 0 NONE
        end

      fun loop (x, wordRev) =
        (case findLastCplus x of
           NONE => (List.rev wordRev, x)
         | SOME s =>
             let
               val x2 = AtlasFFI.atlas_kgb_cross (g, s, x)
               val () =
                 if x2 < 0 then
                   raise Fail ("Weylgroup.from_no_Cplus_kgb: kgb_cross failed: " ^ AtlasFFI.atlas_last_error ())
                 else
                   ()
             in
               loop (x2, s :: wordRev)
             end)
    in
      loop (x, [])
    end

  fun to_no_Cminus_kgb (g: group, x: int) : int =
    let
      val (_, x0) = from_no_Cminus_kgb (g, x)
    in
      x0
    end

  fun to_no_Cplus_kgb (g: group, x: int) : int =
    let
      val (_, x0) = from_no_Cplus_kgb (g, x)
    in
      x0
    end

  (* Parameter transport (mirrors `.at`):
       let (w,x0)=from_no_Cminus(p.x) then w1=w.inverse
       in (w, parameter(x0, w1*p.lambda, w1*p.nu))

     Here `w` is a witness for `p.x = cross(w,x0)`; we transport weights by
     applying `w^{-1}` to keep the parameter “the same” while changing KGB. *)
  fun from_no_Cminus_param (g: group, p: param) : weyl_word * param =
    let
      val x = AtlasFFI.atlas_param_x p
      val (w, x0) = from_no_Cminus_kgb (g, x)
      val winv = WeylWord.inverse w
      val (lam, nu) = lambda_nu p
      val lam0 = WeylWord.actRatvec (g, winv, lam)
      val nu0 = WeylWord.actRatvec (g, winv, nu)
      val q = Representations.parameter (g, x0, lam0, nu0)
    in
      (w, q)
    end

  fun from_no_Cplus_param (g: group, p: param) : weyl_word * param =
    let
      val x = AtlasFFI.atlas_param_x p
      val (w, x0) = from_no_Cplus_kgb (g, x)
      val winv = WeylWord.inverse w
      val (lam, nu) = lambda_nu p
      val lam0 = WeylWord.actRatvec (g, winv, lam)
      val nu0 = WeylWord.actRatvec (g, winv, nu)
      val q = Representations.parameter (g, x0, lam0, nu0)
    in
      (w, q)
    end

  fun to_no_Cminus_param (g: group, p: param) : param =
    let
      val (_, q) = from_no_Cminus_param (g, p)
    in
      q
    end

  fun to_no_Cplus_param (g: group, p: param) : param =
    let
      val (_, q) = from_no_Cplus_param (g, p)
    in
      q
    end
end

