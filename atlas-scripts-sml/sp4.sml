use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/groups.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/ParamBlocks.sml";
use "atlas-scripts-sml/Rat.sml";

(*
  File: atlas-scripts-sml/sp4.sml

  Purpose
  - Standard ML translation of `atlas-scripts/sp4.at`.
  - Provides a convenient “Sp(4,R) playground”:
      - an explicit C2 root datum in the “natural coordinates” used by the
        original script,
      - the split real form `Sp(4,R)` as an Atlas group handle,
      - the full list of KGB indices,
      - and several helper constructors for commonly used parameter families
        attached to specific Cartan classes/KGB indices (ds0/ds1/hds/ads/c1/c2a/c2b/c3).

  Notes / translation choices
  - The `.at` script builds `sp4R` via `root_datum` + `inner_class` +
    `quasisplit_form`. In the SML port we construct the same real form using
    the existing group constructor `Groups.Sp_R(4)` (type C2, quasisplit).
  - The `.at` `param(x,lambda_rho,gamma)` primitive uses an integral
    `lambda_rho` and a rational “infinitesimal character” vector. The Poly/ML
    shim constructor `atlas_param_new_from_lambda_rho_gamma_text` is a direct
    wrapper for the Atlas C++ `Rep_context::sr_gamma` constructor, matching the
    `.at` semantics.

  Ownership
  - `sp4R` and `sp4` are allocated handles held globally in this structure (as
    in the `.at` script). They are not automatically freed.
  - All parameter constructors return owned `AtlasFFI.param` handles; callers
    must free them with `AtlasFFI.atlas_param_free`.
*)

structure Sp4 = struct
  type group = AtlasFFI.group
  type rootdatum = RootDatum.t
  type param = AtlasFFI.param
  type rat = Rat.rat
  type ratvec = Lattice.ratvec

  (* This defines Sp(4,R) in natural coordinates. Diagram is type C2. *)
  val simple_roots_cols : int list list = [[1, ~1], [0, 2]]
  val simple_coroots_cols : int list list = [[1, ~1], [0, 1]]

  (* Convert a list of column vectors into a row-major matrix. *)
  fun matFromCols cols : int list list =
    (case cols of
       [] => []
     | c0 :: _ =>
         let
           val nRows = length c0
           fun row i = List.map (fn c => List.nth (c, i)) cols
         in
           List.tabulate (nRows, row)
         end)

  val simple_roots = matFromCols simple_roots_cols
  val simple_coroots = matFromCols simple_coroots_cols

  val sp4 : rootdatum = RootDatum.newFromSimpleMats (simple_roots, simple_coroots, false)

  (* Split real form Sp(4,R) (type C2). *)
  val sp4R : group = Groups.Sp_R 4

  val kgbSize : int = AtlasFFI.atlas_group_kgb_size sp4R
  val x : int list = List.tabulate (kgbSize, fn i => i)

  fun fail where' msg = raise Fail ("Sp4." ^ where' ^ ": " ^ msg)

  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  fun intsToCText xs = String.concatWith " " (List.map intToCText xs)

  fun ratAbs (r: rat) : rat =
    let
      val r = Rat.normalize r
    in
      if #num r < 0 then Rat.make (~ (#num r), #den r) else r
    end

  fun ratGeq (a: rat, b: rat) : bool =
    let
      val a = Rat.normalize a
      val b = Rat.normalize b
      val lhs = IntInf.fromInt (#num a) * IntInf.fromInt (#den b)
      val rhs = IntInf.fromInt (#num b) * IntInf.fromInt (#den a)
    in
      lhs >= rhs
    end

  fun ratDivInt (a: rat, k: int) : rat =
    if k = 0 then fail "ratDivInt" "divide by zero" else Rat.make (#num (Rat.normalize a), #den (Rat.normalize a) * k)

  fun ratNeg (a: rat) : rat = Rat.make (~ (#num (Rat.normalize a)), #den (Rat.normalize a))

  (* Convert a list of scalar rationals to a single `ratvec` with common denominator. *)
  fun ratvecOfRats (rs: rat list) : ratvec =
    let
      val rs = List.map Rat.normalize rs
      fun gcdInf (u: IntInf.int, v: IntInf.int) : IntInf.int =
        let
          val u = IntInf.abs u
          val v = IntInf.abs v
          fun loop (x, 0) = x
            | loop (x, y) = loop (y, IntInf.mod (x, y))
        in
          if u = 0 then v else loop (u, v)
        end
      fun lcmInf (u: IntInf.int, v: IntInf.int) : IntInf.int =
        if u = 0 orelse v = 0 then 0 else IntInf.div (IntInf.abs (u * v), gcdInf (u, v))
      val denL = List.foldl (fn (r, acc) => lcmInf (acc, IntInf.fromInt (#den r))) 1 rs
      fun scaledNum r =
        let
          val mul = IntInf.div (denL, IntInf.fromInt (#den r))
          val n = IntInf.fromInt (#num r) * mul
        in
          IntInf.toInt n handle _ => fail "ratvecOfRats" "numerator overflow"
        end
      val nums = List.map scaledNum rs
      val den = IntInf.toInt denL handle _ => fail "ratvecOfRats" "denominator overflow"
    in
      Lattice.ratvecNormalize {den = den, nums = nums}
    end

  (* Build `param(x,lambda_rho,gamma)` (script semantics). *)
  fun param (xIndex: int, lambdaRho: int list, gamma: ratvec) : param =
    let
      val gamma = Lattice.ratvecNormalize gamma
      val p =
        AtlasFFI.atlas_param_new_from_lambda_rho_gamma_text
          ( sp4R
          , xIndex
          , intsToCText lambdaRho
          , 1
          , intsToCText (#nums gamma)
          , #den gamma
          )
    in
      if p = Foreign.Memory.null then
        fail "param" ("param construction failed: " ^ AtlasFFI.atlas_last_error ())
      else
        p
    end

  (* `do_block(f)(p)` = apply `f` to each survivor in `block(p)`. *)
  fun do_block (f: param -> unit) (p: param) : unit =
    let
      val (terms, _) = ParamBlocks.block_survivors p
      fun step (q, _) = (f q; AtlasFFI.atlas_param_free q)
    in
      List.app step terms
    end

  val show_block : param -> unit =
    do_block (fn q => TextIO.print (AtlasFFI.atlas_param_gamma_text q ^ "\n"))

  (* Cartan #0 discrete-series parameters; assume a,b >= 0. *)
  fun ds0 (a: int, b: int) : param = param (List.nth (x, 0), [a - 2, b - 1], ratvecOfRats [Rat.make (0, 1), Rat.make (0, 1)])
  fun ds1 (a: int, b: int) : param = param (List.nth (x, 1), [a - 2, b - 1], ratvecOfRats [Rat.make (0, 1), Rat.make (0, 1)])
  fun hds (a: int, b: int) : param = param (List.nth (x, 2), [a - 2, b - 1], ratvecOfRats [Rat.make (0, 1), Rat.make (0, 1)])
  fun ads (a: int, b: int) : param = param (List.nth (x, 3), [a - 2, b - 1], ratvecOfRats [Rat.make (0, 1), Rat.make (0, 1)])

  (* Cartan #1: short real roots ±(1,1); assume c >= 0. *)
  fun c1 (c: int, yIn: rat) : param =
    let
      val y = ratAbs yIn
      val halfC = Rat.make (c, 2)
      val y2 = ratDivInt (y, 2)
    in
      if ratGeq (y, halfC) then
        param (List.nth (x, 9), [c - 1, 0], ratvecOfRats [y2, y2])
      else
        param (List.nth (x, 4), [c - 3, 0], ratvecOfRats [y2, ratNeg y2])
    end

  fun modPos (n: int, m: int) : int =
    let
      val r = n mod m
    in
      if r < 0 then r + m else r
    end

  (* Cartan #2: long real roots ±(2,0); assume b >= 0. *)
  fun c2a (a: int, b: int, yIn: rat) : param =
    let
      val y = ratAbs yIn
    in
      if ratGeq (y, Rat.make (b, 1)) then
        param (List.nth (x, 7), [modPos (a, 2), b - 1], ratvecOfRats [y, Rat.make (0, 1)])
      else
        param (List.nth (x, 5), [b - 2, modPos (a - 1, 2)], ratvecOfRats [Rat.make (0, 1), y])
    end

  fun c2b (a: int, b: int, yIn: rat) : param =
    let
      val y = ratAbs yIn
    in
      if ratGeq (y, Rat.make (b, 1)) then
        param (List.nth (x, 8), [modPos (a, 2), b - 1], ratvecOfRats [y, Rat.make (0, 1)])
      else
        param (List.nth (x, 6), [b - 2, modPos (a - 1, 2)], ratvecOfRats [Rat.make (0, 1), y])
    end

  (* Cartan #3: split Cartan. *)
  fun c3 (a: int, b: int, nu0In: rat, nu1In: rat) : param =
    let
      val nu0 = ratAbs nu0In
      val nu1 = ratAbs nu1In
    in
      if ratGeq (nu0, nu1) then
        param (List.nth (x, 10), [modPos (a, 2), modPos (b - 1, 2)], ratvecOfRats [nu0, nu1])
      else
        param (List.nth (x, 10), [modPos (b - 1, 2), modPos (a, 2)], ratvecOfRats [nu1, nu0])
    end

  (* Example: trivial representation. *)
  fun triv () : param =
    param (List.nth (x, 10), [0, 0], ratvecOfRats [Rat.make (2, 1), Rat.make (1, 1)])
end
