use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/parameters.sml";

(*
  File: atlas-scripts-sml/is_normal.sml

  Purpose
  - Partial Standard ML translation of `atlas-scripts/is_normal.at`.
  - The `.at` file defines (at least) two notions of “normal”, one for
    parameters and one for K-types; this SML port currently focuses on the
    parameter-side predicate used in later scripts:
      `is_normal(Param p) : (bool, int)`

  Implemented
  - `is_normal_param(p)`:
      - returns `(true, ~1)` if `p` is dominant and has no singular complex
        descents, and `(false, witness)` otherwise.
      - dominance is tested by `⟨alpha_i^∨, gamma(p)⟩ >= 0` for all simple
        coroots (using `atlas_param_gamma_text`).
      - singular complex descents are detected using the Atlas KGB status
        codes via `atlas_kgb_status(g, i, x)`:
          - complex descent: status `0`
          - complex ascent:  status `4`
        so `is_complex(i,x)` is `status in {0,4}`, and the descent test used
        here is `status = 0` (matching the C++ shim’s encoding).

  Not implemented (yet)
  - K-type normality (`is_normal_K`) and the overloads for KTypePol-like term
    lists from the `.at` file. Those require additional K-type infrastructure.

  Ownership
  - This module does not allocate new Atlas handles; it only queries existing
    `param` / `group` / `rootdatum` data through FFI calls.
*)

structure IsNormal = struct
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ratvec = Lattice.ratvec

  fun fail where' msg = raise Fail ("IsNormal." ^ where' ^ ": " ^ msg)

  fun parseRatvecText (s: string) : ratvec = Parameters.parseRatvecText s

  fun dotRatNumer (a: int list, u: ratvec) : IntInf.int =
    let
      val u = Lattice.ratvecNormalize u
      val nums = #nums u
      val () =
        if length a = length nums then ()
        else fail "dotRatNumer" "length mismatch"
    in
      List.foldl (op +) 0 (ListPair.mapEq (fn (x, y) => IntInf.fromInt x * IntInf.fromInt y) (a, nums))
    end

  fun isDominantGamma (simpleCoroots: int list list, gamma: ratvec) : bool =
    let
      val gamma = Lattice.ratvecNormalize gamma
      fun ok alphav = dotRatNumer (alphav, gamma) >= 0
    in
      List.all ok simpleCoroots
    end

  fun firstNegativeDominanceWitness (simpleCoroots: int list list, gamma: ratvec) : int option =
    let
      val gamma = Lattice.ratvecNormalize gamma
      fun loop ([], _) = NONE
        | loop (alphav :: rest, i) =
            if dotRatNumer (alphav, gamma) < 0 then SOME i else loop (rest, i + 1)
    in
      loop (simpleCoroots, 0)
    end

  (* Atlas KGB status encoding used by the C++ shim.
     - `atlas_kgb_status(g,i,x) = 0`  => complex descent
     - `atlas_kgb_status(g,i,x) = 4`  => complex ascent
     Other values correspond to non-complex root types. *)
  fun kgbStatus (g: group, i: int, x: int) : int =
    let
      val s = AtlasFFI.atlas_kgb_status (g, i, x)
    in
      if s < 0 then fail "kgbStatus" (AtlasFFI.atlas_last_error ()) else s
    end

  fun is_complex (g: group, i: int, x: int) : bool =
    let
      val s = kgbStatus (g, i, x)
    in
      s = 0 orelse s = 4
    end

  fun is_descent (g: group, i: int, x: int) : bool =
    kgbStatus (g, i, x) = 0

  (* Primary port: `is_normal(Param p)` from `is_normal.at`. *)
  fun is_normal_param (p: param) : bool * int =
    let
      val g = AtlasFFI.atlas_param_group_handle p
      val x = AtlasFFI.atlas_param_x p
      val gamma = parseRatvecText (AtlasFFI.atlas_param_gamma_text p)

      val rdH = AtlasFFI.atlas_group_rootdatum_new g
      val () =
        if rdH = Foreign.Memory.null then
          fail "is_normal_param" ("rootdatum_new failed: " ^ AtlasFFI.atlas_last_error ())
        else
          ()
      val simpleCoroots = RootDatum.simpleCorootsCols rdH
      val ssr = length simpleCoroots

      fun cleanup () = AtlasFFI.atlas_rootdatum_free rdH
    in
      (case firstNegativeDominanceWitness (simpleCoroots, gamma) of
         SOME i => (cleanup (); (false, i))
       | NONE =>
           let
             val gamma = Lattice.ratvecNormalize gamma
             fun singularComplexDescent i =
               let
                 val alphav = List.nth (simpleCoroots, i)
               in
                 dotRatNumer (alphav, gamma) = 0 andalso is_complex (g, i, x) andalso is_descent (g, i, x)
               end
             fun firstIndex pred =
               let
                 fun loop j = if j >= ssr then NONE else if pred j then SOME j else loop (j + 1)
               in
                 loop 0
               end
           in
             case firstIndex singularComplexDescent of
               NONE => (cleanup (); (true, ~1))
             | SOME i => (cleanup (); (false, i))
           end)
    end

  (* Placeholders for the K-type normality functions from `is_normal.at`. *)
  fun is_normal_K (_: unit) : unit = fail "is_normal_K" "not implemented"
end

