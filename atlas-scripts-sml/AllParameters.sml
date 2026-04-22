use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/ParamFinals.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";
use "atlas-scripts-sml/Dominant.sml";

(*
  File: atlas-scripts-sml/AllParameters.sml

  Purpose
  - Construct (final) Atlas parameters from “geometric” input data, primarily a
    KGB element `x` and a rational weight `gamma` (often taken from the
    fundamental parallelepiped / FPP routines).

  Background / Atlas correspondence
  - In the Atlas interpreter, scripts frequently build parameters by specifying
    `(x,lambda,nu)` implicitly via relations involving the involution `theta`
    attached to `x`. This module provides the SML-side analogue needed by the
    F4 FPP/unitarity workflow, without depending on any `.at` scripts.

  Conventions and ownership
  - `ratweight` is represented as `{den, nums}` with integers; values are not
    automatically normalized except where documented.
  - Functions returning `AtlasFFI.param list` allocate C++ parameters; callers
    must eventually free them (use `freeAll`).
  - Some helpers (e.g. `addUniqueByEquivalent`) may free parameters to avoid
    duplicates; read the function-level comments.
*)
structure AllParameters = struct
  type ratweight = {den: int, nums: int list}

  (* Parse a whitespace-separated list of integers. *)
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("AllParameters: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  (* Parse a `ratweight` from Atlas text: `den n1 n2 ... nk`. *)
  fun parseRatWeightText s : ratweight =
    (case parseInts s of
       den :: rest => {den = den, nums = rest}
     | _ => raise Fail ("AllParameters: bad ratweight text: " ^ s))

  (* Convert SML `~` negatives to C-style `-` negatives (Atlas C++ parser). *)
  fun intToCText n =
    let
      val s = Int.toString n
    in
      if String.size s > 0 andalso String.sub (s, 0) = #"~" then
        "-" ^ String.extract (s, 1, NONE)
      else
        s
    end

  (* Serialize an integer list in Atlas C++ “int list text” format. *)
  fun intsToCText xs =
    String.concatWith " " (List.map intToCText xs)

  (* Parse `atlas_group_kgb_involution_matrix_text` output.
     Format: `rank` followed by `rank*rank` row-major entries. *)
  fun parseInvolutionMatrixText s : Lattice.mat =
    let
      val ns = parseInts s
    in
      case ns of
        rank :: rest =>
          let
            val need = rank * rank
            val () =
              if length rest <> need then
                raise Fail "AllParameters: parseInvolutionMatrixText: bad size"
              else
                ()
            fun row i =
              List.take (List.drop (rest, i * rank), rank)
          in
            List.tabulate (rank, row)
          end
      | _ => raise Fail "AllParameters: parseInvolutionMatrixText: empty"
    end

  (* Add an integral vector to a `ratweight` with denominator 1. *)
  fun ratvecAddIntVec (u: ratweight, v: int list) : ratweight =
    if #den u <> 1 then
      raise Fail "AllParameters: ratvecAddIntVec: expected denom=1"
    else
      {den = 1, nums = ListPair.mapEq (op +) (#nums u, v)}

  (* Serialize a `ratweight` as `den n1 ... nk` (SML-side, not C++-normalized). *)
  fun ratweightToText ({den, nums}: ratweight) : string =
    Int.toString den ^ " " ^ String.concatWith " " (List.map Int.toString nums)

  (* Add `p` to `acc` unless already equivalent to some element of `acc`.
     Frees `p` on the duplicate path (ownership is consumed either way). *)
  fun addUniqueByEquivalent (p: AtlasFFI.param, acc: AtlasFFI.param list) : AtlasFFI.param list =
    if List.exists (fn q => AtlasFFI.atlas_param_equivalent (p, q) = 1) acc then
      (AtlasFFI.atlas_param_free p; acc)
    else
      p :: acc

  (* Enumerate final parameters for fixed `(g,x,gamma)` without forcing `gamma`
     to be dominant. The algorithm:
       1) read `theta(x)` from Atlas, build `(I+theta)`
       2) solve for integral `lambda` consistent with `gamma` and `rho(g)`
       3) set `nu = (1-theta)*gamma/2`
       4) apply all `theta`-lattice twists (from `LambdaDifferential0`)
       5) normalize and take `finals`, deduplicating by `equivalent`.

     Returns newly allocated `AtlasFFI.param`s; caller must free them. *)
  fun all_parameters_x_gamma_raw (g: AtlasFFI.group, x: int, gamma: ratweight) : AtlasFFI.param list =
    let
      val rank = AtlasFFI.atlas_group_rank g
      val theta =
        parseInvolutionMatrixText (AtlasFFI.atlas_group_kgb_involution_matrix_text (g, x))
      val th1 = Lattice.matAdd (Lattice.identity rank, theta)
      val rho = parseRatWeightText (AtlasFFI.atlas_group_rho_text g)
      val u = Lattice.matVecMulRatvec th1 (Lattice.ratvecSub (gamma, rho))
      val lrOpt = Lattice.vec_solve (th1, u)
    in
      case lrOpt of
        NONE => []
      | SOME lr =>
          let
            val lambda = ratvecAddIntVec (rho, lr)
            val oneMinusTheta =
              ListPair.mapEq
                (fn (ri, ti) => ListPair.mapEq (op -) (ri, ti))
                (Lattice.identity rank, theta)
            val nu = Lattice.ratvecScale (Lattice.matVecMulRatvec oneMinusTheta gamma, 1, 2)

            val twists = LambdaDifferential0.allTheta theta

            fun addTwist (v: int list, acc: AtlasFFI.param list) =
              let
                val lambda2 = ratvecAddIntVec (lambda, v)
                val p0 =
                  AtlasFFI.atlas_param_new_from_lambda_nu_text
                    (g, x, intsToCText (#nums lambda2), #den lambda2, intsToCText (#nums nu), #den nu)
                val () =
                  if p0 = Foreign.Memory.null then
                    raise Fail ("AllParameters: parameter construction failed: " ^ AtlasFFI.atlas_last_error ())
                  else
                    ()
                val p1 = AtlasFFI.atlas_param_normalise p0
                val () = AtlasFFI.atlas_param_free p0
                val () =
                  if p1 = Foreign.Memory.null then
                    raise Fail ("AllParameters: normalise failed: " ^ AtlasFFI.atlas_last_error ())
                  else
                    ()

                val finals = ParamFinals.finals p1
                val () = AtlasFFI.atlas_param_free p1

                fun addFinal ((q, mult), acc2) =
                  if mult = 0 then
                    (AtlasFFI.atlas_param_free q; acc2)
                  else
                    let
                      val q1 = AtlasFFI.atlas_param_normalise q
                      val () = AtlasFFI.atlas_param_free q
                      val () =
                        if q1 = Foreign.Memory.null then
                          raise Fail ("AllParameters: normalise(final) failed: " ^ AtlasFFI.atlas_last_error ())
                        else
                          ()
                    in
                      addUniqueByEquivalent (q1, acc2)
                    end
              in
                List.foldl addFinal acc finals
              end
          in
            List.rev (List.foldl addTwist [] twists)
          end
    end

  (* As `all_parameters_x_gamma_raw`, but first makes `gamma` dominant for `g`. *)
  fun all_parameters_x_gamma_dominant (g: AtlasFFI.group, x: int, gamma: ratweight) : AtlasFFI.param list =
    let
      val gammaDomText = Dominant.makeDominantText g (ratweightToText gamma)
      val gammaDom = parseRatWeightText gammaDomText
    in
      all_parameters_x_gamma_raw (g, x, gammaDom)
    end

  (* Default choice: do not force dominance. *)
  val all_parameters_x_gamma = all_parameters_x_gamma_raw

  (* Enumerate parameters across all KGB elements, deduplicating by equivalence.
     This can allocate many parameters; callers should free the result. *)
  fun all_parameters_gamma_raw (g: AtlasFFI.group, gamma: ratweight) : AtlasFFI.param list =
    let
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
      fun loop (x, acc) =
        List.foldl addUniqueByEquivalent acc (all_parameters_x_gamma_raw (g, x, gamma))
    in
      List.rev (List.foldl loop [] (List.tabulate (kgbSize, fn i => i)))
    end

  (* Default choice: do not force dominance. *)
  val all_parameters_gamma = all_parameters_gamma_raw

  (* As `all_parameters_gamma_raw`, but first makes `gamma` dominant for `g`. *)
  fun all_parameters_gamma_dominant (g: AtlasFFI.group, gamma: ratweight) : AtlasFFI.param list =
    let
      val gammaDomText = Dominant.makeDominantText g (ratweightToText gamma)
      val gammaDom = parseRatWeightText gammaDomText
    in
      all_parameters_gamma_raw (g, gammaDom)
    end

  (* Free a list of `AtlasFFI.param` handles. *)
  fun freeAll (ps: AtlasFFI.param list) : unit =
    List.app AtlasFFI.atlas_param_free ps
end
