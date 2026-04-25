use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/lattice.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/twist.sml

  Purpose
  - Partial Standard ML translation of `atlas-scripts/twist.at`.
  - The original `.at` script provides utilities for “twisting”:
      - an external involution acting on a KGB set,
      - the corresponding action on blocks,
      - and a derived action on parameters.

  What is implemented in this SML port
  - Pure matrix/list utilities that do not require new Atlas bindings:
      - `perm_mat`  : permutation matrix from a permutation vector
      - `invol_mat` : permutation matrix from a list of transpositions
      - `show_pairs`: pretty-print an involution given by `int -> int`
      - `common_denominator`: lcm of denominators of a list of `ratvec`
  - Parameter twist by an integer matrix:
      - `twist_param (p, delta)` is implemented via the C++ shim primitive
        `atlas_param_twist_by_delta_text`.

  What is NOT implemented yet
  - The substantial parts of `twist.at` that manipulate `InnerClass`, `RootDatum`,
    `KGBElt`, and `Block` values (e.g. `KGB_twist`, `block_twist`, `quotient`,
    `kernel_sublattice`, and all the verification helpers).
  - Those require additional SML-visible bindings for inner classes, real forms
    (beyond the current `AtlasFFI.group` handle usage), and block APIs.

  Notes
  - This file is still useful because the small matrix helpers are used as
    building blocks in other ports, and because parameter twisting is a common
    operation.
*)

structure Twist = struct
  type mat = MatrixAT.mat
  type ratvec = Lattice.ratvec
  type param = AtlasFFI.param

  (* Placeholder types for the unported portions of `twist.at`. *)
  type inner_class = unit
  type real_form = unit
  type block = unit
  type kgb_elt = int

  fun failUnported (name: string) : 'a =
    raise Fail ("Twist." ^ name ^ ": not yet ported (needs InnerClass/KGB/Block bindings)")

  fun failFFI (where': string) : 'a =
    raise Fail ("Twist." ^ where' ^ ": " ^ AtlasFFI.atlas_last_error ())

  fun checkNonNullParam (where': string, p: param) : param =
    if p = Foreign.Memory.null then failFFI where' else p

  (*
    perm_mat(pi) : mat

    Atlas `.at`:
      set perm_mat ([int] pi) = mat:
        let n=#pi then id = id_mat(n) in n # for c in pi do id[c] od
  *)
  fun perm_mat (pi: int list) : mat = MatrixAT.permutation_matrix pi

  (*
    invol_mat(transp,n) : mat

    Atlas `.at`:
      set invol_mat ([(int,int)] transp, int n) = mat:
        let pi=#n
        in begin for (i,j) in transp do let t=pi[i] in pi[i]:=pi[j]; pi[j]:=t od
        ; perm_mat(pi) end
  *)
  fun invol_mat (transp: (int * int) list, n: int) : mat =
    if n < 0 then
      raise Fail "Twist.invol_mat: negative n"
    else
      let
        val pi = Array.tabulate (n, fn i => i)
        fun swap (i, j) =
          if i < 0 orelse j < 0 orelse i >= n orelse j >= n then
            raise Fail "Twist.invol_mat: transposition index out of range"
          else
            let
              val t = Array.sub (pi, i)
              val () = Array.update (pi, i, Array.sub (pi, j))
              val () = Array.update (pi, j, t)
            in
              ()
            end
        val () = List.app swap transp
      in
        perm_mat (List.tabulate (n, fn i => Array.sub (pi, i)))
      end

  (*
    show_pairs(f,n) : string

    Atlas `.at` prints a compact representation of fixed points and 2-cycles:
      - fixed points: " i"
      - 2-cycles:     " (i,j)" for i<j
  *)
  fun show_pairs (f: int -> int, n: int) : string =
    if n < 0 then
      raise Fail "Twist.show_pairs: negative n"
    else
      let
        fun piece i =
          let
            val j = f i
          in
            if j = i then
              " " ^ Int.toString i
            else if i < j then
              " (" ^ Int.toString i ^ "," ^ Int.toString j ^ ")"
            else
              ""
          end
      in
        String.concat (List.tabulate (n, piece))
      end

  (*
    common_denominator(list) : int

    Atlas `.at`:
      denom(ratvec: for v in list do /denom(v) od)
    which is the lcm of the denominators of the input rational vectors.
  *)
  fun common_denominator (vs: ratvec list) : int =
    (case vs of
       [] => 1
     | _ =>
         let
           val ds = List.map (fn v => Int.abs (#den (Lattice.ratvecNormalize v))) vs
         in
           MatrixAT.lcmList ds
         end)

  (*
    twist_param(p,delta) : param

    SML replacement for the `.at` `twist(Param,mat)` operation, implemented
    using the C++ shim primitive `atlas_param_twist_by_delta_text`.

    Ownership
    - Returns a fresh `param` handle; caller must free it with
      `AtlasFFI.atlas_param_free`.
  *)
  fun twist_param (p: param, delta: mat) : param =
    let
      val deltaText = Lattice.matToText delta
      val q = AtlasFFI.atlas_param_twist_by_delta_text (p, deltaText)
    in
      checkNonNullParam ("twist_param", q)
    end

  (* --- Unported portions of `twist.at` (stubs) --- *)

  fun check (_: inner_class, _: mat) : bool = failUnported "check"
  fun KGB_twist (_: inner_class, _: mat) : kgb_elt -> kgb_elt = fn _ => failUnported "KGB_twist"
  fun is_twist_fixed (_: inner_class * mat) : real_form -> bool = fn _ => failUnported "is_twist_fixed"
  fun test (_: real_form) : bool = failUnported "test"
  fun block_is_twist_fixed (_: inner_class * mat) : int * int -> bool = fn _ => failUnported "block_is_twist_fixed"
  fun block_twist (_: inner_class, _: mat) : block -> int -> int = fn _ => failUnported "block_twist"
  fun show_block_twists (_: inner_class * mat) : unit = failUnported "show_block_twists"
  fun quotient (_: inner_class, _: mat, _: mat) : inner_class * mat = failUnported "quotient"
  fun show_kernel (_: unit) : unit = failUnported "show_kernel"
  fun kernel_sublattice (_: unit, _: ratvec list) : mat = failUnported "kernel_sublattice"
end

