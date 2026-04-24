use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/ParamPol.sml";

(*
  File: atlas-scripts-sml/std_decs.sml

  Purpose
  - SML analogue of `atlas-scripts/std_decs.at`.
  - In the Atlas interpreter, `std_decs.at` is a convenience “declarations”
    file that assigns types to a collection of common global variable names
    used in interactive sessions and ad-hoc scratch scripts.
  - Standard ML does not have the same global-typing mechanism; this port
    therefore provides:
      - type aliases matching the common Atlas script conventions; and
      - optional mutable bindings (`... option ref`) for the same names, so
        translated scripts and interactive Poly/ML sessions can reuse the
        familiar identifiers without committing to a particular initial value.

  Notes
  - These bindings are *not* used by the main ported libraries; they exist only
    as a lightweight compatibility layer for translation work and interactive
    experimentation.
*)

structure StdDecs = struct
  type vec = int list
  type mat = IntMatrix.mat
  type ratvec = Lattice.ratvec

  type rootdatum = RootDatum.t
  type group = AtlasFFI.group
  type param = AtlasFFI.param
  type ktype = AtlasFFI.ktype
  type ktypepol = AtlasFFI.ktypepol

  type weyl_word = WeylWord.t

  (* The bindings below mirror `std_decs.at` variable names. *)
  val v: vec option ref = ref NONE
  val M: mat option ref = ref NONE
  val A: mat option ref = ref NONE
  val B: mat option ref = ref NONE

  val roots: mat option ref = ref NONE
  val coroots: mat option ref = ref NONE
  val rd: rootdatum option ref = ref NONE

  val alpha: vec option ref = ref NONE
  val beta: vec option ref = ref NONE
  val alpha_v: vec option ref = ref NONE
  val beta_v: vec option ref = ref NONE
  val delta: mat option ref = ref NONE
  val xi: mat option ref = ref NONE

  (* InnerClass has no direct SML handle yet; store as a user-facing tag. *)
  val ic: string option ref = ref NONE

  val g: ratvec option ref = ref NONE
  val G: group option ref = ref NONE

  val gamma: ratvec option ref = ref NONE
  val lambda: ratvec option ref = ref NONE
  val mu: vec option ref = ref NONE
  val nu: ratvec option ref = ref NONE

  (* KGB elements are represented as integer indices in the SML port. *)
  val x: int option ref = ref NONE
  val y: int option ref = ref NONE

  val rho: ratvec option ref = ref NONE
  val rho_check: ratvec option ref = ref NONE
  val theta: mat option ref = ref NONE

  val p: param option ref = ref NONE
  val q: param option ref = ref NONE
  val Kt: ktype option ref = ref NONE
  val KP: ktypepol option ref = ref NONE

  val block: param list ref = ref []
  val P: ParamPol.t = ParamPol.create ()

  val w: weyl_word ref = ref []
end
