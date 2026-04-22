use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/LatticeAT.sml";
use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/sort.sml";
use "atlas-scripts-sml/twisted_root_datum.sml";

(*
  File: atlas-scripts-sml/cofolded.sml

  Purpose
  - Port of the `cofoldedBOTH` / `cofolded(InnerClass)` machinery from
    `atlas-scripts/FPP_faces_geom.at`.
  - Constructs the “cofolded” root datum (types B/C) and the folding matrix used
    in the folded-FPP computations for F4.

  Inputs/outputs
  - Given a root datum `rd` and a distinguished involution `delta`, `cofoldedBOTH`
    returns `(rdB, rdC, tStar)` where `tStar` is a basis for the delta-fixed lattice.
  - Given an Atlas group handle `g`, `cofolded` reads `rd` and the distinguished
    involution from the library and returns `(rdB, m, j0)` as in the `.at` code.

  Ownership
  - `cofoldedBOTH` returns new rootdatum handles; caller owns them.
  - `cofolded` returns a new rootdatum handle `rdB`; caller must free it.
*)
structure Cofolded = struct
  type rootdatum = RootDatum.t
  type mat = IntMatrix.mat
  type vec = int list
  type ratvec = Lattice.ratvec

  (* Add integer vectors componentwise. *)
  fun vecAdd (a: vec, b: vec) : vec = ListPair.mapEq (op +) (a, b)

  (* Scale an integer vector. *)
  fun vecScale (v: vec, k: int) : vec = List.map (fn x => x * k) v

  (* Zero vector. *)
  fun vecZero n : vec = List.tabulate (n, fn _ => 0)

  (* Multiply a row vector by a matrix (implemented via transpose). *)
  fun vecMatMul (v: vec, m: mat) : vec =
    IntMatrix.matVecMul (IntMatrix.transpose m, v)

  (* Half a vector if all entries are even. *)
  fun halfIfEven (v: vec) : vec option =
    if List.all (fn x => x mod 2 = 0) v then SOME (List.map (fn x => x div 2) v) else NONE

  (* First index of an element satisfying predicate `p`. *)
  fun firstIndex (p: 'a -> bool, xs: 'a list) : int option =
    let
      fun loop (_, []) = NONE
        | loop (i, x :: rest) = if p x then SOME i else loop (i + 1, rest)
    in
      loop (0, xs)
    end

  (* Sum a list of vectors in dimension `n`. *)
  fun sumVecs (n: int, vs: vec list) : vec =
    List.foldl (fn (v, acc) => vecAdd (v, acc)) (vecZero n) vs

  (* Core cofolding routine (see `FPP_faces_geom.at`):
     builds both B- and C-type cofolded data and the fixed-lattice basis `tStar`. *)
  fun cofoldedBOTH (rd: rootdatum, delta: mat) : rootdatum * rootdatum * mat =
    let
      val () =
        if TwistedRootDatum.is_distinguished (rd, delta) then ()
        else raise Fail "Cofolded.cofoldedBOTH: delta is not distinguished"

      val tStar = IntMatrix.eigenLattice (delta, 1) (* n x r; columns basis of delta-fixed weights *)
      val n = RootDatum.rank rd

      val posCoroots = RootDatum.posCorootsCols rd
      val corootsNonreduced = Sort.sort_u_rlex (List.map (fn av => vecMatMul (av, tStar)) posCoroots)
      val locate = Basic.binary_search_in (corootsNonreduced, Sort.rlex_leq)
      fun has v = Option.isSome (locate v)

      val corootsC =
        List.filter
          (fn alphavee =>
            case halfIfEven alphavee of
              NONE => true
            | SOME half => not (has half))
          corootsNonreduced

      val corootsB =
        List.filter
          (fn alphavee => not (has (vecScale (alphavee, 2))))
          corootsNonreduced

      fun restrict av = vecMatMul (av, tStar)

      fun pullbackOne alphavee : vec =
        case firstIndex (fn betavee => restrict betavee = alphavee, posCoroots) of
          NONE => raise Fail "Cofolded.cofoldedBOTH: empty pullback"
        | SOME j => List.nth (posCoroots, j)

      fun pullbackAllRoots alphavee : vec list =
        let
          fun pick betavee =
            if restrict betavee = alphavee then SOME (RootDatum.root rd betavee) else NONE
        in
          List.mapPartial pick posCoroots
        end

      fun corestrictRoot alphavee : vec =
        let
          val pullbackAlphavee = pullbackOne alphavee
          val rootsPb = pullbackAllRoots alphavee
          val v = sumVecs (n, rootsPb)
          val denom = RootDatum.dot (v, pullbackAlphavee)
          val () = if denom <> 0 then () else raise Fail "Cofolded.cofoldedBOTH: zero pairing"
          val w : ratvec = Lattice.ratvecNormalize {den = denom, nums = vecScale (v, 2)}
        in
          case LatticeAT.solve_ratvec_as_vec (tStar, w) of
            NONE => raise Fail "Cofolded.cofoldedBOTH: corestrict solve failed"
          | SOME coords => coords
        end

      val rootsC = List.map corestrictRoot corootsC
      val rootsB = List.map corestrictRoot corootsB

      val rdC = TwistedRootDatum.rootdatum_from_positive (rootsC, corootsC)
      val rdB = TwistedRootDatum.rootdatum_from_positive (rootsB, corootsB)
    in
      (rdB, rdC, tStar)
    end

  (* Port of `cofolded(InnerClass ic)` from `FPP_faces_geom.at`,
     with an InnerClass represented here by an Atlas group handle. *)
  (* Construct the cofolded datum for the group `g`:
       - returns the B-type folded root datum `rdB`
       - returns the folding matrix `m`
       - returns `j0` (currently expected to be `~1` for the cases we use). *)
  fun cofolded (g: AtlasFFI.group) : rootdatum * mat * int =
    let
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val delta = IntMatrix.parseMatText (AtlasFFI.atlas_group_distinguished_involution_text g)
      val (rdB, rdC, m) = cofoldedBOTH (rd, delta)
      val simpleB = RootDatum.simpleRootsCols rdB
      val simpleC = RootDatum.simpleRootsCols rdC
      val () = RootDatum.free rd

      fun firstDiff ([], [], _) = NONE
        | firstDiff (a :: as', b :: bs', i) = if a = b then firstDiff (as', bs', i + 1) else SOME i
        | firstDiff _ = raise Fail "Cofolded.cofolded: simple roots mismatch"

      val j0 = case firstDiff (simpleB, simpleC, 0) of NONE => ~1 | SOME j => j

      val () = RootDatum.free rdC
    in
      (rdB, m, j0)
    end
end
