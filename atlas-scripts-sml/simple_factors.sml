use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(* Partial port of `atlas-scripts/simple_factors.at`.
   Focuses on Dynkin-diagram decomposition and Bourbaki reordering. *)
structure SimpleFactors = struct
  type rootdatum = RootDatum.t

  type ratvec = Lattice.ratvec

  fun mapi f ys =
    let
      fun loop (_, [], acc) = List.rev acc
        | loop (i, z :: zs, acc) = loop (i + 1, zs, f (i, z) :: acc)
    in
      loop (0, ys, [])
    end

  fun ratvecZero (n: int) : ratvec = {den = 1, nums = List.tabulate (n, fn _ => 0)}

  val ratvecNormalize = Lattice.ratvecNormalize

  fun ratvecSub (u: ratvec, v: ratvec) : ratvec = Lattice.ratvecSub (u, v)

  fun ratvecDotIntVec (u: ratvec, a: int list) : ratvec =
    let
      val () = if length (#nums u) = length a then () else raise Fail "SimpleFactors: dot length mismatch"
      val num = List.foldl (op +) 0 (ListPair.mapEq (op *) (#nums u, a))
    in
      ratvecNormalize {den = #den u, nums = [num]}
    end

  fun ratIsZero (u: ratvec) : bool =
    let
      val u = ratvecNormalize u
    in
      List.all (fn x => x = 0) (#nums u)
    end

  fun ratvecIsZero (u: ratvec) : bool = ratIsZero u

  fun parseCartanMatrixTypeText (s: string) : LieType.t * int list =
    let
      val parts = String.fields (fn c => c = #"|") s
      val () = if length parts = 2 then () else raise Fail "SimpleFactors: bad Cartan_matrix_type format"
      val ltPart = String.tokens Char.isSpace (List.nth (parts, 0))
      val piPart = String.tokens Char.isSpace (List.nth (parts, 1))

      fun parseLT toks =
        (case toks of
           kTok :: rest =>
             (case Int.fromString kTok of
                SOME k =>
                  let
                    fun loop (0, xs, acc) = if null xs then List.rev acc else raise Fail "SimpleFactors: extra lt tokens"
                      | loop (n, cTok :: rTok :: xs, acc) =
                          let
                            val () = if String.size cTok = 1 then () else raise Fail "SimpleFactors: bad type token"
                            val c = String.sub (cTok, 0)
                            val r =
                              (case Int.fromString rTok of
                                 SOME rr => rr
                               | NONE => raise Fail "SimpleFactors: bad rank token")
                          in
                            loop (n - 1, xs, (c, r) :: acc)
                          end
                      | loop _ = raise Fail "SimpleFactors: truncated lt tokens"
                  in
                    loop (k, rest, [])
                  end
              | NONE => raise Fail "SimpleFactors: bad lt header")
         | _ => raise Fail "SimpleFactors: missing lt header")

      fun parsePi toks =
        (case toks of
           nTok :: rest =>
             (case Int.fromString nTok of
                SOME n =>
                  if length rest <> n then
                    raise Fail "SimpleFactors: bad perm length"
                  else
                    List.map
                      (fn t =>
                         case Int.fromString t of
                           SOME x => x
                         | NONE => raise Fail "SimpleFactors: bad perm entry")
                      rest
              | NONE => raise Fail "SimpleFactors: bad perm header")
         | _ => raise Fail "SimpleFactors: missing perm header")
    in
      (parseLT ltPart, parsePi piPart)
    end

  fun cartan_matrix_type (cm: IntMatrix.mat) : LieType.t * int list =
    let
      val out = AtlasFFI.atlas_intmat_cartan_matrix_type_text (IntMatrix.matToText cm)
    in
      if out = "-1" then
        raise Fail ("SimpleFactors.cartan_matrix_type: failed: " ^ AtlasFFI.atlas_last_error ())
      else
        parseCartanMatrixTypeText out
    end

  (* Port of `permute` from `simple_factors.at` (simple roots only). *)
  fun permute (rd: rootdatum, sigma: int list) : rootdatum =
    let
      val sr = RootDatum.simpleRootsCols rd
      val scr = RootDatum.simpleCorootsCols rd
      val () =
        if length sigma = length sr andalso length sigma = length scr then ()
        else raise Fail "SimpleFactors.permute: length mismatch"

      fun nth xs i = List.nth (xs, i)
      val sr' = List.map (nth sr) sigma
      val scr' = List.map (nth scr) sigma
      val r = RootDatum.rank rd

      fun matFromColumns cols =
        (case cols of
           [] => List.tabulate (r, fn _ => [])
         | _ =>
             let
               fun row i = List.map (fn c => List.nth (c, i)) cols
             in
               List.tabulate (r, row)
             end)
    in
      RootDatum.newFromSimpleMats (matFromColumns sr', matFromColumns scr', false)
    end

  fun reorder_diagram_Bourbaki (rd: rootdatum) : rootdatum * int list =
    let
      val (_, pi) = cartan_matrix_type (RootDatum.cartanMatrix rd)
    in
      (permute (rd, pi), pi)
    end

  fun stratify (rd: rootdatum) : rootdatum =
    #1 (reorder_diagram_Bourbaki rd)

  fun diagram_components (rd: rootdatum) : int list list =
    let
      val (lt, pi) = cartan_matrix_type (RootDatum.cartanMatrix rd)
      fun slice (xs, i, k) = List.take (List.drop (xs, i), k)
      fun loop ([], _, acc) = List.rev acc
        | loop ((_, r) :: rest, pos, acc) = loop (rest, pos + r, slice (pi, pos, r) :: acc)
    in
      loop (LieType.simple_factors lt, 0, [])
    end

  fun diagram_component (rd: rootdatum, i: int) : int list =
    let
      val comps = diagram_components rd
      fun has xs = List.exists (fn j => j = i) xs
    in
      case List.find has comps of
        SOME c => c
      | NONE => []
    end

  fun number_simple_factors (rd: rootdatum) : int =
    length (diagram_components rd)

  fun root_coradical (rd: rootdatum) : IntMatrix.mat =
    RootDatum.rootCoradicalMat rd

  fun coroot_radical (rd: rootdatum) : IntMatrix.mat =
    RootDatum.corootRadicalMat rd

  (* RootDatum with simple roots those indexed by S (in that order). *)
  fun sub_datum (rd: rootdatum, s: int list) : rootdatum =
    let
      val sr = RootDatum.simpleRootsCols rd
      val scr = RootDatum.simpleCorootsCols rd
      fun pick xs = List.map (fn i => List.nth (xs, i)) s
      val r = RootDatum.rank rd
      fun matFromColumns cols =
        (case cols of
           [] => List.tabulate (r, fn _ => [])
         | _ =>
             let
               fun row i = List.map (fn c => List.nth (c, i)) cols
             in
               List.tabulate (r, row)
             end)
    in
      RootDatum.newFromSimpleMats (matFromColumns (pick sr), matFromColumns (pick scr), false)
    end

  fun simple_factors (rd: rootdatum) : rootdatum list =
    List.map (fn s => sub_datum (rd, s)) (diagram_components rd)

  (* Port of the cheap comparison predicates from `atlas-scripts/simple_factors.at`. *)
  fun same_weight_projection_simple_factor (rd: rootdatum, i: int) : ratvec * ratvec -> bool =
    let
      val comp = diagram_component (rd, i)
      val coroots = RootDatum.simpleCorootsCols rd
      val picked = List.map (fn j => List.nth (coroots, j)) comp

      fun ok (v0: ratvec, w0: ratvec) : bool =
        let
          val diff = ratvecSub (v0, w0)
          fun test a = ratIsZero (ratvecDotIntVec (diff, a))
        in
          List.all test picked
        end
    in
      ok
    end

  fun same_coweight_projection_simple_factor (rd: rootdatum, i: int) : ratvec * ratvec -> bool =
    let
      val comp = diagram_component (rd, i)
      val roots = RootDatum.simpleRootsCols rd
      val picked = List.map (fn j => List.nth (roots, j)) comp

      fun ok (v0: ratvec, w0: ratvec) : bool =
        let
          val diff = ratvecSub (v0, w0)
          fun test a = ratIsZero (ratvecDotIntVec (diff, a))
        in
          List.all test picked
        end
    in
      ok
    end

  (* Project a weight to the Q-span of roots in the simple factor containing node i.
     This is the direct analogue of `project_on_simple_factor(rd,i,wt)` in `simple_factors.at`,
     but returns a `ratvec` directly. *)
  fun project_on_simple_factor_weight (rd: rootdatum, i: int, wt: ratvec) : ratvec =
    let
      val comp = diagram_component (rd, i)
      val r = RootDatum.rank rd
      val ssr = RootDatum.semisimpleRank rd

      val M = root_coradical rd (* r x r *)
      val (J, d) = MatrixAT.weak_left_inverse M (* J*M = d*I, so J = d*M^{-1} *)

      val coordsNum = IntMatrix.matVecMul (J, #nums wt)
      val coordsDen = #den wt * d

      fun keepIndex j = List.exists (fn k => k = j) comp
      fun prune (j, x) = if j < ssr andalso keepIndex j then x else 0
      val coordsNum' = mapi prune coordsNum

      val projNum = IntMatrix.matVecMul (M, coordsNum')
    in
      ratvecNormalize {den = coordsDen, nums = projNum}
    end

  (* Dual approach: project a coweight in rd by projecting the same coordinate vector
     as a weight in the dual root datum. *)
  fun project_on_simple_factor_coweight (rd: rootdatum, i: int, cwt: ratvec) : ratvec =
    let
      val rdDual = RootDatum.dual rd
      val proj = project_on_simple_factor_weight (rdDual, i, cwt)
      val () = RootDatum.free rdDual
    in
      proj
    end
end
