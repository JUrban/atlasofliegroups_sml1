use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(* Partial port of `atlas-scripts/simple_factors.at`.
   Focuses on Dynkin-diagram decomposition and Bourbaki reordering. *)
structure SimpleFactors = struct
  type rootdatum = RootDatum.t

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
end
