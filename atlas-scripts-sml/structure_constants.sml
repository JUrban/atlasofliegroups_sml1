use "atlas-scripts-sml/misc.sml";
use "atlas-scripts-sml/matrix.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/IntMatrix.sml";

(*
  File: atlas-scripts-sml/structure_constants.sml

  Purpose
  - Standard ML translation of `atlas-scripts/structure_constants.at`.
  - Constructs a table of structure constants `N(α,β)` for a Lusztig basis of
    a semisimple Lie algebra, following the scheme described in the `.at`
    script (Geck/Lusztig conventions).

  Scope
  - This port focuses on the algebraic/combinatorial part and uses only:
      - `RootDatum` for the root system data,
      - `Misc` for basic root utilities (`root_string`, `height`, etc.),
      - `MatrixAT` for `principal_submatrix`.
  - It does not depend on the Atlas interpreter.

  Representation differences vs `.at`
  - In `.at`, `mat` is column-major and mutable. Here we store the table as a
    flat mutable `int array` of size `nRoots*nRoots` (row-major by root index).
  - Roots are represented as `int list` vectors in the same ambient coordinates
    as in `.at`.
*)

structure StructureConstants = struct
  type vec = int list
  type rootdatum = RootDatum.t

  type table =
    { root_datum: rootdatum
    , orientation_function: int -> int
    , roots: vec list
    , data: int array
    }

  fun fail where' msg = raise Fail ("StructureConstants." ^ where' ^ ": " ^ msg)

  fun vecNeg (v: vec) : vec = List.map (fn x => ~x) v
  fun vecAdd (a: vec, b: vec) : vec = ListPair.mapEq (op +) (a, b)
  fun vecSub (a: vec, b: vec) : vec = ListPair.mapEq (op -) (a, b)
  fun vecScale (k: int, v: vec) : vec = List.map (fn x => k * x) v

  fun is_root (rd: rootdatum, alpha: vec) : bool = Misc.is_root (rd, alpha)
  fun not_a_root (rd: rootdatum, alpha: vec) : bool = Misc.not_a_root (rd, alpha)

  fun roots (rd: rootdatum) : vec list = RootDatum.rootsCols rd
  fun simple_roots (rd: rootdatum) : vec list = RootDatum.simpleRootsCols rd

  fun num_roots (rd: rootdatum) : int = length (roots rd)

  fun idxOfRoot (roots: vec list) (alpha: vec) : int =
    let
      fun loop ([], _) = ~1
        | loop (r :: rs, i) = if r = alpha then i else loop (rs, i + 1)
    in
      loop (roots, 0)
    end

  fun tableSize (T: table) : int =
    let
      val n = length (#roots T)
    in
      n
    end

  fun key (n: int, i: int, j: int) : int = i * n + j

  fun get (T: table, i: int, j: int) : int =
    let
      val n = tableSize T
    in
      Array.sub (#data T, key (n, i, j))
    end

  fun set (T: table, i: int, j: int, v: int) : unit =
    let
      val n = tableSize T
    in
      Array.update (#data T, key (n, i, j), v)
    end

  (*
    m_plus/m_minus from `structure_constants.at`:
      m_+(α,β) = p+1 where (p,q) is the root string through α in direction β.
      m_-(α,β) = q+1.
  *)
  fun m_plus (rd: rootdatum, alpha: vec, beta: vec) : int =
    let
      val (p, _) = Misc.root_string (rd, alpha, beta)
    in
      p + 1
    end

  fun m_minus (rd: rootdatum, alpha: vec, beta: vec) : int =
    let
      val (_, q) = Misc.root_string (rd, alpha, beta)
    in
      q + 1
    end

  (*
    orientation_function(rd)(i) : int

    Port of `orientation_function` from `structure_constants.at`.
    Produces one of the two sign functions on the Dynkin diagram satisfying:
      f(α_i) f(α_j) = -1 whenever i,j are adjacent.
  *)
  fun orientation_function (rd: rootdatum) : int -> int =
    let
      val n = RootDatum.semisimpleRank rd
      val roots = RootDatum.simpleRootsCols rd
      val coroots = RootDatum.simpleCorootsCols rd
      fun adjacent (i: int, j: int) : bool =
        RootDatum.dot (List.nth (roots, i), List.nth (coroots, j)) <> 0

      val v = Array.array (n, 0)
      val () = if n > 0 then Array.update (v, 0, 1) else ()

      fun productNonZero () : bool =
        let
          fun loop i acc =
            if i = n then acc <> 0
            else
              let
                val x = Array.sub (v, i)
              in
                if x = 0 then false else loop (i + 1) (acc * x)
              end
        in
          loop 0 1
        end

      fun firstZero () : int option =
        let
          fun loop i =
            if i = n then NONE else if Array.sub (v, i) = 0 then SOME i else loop (i + 1)
        in
          loop 0
        end

      fun findEdge () : (int * int) option =
        let
          fun loopI i =
            if i = n then NONE
            else if Array.sub (v, i) = 0 then loopI (i + 1)
            else
              let
                fun loopJ j =
                  if j = n then loopI (i + 1)
                  else if Array.sub (v, j) <> 0 then loopJ (j + 1)
                  else if adjacent (i, j) then SOME (i, j)
                  else loopJ (j + 1)
              in
                loopJ 0
              end
        in
          loopI 0
        end

      fun fill () : unit =
        if productNonZero () then
          ()
        else
          (case findEdge () of
             SOME (i, j) =>
               (Array.update (v, j, ~(Array.sub (v, i))); fill ())
           | NONE =>
               (* Disconnected diagram: start a new component. *)
               (case firstZero () of
                  NONE => ()
                | SOME j => (Array.update (v, j, 1); fill ())))
    in
      fill ();
      fn index =>
        if 0 <= index andalso index < n then Array.sub (v, index) else 0
    end

  fun epsilon (T: table) (alpha: vec) : int =
    let
      val rd = #root_datum T
      val f = #orientation_function T
      val simps = RootDatum.simpleRootsCols rd
      fun findIndex (xs: vec list, v: vec) =
        let
          fun loop ([], _) = NONE
            | loop (x :: rest, i) = if x = v then SOME i else loop (rest, i + 1)
        in
          loop (xs, 0)
        end
    in
      case findIndex (simps, alpha) of
        SOME i => f i
      | NONE =>
          (case findIndex (simps, vecNeg alpha) of
             SOME i => ~(f i)
           | NONE => fail "epsilon" "expected +/- simple root")
    end

  fun lookup (T: table) (alpha: vec, beta: vec) : int =
    if not_a_root (#root_datum T, alpha) orelse not_a_root (#root_datum T, beta) then
      0
    else
      let
        val i = idxOfRoot (#roots T) alpha
        val j = idxOfRoot (#roots T) beta
      in
        if i < 0 orelse j < 0 then
          fail "lookup" "root not found in stored root list"
        else
          get (T, i, j)
      end

  fun update (T: table) (alpha: vec, beta: vec, n: int) : table =
    let
      val i = idxOfRoot (#roots T) alpha
      val j = idxOfRoot (#roots T) beta
      val () = if i < 0 orelse j < 0 then fail "update" "root not found" else ()
      val () = set (T, i, j, n)
    in
      T
    end

  fun initialize_structure_constant_table (rd: rootdatum, f: int -> int) : table =
    let
      val rs = roots rd
      val n = length rs
      val data = Array.array (n * n, 0)
      val T = {root_datum = rd, orientation_function = f, roots = rs, data = data}
      val simps = simple_roots rd
      fun loopAlpha [] = ()
        | loopAlpha (alpha :: restAlpha) =
            let
              fun loopBeta [] = ()
                | loopBeta (beta :: restBeta) =
                    ( if is_root (rd, vecAdd (alpha, beta)) then
                        ignore (update T (alpha, beta, epsilon T alpha * m_minus (rd, alpha, beta)))
                      else
                        ();
                      loopBeta restBeta
                    )
              val () = loopBeta rs
            in
              loopAlpha restAlpha
            end

      fun loopNegAlpha [] = ()
        | loopNegAlpha (alpha :: restAlpha) =
            let
              val malpha = vecNeg alpha
              fun loopBeta [] = ()
                | loopBeta (beta :: restBeta) =
                    ( if is_root (rd, vecAdd (malpha, beta)) then
                        ignore (update T (malpha, beta, epsilon T malpha * m_minus (rd, alpha, vecNeg beta)))
                      else
                        ();
                      loopBeta restBeta
                    )
              val () = loopBeta rs
            in
              loopNegAlpha restAlpha
            end
    in
      loopAlpha simps;
      loopNegAlpha simps;
      T
    end

  fun initialize_structure_constant_table_default (rd: rootdatum) : table =
    initialize_structure_constant_table (rd, orientation_function rd)

  fun divExactIntInf (num: IntInf.int, den: IntInf.int) : IntInf.int =
    let
      val (q, r) = IntInf.divMod (num, den)
    in
      if r = 0 then q else fail "divExact" "inexact division"
    end

  fun powNegOne (k: int) : int = if k mod 2 = 0 then 1 else ~1

  fun compute_structure_constant (T: table, alpha: vec, beta: vec) : int =
    let
      val rd = #root_datum T
    in
      if not_a_root (rd, vecAdd (alpha, beta)) then
        0
      else
        let
          val phi = Misc.simple_root_summand (rd, alpha)
          val psi = vecSub (alpha, phi)
          val () = if is_root (rd, psi) then () else fail "compute_structure_constant" "psi not a root"

          fun L a b = lookup T (a, b)
          val denom = L phi psi
          val () = if denom <> 0 then () else fail "compute_structure_constant" "denom=0"

          fun pairRootCoroot (r: vec, s: vec) : int =
            RootDatum.dot (r, RootDatum.coroot rd s)

          val res =
            if phi <> vecNeg beta andalso psi <> vecNeg beta then
              let
                val num =
                  IntInf.fromInt (L psi beta * L phi (vecAdd (psi, beta)) - L phi beta * L psi (vecAdd (phi, beta)))
                val q = divExactIntInf (num, IntInf.fromInt denom)
              in
                IntInf.toInt q
              end
            else if phi = vecNeg beta then
              let
                val term1 = IntInf.fromInt (L psi beta * L phi (vecAdd (psi, beta)))
                val term2 = IntInf.fromInt (powNegOne (Misc.height (rd, phi)) * pairRootCoroot (psi, phi))
                val num = term1 + term2
                val q = divExactIntInf (num, IntInf.fromInt denom)
              in
                IntInf.toInt q
              end
            else
              let
                (* psi = -beta *)
                val term1 =
                  IntInf.fromInt (powNegOne (Misc.height (rd, psi)) * pairRootCoroot (phi, psi))
                val term2 = IntInf.fromInt (L phi beta * L psi (vecAdd (phi, beta)))
                val num = ~(term1 + term2)
                val q = divExactIntInf (num, IntInf.fromInt denom)
              in
                IntInf.toInt q
              end
        in
          res
        end
    end

  fun update_structure_constant_table (T: table, h: int) : table =
    let
      val rd = #root_datum T
      val rootsH = Misc.roots_of_height (rd, h)
      val rs = #roots T
      fun loopAlpha [] = ()
        | loopAlpha (alpha :: rest) =
            let
              fun loopBeta [] = ()
                | loopBeta (beta :: betas) =
                    (ignore (update T (alpha, beta, compute_structure_constant (T, alpha, beta))); loopBeta betas)
            in
              loopBeta rs;
              loopAlpha rest
            end
    in
      loopAlpha rootsH;
      T
    end

  fun fill_structure_constant_table (T: table) : table =
    let
      val rd = #root_datum T
      val mh = Misc.max_height rd
      fun loop i =
        if i > mh - 1 then ()
        else (ignore (update_structure_constant_table (T, i)); loop (i + 1))
    in
      loop 2;
      T
    end

  fun structure_constant_table (rd: rootdatum) : table =
    fill_structure_constant_table (initialize_structure_constant_table_default rd)

  (* Convenience wrappers matching the `.at` naming. *)
  fun N (T: table) (alpha: vec, beta: vec) : int = lookup T (alpha, beta)

  (* A diagnostic “Jacobi identity” triple: the three summands appearing in the
     usual scalar Jacobi relation for structure constants. *)
  fun jacobi_formula (T: table) (alpha: vec, beta: vec, gamma: vec) : int list =
    let
      val N = N T
      val rd = #root_datum T
      fun pairRootCoroot (r: vec, s: vec) : int =
        RootDatum.dot (r, RootDatum.coroot rd s)
      fun term (x, y) = N (x, y)
    in
      if alpha = beta orelse alpha = gamma orelse beta = gamma then
        [0, 0, 0]
      else if alpha = vecNeg beta then
        [ term (beta, gamma) * term (alpha, vecAdd (beta, gamma))
        , pairRootCoroot (gamma, alpha) * powNegOne (Misc.height (rd, alpha)) * ~1
        , term (gamma, alpha) * term (beta, vecAdd (gamma, alpha))
        ]
      else if alpha = vecNeg gamma then
        [ term (beta, gamma) * term (alpha, vecAdd (beta, gamma))
        , term (alpha, beta) * term (gamma, vecAdd (alpha, beta))
        , pairRootCoroot (beta, alpha) * powNegOne (Misc.height (rd, alpha))
        ]
      else if beta = vecNeg gamma then
        [ pairRootCoroot (alpha, beta) * powNegOne (Misc.height (rd, beta)) * ~1
        , term (alpha, beta) * term (gamma, vecAdd (alpha, beta))
        , term (gamma, alpha) * term (beta, vecAdd (gamma, alpha))
        ]
      else
        [ term (beta, gamma) * term (alpha, vecAdd (beta, gamma))
        , term (alpha, beta) * term (gamma, vecAdd (alpha, beta))
        , term (gamma, alpha) * term (beta, vecAdd (gamma, alpha))
        ]
    end

  fun jacobi_structure_constants_identity (T: table) : bool =
    let
      val rs = #roots T
      fun okTriple (a, b, c) =
        let
          val ts = jacobi_formula T (a, b, c)
        in
          List.foldl (op +) 0 ts = 0
        end
    in
      List.all okTriple (List.concat (List.map (fn a => List.concat (List.map (fn b => List.map (fn c => (a, b, c)) rs) rs)) rs))
    end

  (* Convert the internal flat table to an `IntMatrix.mat` for extraction. *)
  fun toMat (T: table) : IntMatrix.mat =
    let
      val n = tableSize T
      fun row i = List.tabulate (n, fn j => get (T, i, j))
    in
      List.tabulate (n, row)
    end

  fun sub_table_by_indices (T: table, S: int list) : IntMatrix.mat =
    MatrixAT.principal_submatrix (toMat T, S)

  fun sub_table_by_roots (T: table, alphas: vec list) : IntMatrix.mat =
    let
      val is = List.map (idxOfRoot (#roots T)) alphas
      val () = if List.all (fn i => i >= 0) is then () else fail "sub_table" "root not found"
    in
      sub_table_by_indices (T, is)
    end
end
