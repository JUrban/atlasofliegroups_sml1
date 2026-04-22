use "atlas-scripts-sml/LieType.sml";
use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/diagram.sml

  Purpose
  - Compute diagram automorphisms of a (semisimple) Lie type, following the
    logic in `atlas-scripts/diagram.at`.
  - Used by the folding/twisted-root-datum code to lift diagram permutations to
    lattice automorphisms.

  Output
  - A diagram automorphism is represented as a permutation of the simple nodes
    `0..(ssrank-1)`.
*)
structure Diagram = struct
  type permutation = int list

  (* Identity permutation on `0..n-1`. *)
  fun identityPerm n = List.tabulate (n, fn i => i)

  (* Reverse permutation `i ↦ n-1-i`. *)
  fun reversePerm n = List.tabulate (n, fn i => n - 1 - i)

  (* Port of `sanitize` from `atlas-scripts/diagram.at`:
     toggles B2<->C2 naming (and swaps the corresponding node positions in sigma). *)
  (* Normalize B2/C2 naming conventions by swapping letters and permutation entries. *)
  fun sanitize (to_C: bool) (lt: LieType.t, sigma: int list) : LieType.t * int list =
    let
      val (fro, too) = if to_C then (#"B", #"C") else (#"C", #"B")
      val sigmaArr = Array.fromList sigma

      fun swap (i, j) =
        let
          val xi = Array.sub (sigmaArr, i)
          val xj = Array.sub (sigmaArr, j)
        in
          Array.update (sigmaArr, i, xj);
          Array.update (sigmaArr, j, xi)
        end

      fun loop ([], _, acc) = (List.rev acc, Array.foldr (op ::) [] sigmaArr)
        | loop ((c, r) :: rest, sum, acc) =
            if r = 2 andalso c = fro then
              (swap (sum, sum + 1); loop (rest, sum + r, (too, r) :: acc))
            else
              loop (rest, sum + r, (c, r) :: acc)
    in
      loop (LieType.simple_factors lt, 0, [])
    end

  (* Port of `simple_automorphisms` for a single simple type. *)
  (* Diagram automorphisms of a simple Dynkin type, as permutations of nodes. *)
  fun simple_automorphisms (typeLetter: char, rank: int) : permutation list =
    let
      val id = identityPerm rank
      fun swapLastTwo () =
        if rank < 2 then id
        else List.tabulate (rank, fn i => if i = rank - 2 then rank - 1 else if i = rank - 1 then rank - 2 else i)
    in
      case typeLetter of
        #"A" => if rank <= 1 then [id] else [id, reversePerm rank]
      | #"B" => [id]
      | #"C" => [id]
      | #"D" =>
          if rank = 4 then
            [ id
            , [0, 1, 3, 2]
            , [2, 1, 0, 3]
            , [2, 1, 3, 0]
            , [3, 1, 0, 2]
            , [3, 1, 2, 0]
            ]
          else
            [id, swapLastTwo ()]
      | #"E" => if rank = 6 then [id, [5, 1, 4, 3, 2, 0]] else [id]
      | #"F" => [id]
      | #"G" => [id]
      | _ => raise Fail "Diagram.simple_automorphisms: unknown type"
    end

  (* Group indices of equal simple factors (same letter+rank). *)
  fun groupEqualFactors (factors: LieType.simple_factor list) : int list list =
    let
      fun add (key, idx, []) = [(key, [idx])]
        | add (key, idx, (k, vs) :: rest) =
            if k = key then (k, idx :: vs) :: rest else (k, vs) :: add (key, idx, rest)

      fun build (_, []) = []
        | build (i, (f as (c, r)) :: rest) = add (f, i, build (i + 1, rest))

      val groups = build (0, factors)
    in
      List.map (fn (_, idxs) => List.rev idxs) groups
    end

  (* List permutations of a list (factorial growth; used only for small lists). *)
  fun permutations (xs: 'a list) : 'a list list =
    let
      fun ins (x, []) = [[x]]
        | ins (x, y :: ys) =
            (x :: y :: ys) :: List.map (fn zs => y :: zs) (ins (x, ys))
      fun perms [] = [[]]
        | perms (x :: xs) = List.concat (List.map (fn p => ins (x, p)) (perms xs))
    in
      perms xs
    end

  (* Cartesian product of a list of choice lists. *)
  fun cartesianProduct (xss: 'a list list) : 'a list list =
    let
      fun step (xs, acc) = List.concat (List.map (fn a => List.map (fn x => x :: a) xs) acc)
    in
      List.foldr step [[]] xss
    end

  (* All diagram automorphisms of a (semisimple) Lie type. *)
  fun diagram_automorphisms (lt: LieType.t) : permutation list =
    let
      val factors = LieType.simple_factors lt
      val n = LieType.semisimple_rank lt

      val starts =
        let
          fun loop ([], _, acc) = List.rev acc
            | loop ((_, r) :: rest, s, acc) = loop (rest, s + r, s :: acc)
        in
          loop (factors, 0, [])
        end

      fun start i = List.nth (starts, i)
      fun rank i = #2 (List.nth (factors, i))

      val internalAutos =
        let
          fun loop ([], _, acc) = List.rev acc
            | loop ((c, r) :: rest, i, acc) = loop (rest, i + 1, (i, simple_automorphisms (c, r)) :: acc)
        in
          loop (factors, 0, [])
        end

      val internalChoices =
        cartesianProduct (List.map (fn (i, autos) => List.map (fn a => (i, a)) autos) internalAutos)

      val equalGroups = groupEqualFactors factors
      val nontrivialGroups = List.filter (fn idxs => length idxs > 1) equalGroups
      val groupPermChoices = cartesianProduct (List.map permutations nontrivialGroups)

      fun buildPerm (destOf, autosFor) : permutation =
        let
          val p = Array.tabulate (n, fn i => i)
          fun setNode (factorIdx, auto) =
            let
              val s0 = start factorIdx
              val s1 = start (destOf factorIdx)
              val r = rank factorIdx
              val () = if length auto = r then () else raise Fail "Diagram.buildPerm: auto length mismatch"
              fun loop j =
                if j >= r then ()
                else (Array.update (p, s0 + j, s1 + List.nth (auto, j)); loop (j + 1))
            in
              loop 0
            end
          val () = List.app setNode autosFor
        in
          List.tabulate (n, fn i => Array.sub (p, i))
        end

      fun autosListToLookup autos =
        let
          fun at i =
            case List.find (fn (j, _) => j = i) autos of
              SOME (_, a) => a
            | NONE => raise Fail "Diagram: missing auto"
        in
          List.tabulate (length factors, fn i => (i, at i))
        end

      val factorCount = length factors
      val identityGroupPerms = [[]]
      val groupPermChoices = if null nontrivialGroups then identityGroupPerms else groupPermChoices

      fun mkAutosFor ic =
        let
          val tbl = Array.array (factorCount, [])
          val () = List.app (fn (i, a) => Array.update (tbl, i, a)) ic
        in
          List.tabulate (factorCount, fn i => (i, Array.sub (tbl, i)))
        end

      fun mkDestOf gp =
        let
          fun findi pred xs =
            let
              fun loop (_, []) = NONE
                | loop (k, x :: rest) = if pred x then SOME (k, x) else loop (k + 1, rest)
            in
              loop (0, xs)
            end

          (* gp is list of permuted groups; need access to original groups to map. *)
          fun destOfFactor i =
            let
              fun search ([], []) = i
                | search (g :: gs, perm :: ps) =
                    (case findi (fn x => x = i) g of
                       NONE => search (gs, ps)
                     | SOME (k, _) => List.nth (perm, k))
                | search _ = raise Fail "Diagram: groupPerm mismatch"
            in
              search (nontrivialGroups, gp)
            end
        in
          destOfFactor
        end

      val perms =
        List.concat
          (List.map
             (fn ic =>
                let
                  val autosFor = mkAutosFor ic
                in
                  List.map
                    (fn gp =>
                       let
                         val destOf = mkDestOf gp
                       in
                         buildPerm (destOf, autosFor)
                       end)
                    groupPermChoices
                end)
             internalChoices)

      fun leqIntList (xs: int list, ys: int list) : bool =
        let
          fun loop ([], []) = true
            | loop ([], _ :: _) = true
            | loop (_ :: _, []) = false
            | loop (a :: as', b :: bs') =
                if a <> b then a < b else loop (as', bs')
        in
          loop (xs, ys)
        end
    in
      (* ensure uniqueness and stable ordering *)
      Basic.sort_u leqIntList perms
    end

  type 'a iterator = {get: unit -> 'a option, incr: unit -> unit}

  (* Build a simple iterator over a list. *)
  fun iteratorOfList (xs: 'a list) : 'a iterator =
    let
      val i = ref 0
      val n = length xs
      fun get () = if !i < n then SOME (List.nth (xs, !i)) else NONE
      fun incr () = i := !i + 1
    in
      {get = get, incr = incr}
    end

  (* Iterator over `diagram_automorphisms lt`. *)
  fun diagram_automorphism_iterator (lt: LieType.t) : permutation iterator =
    iteratorOfList (diagram_automorphisms lt)
end
