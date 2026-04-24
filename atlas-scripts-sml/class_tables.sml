use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/weylgroup_at.sml";
use "atlas-scripts-sml/IntMatrix.sml";
use "atlas-scripts-sml/hash.sml";

(*
  File: atlas-scripts-sml/class_tables.sml

  Purpose
  - Partial SML translation of `atlas-scripts/class_tables.at`.
  - The `.at` file defines a substantial amount of Weyl-group conjugacy-class
    infrastructure (`WeylClassTable`) for many root data types.

  Scope of this port (current)
  - This module is intentionally incremental. At this stage we implement only
    the G2 Weyl-group class table (`class_table_G`) in a way that is sufficient
    to support character-table scripts such as `character_table_G.at`.
  - Types F4/E6/E7/E8, and the generic `W_class_table` constructor, are not yet
    ported to SML.

  Representation choice
  - We represent Weyl-group elements as `WeylWord.t` (simple reflection words),
    and we compute class membership for G2 using the same invariant as the `.at`
    code: `magic_coweight*(w*rho) mod 15`.
*)

structure WeylClassTable = struct
  type weyl_word = WeylWord.t

  type t =
    { n_classes: int
    , class_representatives: weyl_word list
    , class_sizes: int list
    , class_orders: int list
    , class_of: weyl_word -> int
    , class_power: int * int -> int
    }
end

structure ClassTables = struct
  type group = AtlasFFI.group
  type weyl_word = WeylWord.t
  type ratvec = Lattice.ratvec
  type rootdatum = RootDatum.t
  type mat = IntMatrix.mat

  (* -------------------- G2 -------------------- *)

  val G2_class_words : weyl_word list =
    [ []
    , [ 1 ]
    , [ 0 ]
    , [ 0, 1, 0, 1, 0, 1 ]
    , [ 0, 1, 0, 1 ]
    , [ 0, 1 ]
    ]

  fun looksLikeG2 (g: group) : bool =
    AtlasFFI.atlas_group_semisimple_rank g = 2
    andalso AtlasFFI.atlas_group_rank g = 2

  fun rho (g: group) : ratvec =
    AllParameters.parseRatWeightText (AtlasFFI.atlas_group_rho_text g)

  (* Pair the G2 “magic coweight” with a weight expressed in fundamental-weight
     coordinates. For a weight `v = (v0,v1)`, this pairing is `5*v0 + 9*v1`.

     In the `.at` file:
       magic_coweight = 5*coroot(rd,map[0]) + 9*coroot(rd,map[1]).
     In fundamental-weight coordinates, `coroot(i)` pairs with coordinate `i`.
  *)
  fun g2_magic_pair (v: ratvec) : int =
    let
      val v = Lattice.ratvecNormalize v
      val den = #den v
      val nums = #nums v
      val () =
        if length nums = 2 then
          ()
        else
          raise Fail "ClassTables.g2_magic_pair: expected rank 2"
      val n0 = List.nth (nums, 0)
      val n1 = List.nth (nums, 1)
      val num = 5 * n0 + 9 * n1
    in
      if den = 1 then
        num
      else if num mod den = 0 then
        num div den
      else
        raise Fail "ClassTables.g2_magic_pair: non-integral pairing"
    end

  fun imod (n: int, m: int) : int =
    let
      val r = n mod m
    in
      if r < 0 then r + m else r
    end

  (* Map residue class in `0..14` to a conjugacy class number in `0..5`,
     or `~1` for “die” (should not occur for Weyl group elements in G2). *)
  val g2_residue_to_class : int list =
    [ ~1, 3, 1, ~1, 2, ~1, ~1, 5, 4, ~1, ~1, 1, ~1, 2, 0 ]

  fun class_table_G (g: group) : WeylClassTable.t =
    if not (looksLikeG2 g) then
      raise Fail "ClassTables.class_table_G: expected a rank-2 semisimple group (G2)"
    else
      let
        val rho0 = rho g

        fun class_of_word (w: weyl_word) : int =
          let
            val v = WeylWord.actRatvec (g, w, rho0)
            val r = imod (g2_magic_pair v, 15)
            val cls = List.nth (g2_residue_to_class, r)
          in
            if cls < 0 then raise Fail "ClassTables.class_table_G: invariant hit 'die'" else cls
          end

        val class_sizes = [ 1, 3, 3, 1, 2, 2 ]
        val class_orders = [ 1, 2, 2, 2, 3, 6 ]

        fun class_power (i: int, k: int) : int =
          if i < 0 orelse i >= 6 then
            raise Fail "ClassTables.class_power(G2): class index out of range"
          else
            (case i of
               0 => 0
             | 1 => (case k of 0 => 0 | 1 => 1 | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | 2 => (case k of 0 => 0 | 1 => 2 | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | 3 => (case k of 0 => 0 | 1 => 3 | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | 4 => (case k of 0 => 0 | 1 => 4 | 2 => 4 | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | 5 =>
                 (case k of
                    0 => 0
                  | 1 => 5
                  | 2 => 4
                  | 3 => 3
                  | 4 => 4
                  | 5 => 5
                  | _ => raise Fail "ClassTables.class_power(G2): bad k")
             | _ => raise Fail "ClassTables.class_power(G2): unreachable")
      in
        { n_classes = 6
        , class_representatives = G2_class_words
        , class_sizes = class_sizes
        , class_orders = class_orders
        , class_of = class_of_word
        , class_power = class_power
        }
      end

  (* -------------------- F4 (Kondo order; brute-force) -------------------- *)

  fun looksLikeF4 (rd: rootdatum) : bool =
    RootDatum.semisimpleRank rd = 4 andalso RootDatum.rank rd = 4

  fun mat_of_word (rd: rootdatum, w: weyl_word) : mat =
    let
      val n = RootDatum.rank rd
      val gens = List.tabulate (RootDatum.semisimpleRank rd, fn s => WeylgroupAT.reflection_matrix_simple (rd, s))
      fun step (s: int, acc: mat) : mat =
        IntMatrix.matMul (List.nth (gens, s), acc)
    in
      List.foldl step (IntMatrix.identity n) w
    end

  fun mat_mul (a: mat, b: mat) : mat = IntMatrix.matMul (a, b)

  fun mat_pow (m: mat, k: int) : mat =
    if k < 0 then
      raise Fail "ClassTables.mat_pow: negative exponent"
    else
      let
        val (n, _) = IntMatrix.matShape m
        fun loop (0, acc) = acc
          | loop (t, acc) = loop (t - 1, mat_mul (acc, m))
      in
        loop (k, IntMatrix.identity n)
      end

  fun order_of_mat (m: mat) : int =
    let
      val (n, _) = IntMatrix.matShape m
      val id = IntMatrix.identity n
      fun loop (k, acc) =
        if acc = id then k
        else loop (k + 1, mat_mul (acc, m))
    in
      if m = id then 1 else loop (1, m)
    end

  (* The 25 Kondo-order conjugacy class representatives for W(F4),
     as Weyl words (not reduced); this matches the `classes_Kondo_F4` list in
     `class_tables.at`, but omits the final `minimal_representative` step
     (which does not change conjugacy class membership). *)
  val F4_Kondo_rep_words : weyl_word list =
    let
      val a = [ 1 ]
      val b = [ 2, 1, 2 ]
      val c = [ 2, 3, 2, 1, 2, 3, 2 ]
      val d = [ 0 ]
      val tau = [ 2 ]
      val sigma = [ 2, 3 ]
      val abcd = a @ b @ c @ d
      val e = abcd @ abcd
      val z = abcd @ abcd @ abcd
    in
      [ [] (* id *)
      , z
      , a @ b
      , e
      , e @ z
      , a @ d @ b @ d @ c @ d
      , sigma
      , sigma @ z
      , sigma @ e
      , sigma @ e @ z
      , c @ d @ sigma
      , d
      , d @ z
      , sigma @ d
      , sigma @ d @ z
      , a @ d @ b
      , tau
      , tau @ z
      , e @ tau
      , e @ tau @ z
      , c @ a @ tau
      , tau @ d
      , a @ tau
      , a @ tau @ z
      , c @ d @ b @ tau
      ]
    end

  fun class_table_F4_kondo (rd: rootdatum) : WeylClassTable.t =
    if not (looksLikeF4 rd) then
      raise Fail "ClassTables.class_table_F4_kondo: expected a simple F4 root datum"
    else
      let
        val ssr = RootDatum.semisimpleRank rd
        val n = RootDatum.rank rd
        val id = IntMatrix.identity n
        val gen_mats = List.tabulate (ssr, fn s => WeylgroupAT.reflection_matrix_simple (rd, s))
        val gens_left_actions = List.map (fn r => (fn m => mat_mul (r, m))) gen_mats

        val elem_hash =
          Hash.make_hash_reserve ({hash_code = Hash.hash_code_mat, eq = (op =) }, 2048)

        val () = ignore (#match elem_hash id)
        val () = ignore (Hash.exhaust (gens_left_actions, elem_hash))
        val elem_count = #size elem_hash ()
        val elems = #list elem_hash ()

        val () =
          if elem_count = 1152 then
            ()
          else
            raise Fail ("ClassTables.class_table_F4_kondo: expected |W(F4)|=1152, got " ^ Int.toString elem_count)

        fun elem_index (m: mat) : int =
          let
            val j = Hash.lookup elem_hash m
          in
            if j < 0 then raise Fail "ClassTables.class_table_F4_kondo: element not in generated group" else j
          end

        fun elem_at (j: int) : mat = #index elem_hash j

        fun conj_by_gen (r: mat) (m: mat) : mat = mat_mul (r, mat_mul (m, r))

        (* Partition elements into conjugacy classes using generator conjugations. *)
        val class_of_elem = Array.array (elem_count, ~1)
        val class_reps_internal : int list ref = ref []
        val class_sizes_internal : int list ref = ref []

        fun build_class_from (seed_idx: int, class_id: int) : int list =
          let
            val visited = Array.array (elem_count, false)
            fun push (j: int, q: int list) =
              if Array.sub (visited, j) then q else (Array.update (visited, j, true); j :: q)
            fun loop ([], acc) = acc
              | loop (j :: q, acc) =
                  let
                    val mj = elem_at j
                    val q' =
                      List.foldl
                        (fn (r, qq) =>
                           let
                             val mk = conj_by_gen r mj
                             val k = elem_index mk
                           in
                             push (k, qq)
                           end)
                        q
                        gen_mats
                  in
                    loop (q', j :: acc)
                  end
          in
            loop (push (seed_idx, []), [])
          end

        fun partition_loop (j: int, next_class: int) : int =
          if j = elem_count then
            next_class
          else if Array.sub (class_of_elem, j) >= 0 then
            partition_loop (j + 1, next_class)
          else
            let
              val members = build_class_from (j, next_class)
              val () = List.app (fn k => Array.update (class_of_elem, k, next_class)) members
              val () = class_reps_internal := j :: !class_reps_internal
              val () = class_sizes_internal := length members :: !class_sizes_internal
            in
              partition_loop (j + 1, next_class + 1)
            end

        val num_classes = partition_loop (0, 0)
        val () =
          if num_classes = 25 then () else raise Fail ("ClassTables.class_table_F4_kondo: expected 25 classes, got " ^ Int.toString num_classes)

        val reps_internal = List.rev (!class_reps_internal)
        val sizes_internal = List.rev (!class_sizes_internal)

        val () =
          if List.foldl (op +) 0 sizes_internal = elem_count then
            ()
          else
            raise Fail "ClassTables.class_table_F4_kondo: class sizes do not sum to group order"

        (* Map internal class id -> Kondo index by locating the class of each Kondo representative. *)
        val internal_of_kondo =
          let
            fun cls_of_word w =
              let
                val m = mat_of_word (rd, w)
                val j = elem_index m
              in
                Array.sub (class_of_elem, j)
              end
          in
            List.map cls_of_word F4_Kondo_rep_words
          end

        val () =
          let
            fun distinct xs =
              let
                val h = Hash.make_hash_reserve (Hash.triv_hash_info, 64)
                val () = List.app (fn x => ignore (#match h x)) xs
              in
                #size h () = length xs
              end
          in
            if length internal_of_kondo = 25 andalso distinct internal_of_kondo then
              ()
            else
              raise Fail "ClassTables.class_table_F4_kondo: Kondo representatives did not hit 25 distinct classes"
          end

        val kondo_of_internal = Array.array (num_classes, ~1)
        val () = List.app (fn (kondo_idx, internal_cls) => Array.update (kondo_of_internal, internal_cls, kondo_idx))
          (ListPair.zip (List.tabulate (25, fn i => i), internal_of_kondo))

        fun internal_to_kondo (internal_cls: int) : int =
          let
            val k = Array.sub (kondo_of_internal, internal_cls)
          in
            if k < 0 then raise Fail "ClassTables.class_table_F4_kondo: missing internal->kondo mapping" else k
          end

        val class_sizes_kondo = List.map (fn internal_cls => List.nth (sizes_internal, internal_cls)) internal_of_kondo

        val class_orders_kondo =
          List.map
            (fn w => order_of_mat (mat_of_word (rd, w)))
            F4_Kondo_rep_words

        (* Precompute the power map table in Kondo order. *)
        val power_table : int list list =
          List.tabulate
            (25, fn i =>
               let
                 val ord = List.nth (class_orders_kondo, i)
                 val rep = mat_of_word (rd, List.nth (F4_Kondo_rep_words, i))
                 fun cls_of_mat m =
                   let
                     val j = elem_index m
                     val internal_cls = Array.sub (class_of_elem, j)
                   in
                     internal_to_kondo internal_cls
                   end
               in
                 List.tabulate (ord, fn k => cls_of_mat (mat_pow (rep, k)))
               end)

        fun class_power (i: int, k: int) : int =
          if i < 0 orelse i >= 25 then
            raise Fail "ClassTables.class_power(F4): class index out of range"
          else
            let
              val row = List.nth (power_table, i)
            in
              if k < 0 orelse k >= length row then
                raise Fail "ClassTables.class_power(F4): exponent out of range"
              else
                List.nth (row, k)
            end

        fun class_of (w: weyl_word) : int =
          let
            val m = mat_of_word (rd, w)
            val j = elem_index m
            val internal_cls = Array.sub (class_of_elem, j)
          in
            internal_to_kondo internal_cls
          end
      in
        { n_classes = 25
        , class_representatives = F4_Kondo_rep_words
        , class_sizes = class_sizes_kondo
        , class_orders = class_orders_kondo
        , class_of = class_of
        , class_power = class_power
        }
      end
end
