(*
  A first CakeML-monadic model of a ParamHash-like bucketed set.

  This is not yet tied to Atlas parameters; it uses `num` as a stand-in for the
  parameter type, `hash_mod p m = p MOD m`, and equality is just `(=)`.

  The purpose is to:
    - exercise the monadic translator on the core state shape (refs + resizable
      arrays + exceptions), and
    - provide a place to start proving/strengthening the "hash-set skeleton"
      correctness claims from `VERIFY_ESTIMATE.md` (Stage A/Stage B).

  In later steps we can:
    - parameterise equality and hashing (or model them as abstract oracles),
    - add stable indexing + an insertion-order `params` array (matching the SML
      `atlas-scripts-sml/ParamHash.sml` structure),
    - connect the stateful model to a pure `param set` abstraction.
*)

Theory ParamHashProg
Libs
  preamble ml_monad_translator_interfaceLib
Ancestors
  arithmetic list combin pair ml_monad_translator

val _ = set_up_monadic_translator ();

Datatype:
  ph_state = <|
    bucket_count : num;
    count : num;
    elems : num list;
    buckets : ((num # num) list) list
  |>
End

Datatype:
  state_exn = Fail string | Subscript
End

val config =
  global_state_config |>
  with_state ``:ph_state`` |>
  with_exception ``:state_exn`` |>
  with_refs [("bucket_count", ``0n:num``),
             ("count", ``0n:num``),
             ("elems", ``[]:num list``)] |>
  with_resizeable_arrays
    [("buckets", ``[] : ((num # num) list) list``,
      ``Subscript``, ``Subscript``)];

Overload failwith = ``raise_Fail``

val _ = start_translation config;

(* --- Pure helpers --- *)

Definition bucket_index_def:
  bucket_index (p:num) m = p MOD m
End
val bucket_index_v_thm = translate bucket_index_def;

Definition find_in_bucket_def:
  (find_in_bucket (p:num) ([]:(num # num) list) = (NONE:num option)) ∧
  (find_in_bucket p ((q,idx)::rest) =
     if p = q then SOME idx else find_in_bucket p rest)
End
val find_in_bucket_v_thm = translate find_in_bucket_def;

Definition nthn_def:
  (nthn 0n (x::xs) = x) ∧
  (nthn (SUC n) (x::xs) = nthn n xs) ∧
  (nthn _ [] = 0n)
End
val nthn_v_thm = translate nthn_def;

(* --- Monadic operations --- *)

Definition ph_create_def:
  ph_create m =
    if m = 0n then failwith "ParamHash.create: bucketCount must be positive" else
    do
      () <- set_bucket_count m;
      () <- set_count 0n;
      () <- set_elems ([]:num list);
      alloc_buckets m ([]:(num # num) list)
    od
End
val ph_create_v_thm = m_translate ph_create_def;

Definition ph_list_def:
  ph_list u =
  do
    ps <- get_elems;
    return ps
  od
End
val ph_list_v_thm = m_translate ph_list_def;

Definition ph_index_def:
  ph_index (j:num) =
  do
    ps <- get_elems;
    return (nthn j ps)
  od
End
val ph_index_v_thm = m_translate ph_index_def;

Definition ph_lookup_def:
  ph_lookup (p:num) =
  do
    m <- get_bucket_count;
    if m = 0n then return (NONE:num option) else
    do
      b <- buckets_sub (bucket_index p m);
      return (find_in_bucket p b)
    od
  od
End
val ph_lookup_v_thm = m_translate ph_lookup_def;

Definition ph_contains_def:
  ph_contains p =
  do
    r <- ph_lookup p;
    return (case r of NONE => F | SOME _ => T)
  od
End
val ph_contains_v_thm = m_translate ph_contains_def;

Definition ph_match_def:
  ph_match p =
  do
    r <- ph_lookup p;
    case r of
      SOME idx => return idx
    | NONE =>
        do
          m <- get_bucket_count;
          if m = 0n then failwith "ParamHash.match: uninitialised" else
          do
            j <- get_count;
            ps <- get_elems;
            b <- buckets_sub (bucket_index p m);
            () <- update_buckets (bucket_index p m) ((p,j)::b);
            () <- set_count (j + 1n);
            () <- set_elems (ps ++ [p]);
            return j
          od
        od
  od
End
val ph_match_v_thm = m_translate ph_match_def;

Definition ph_insert_all_def:
  (ph_insert_all [] = return ()) ∧
  (ph_insert_all (p::ps) =
     do
       () <- ph_match p;
       ph_insert_all ps
     od)
End
val ph_insert_all_v_thm = m_translate ph_insert_all_def;

Definition ph_all_present_def:
  (ph_all_present [] = return T) ∧
  (ph_all_present (p::ps) =
     do
       b <- ph_contains p;
       if b then ph_all_present ps else return F
     od)
End
val ph_all_present_v_thm = m_translate ph_all_present_def;

(* ------------------------------------------------------------------------- *)
(* Pure state model + invariants (used for staged proofs in VERIFY_ESTIMATE). *)
(* ------------------------------------------------------------------------- *)

Definition ph_set_def:
  ph_set (s:ph_state) = set (s.elems)
End

Definition ph_ok_def:
  ph_ok (s:ph_state) =
    (s.bucket_count ≠ 0n) ∧
    (LENGTH (s.buckets) = s.bucket_count) ∧
    (s.count = LENGTH (s.elems)) ∧
    (∀p idx.
       MEM (p,idx) (FLAT (s.buckets)) ⇒
         (idx < LENGTH (s.elems)) ∧
         (nthn idx (s.elems) = p))
End

Definition ph_lookup_state_def:
  ph_lookup_state (p:num) (s:ph_state) =
    if s.bucket_count = 0n then (NONE:num option)
    else
      let i = bucket_index p (s.bucket_count) in
        find_in_bucket p (EL i (s.buckets))
End

Definition ph_all_present_state_def:
  (ph_all_present_state ([]:num list) (s:ph_state) = T) ∧
  (ph_all_present_state (p::ps) s =
     case ph_lookup_state p s of
       NONE => F
     | SOME _ => ph_all_present_state ps s)
End

Definition ph_match_state_def:
  ph_match_state (p:num) (s:ph_state) =
    case ph_lookup_state p s of
      SOME idx => (idx,s)
    | NONE =>
        let m = s.bucket_count in
        let i = bucket_index p m in
        let j = s.count in
        let b = EL i (s.buckets) in
        let bs' = LUPDATE ((p,j)::b) i (s.buckets) in
        let ps' = (s.elems) ++ [p] in
          (j, s with <| count := j + 1n; elems := ps'; buckets := bs' |>)
End

Theorem bucket_index_lt:
  ∀p m. m ≠ 0n ⇒ bucket_index p m < m
Proof
  rw[bucket_index_def] \\ fs[MOD_LESS]
QED

Theorem find_in_bucket_NONE_iff:
  ∀p b. (find_in_bucket p b = NONE) ⇔ ¬(∃idx. MEM (p,idx) b)
Proof
  Induct_on `b`
  \\ rw[find_in_bucket_def]
  \\ Cases_on `h`
  \\ rw[find_in_bucket_def]
  \\ Cases_on `p = q`
  >- (fs[] \\ qexists_tac `r` \\ simp[])
  \\ fs[]
QED

Theorem find_in_bucket_SOME_MEM:
  ∀p b idx. (find_in_bucket p b = SOME idx) ⇒ MEM (p,idx) b
Proof
  Induct_on `b`
  \\ simp[find_in_bucket_def]
  \\ Cases_on `h`
  \\ simp[find_in_bucket_def]
  \\ Cases_on `p = q`
  \\ simp[]
QED

Theorem find_in_bucket_MEM_imp_SOME:
  ∀p b idx. MEM (p,idx) b ⇒ ∃idx'. find_in_bucket p b = SOME idx'
Proof
  rw[]
  \\ Cases_on `find_in_bucket p b`
  >- (fs[find_in_bucket_NONE_iff] \\ metis_tac[])
  \\ qexists_tac `x` \\ simp[]
QED

Theorem ph_lookup_state_SOME_imp_mem_flat:
  ∀p s idx.
    ph_ok s ∧ (ph_lookup_state p s = SOME idx) ⇒
      MEM (p,idx) (FLAT (s.buckets))
Proof
  rw[ph_ok_def,ph_lookup_state_def]
  \\ qabbrev_tac `i = bucket_index p (s.bucket_count)`
  \\ `i < LENGTH (s.buckets)` by
    (fs[Abbr`i`] \\ metis_tac[bucket_index_lt])
  \\ `MEM (p,idx) (EL i (s.buckets))` by
    (fs[Abbr`i`] \\ metis_tac[find_in_bucket_SOME_MEM])
  \\ fs[MEM_FLAT]
  \\ qexists_tac `EL i (s.buckets)`
  \\ conj_tac
  >- (match_mp_tac EL_MEM \\ fs[])
  \\ fs[]
QED

Theorem nth_lt_imp_MEM:
  ∀xs n. n < LENGTH xs ⇒ MEM (nthn n xs) xs
Proof
  Induct \\ Cases_on `n` \\ rw[nthn_def]
QED

Theorem ph_lookup_state_SOME_imp_in_set:
  ∀p s idx.
    ph_ok s ∧ (ph_lookup_state p s = SOME idx) ⇒ p ∈ ph_set s
Proof
  rw[ph_set_def] \\
  `MEM (p,idx) (FLAT (s.buckets))` by
    metis_tac[ph_lookup_state_SOME_imp_mem_flat]
  \\ fs[ph_ok_def]
  \\ qpat_x_assum `MEM (p,idx) (FLAT (s.buckets))`
      (fn memth =>
        qpat_x_assum `∀p idx. MEM (p,idx) (FLAT (s.buckets)) ⇒ _`
          (fn impth => mp_tac (MP (SPECL [``p:num``, ``idx:num``] impth) memth)))
  \\ strip_tac
  \\ `MEM (nthn idx (s.elems)) (s.elems)` by metis_tac[nth_lt_imp_MEM]
  \\ metis_tac[]
QED

Theorem ph_all_present_state_sound:
  ∀ps s.
    ph_ok s ∧ ph_all_present_state ps s ⇒
      ∀p. MEM p ps ⇒ p ∈ ph_set s
Proof
  Induct
  \\ rw[ph_all_present_state_def]
  \\ Cases_on `ph_lookup_state h s`
  \\ fs[ph_all_present_state_def]
  \\ metis_tac[ph_lookup_state_SOME_imp_in_set]
QED

Definition ph_bucketed_def:
  ph_bucketed (s:ph_state) =
    ∀i p idx.
      (i < LENGTH (s.buckets) ∧ MEM (p,idx) (EL i (s.buckets))) ⇒
        (bucket_index p (s.bucket_count) = i)
End

Definition ph_covered_def:
  ph_covered (s:ph_state) =
    ∀j. j < LENGTH (s.elems) ⇒ MEM (nthn j (s.elems), j) (FLAT (s.buckets))
End

Definition ph_invariant_def:
  ph_invariant (s:ph_state) = ph_ok s ∧ ph_bucketed s ∧ ph_covered s
End

Theorem MEM_imp_exists_nth:
  ∀(x:num) (xs:num list). MEM x xs ⇒ ∃n. (n < LENGTH xs) ∧ (nthn n xs = x)
Proof
  Induct_on `xs` \\ rw[]
  >- (qexists_tac `0n` \\ simp[nthn_def])
  \\ first_x_assum (drule) \\ strip_tac
  \\ qexists_tac `SUC n` \\ simp[nthn_def]
QED

Theorem ph_lookup_state_MEM_elems_imp_SOME:
  ∀p s. ph_invariant s ∧ MEM p s.elems ⇒ ∃idx. ph_lookup_state p s = SOME idx
Proof
  (* TODO (Stage B in VERIFY_ESTIMATE): complete this proof.

     Intended argument:
       - from `MEM p s.elems`, use `MEM_imp_exists_nth` to pick `j` with
         `j < LENGTH s.elems` and `nthn j s.elems = p`;
       - from `ph_covered s`, derive `MEM (p,j) (FLAT s.buckets)`;
       - pick the bucket index `k` with `(p,j) ∈ EL k s.buckets`;
       - from `ph_bucketed s`, conclude `bucket_index p s.bucket_count = k`;
       - use `find_in_bucket_MEM_imp_SOME` to get some `idx` with
         `find_in_bucket p (EL k s.buckets) = SOME idx`;
       - unfold `ph_lookup_state` and rewrite by the bucket-index equality. *)
  cheat
QED

Definition init_ph_state_def:
  init_ph_state =
    <| bucket_count := ref_init_bucket_count
     ; count := ref_init_count
     ; elems := ref_init_elems
     ; buckets := rarray_init_buckets |>
End
