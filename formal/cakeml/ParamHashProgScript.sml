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
             ("count", ``0n:num``)] |>
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

(* --- Monadic operations --- *)

Definition ph_create_def:
  ph_create m =
    if m = 0n then failwith "ParamHash.create: bucketCount must be positive" else
    do
      () <- set_bucket_count m;
      () <- set_count 0n;
      alloc_buckets m ([]:(num # num) list)
    od
End
val ph_create_v_thm = m_translate ph_create_def;

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
            b <- buckets_sub (bucket_index p m);
            () <- update_buckets (bucket_index p m) ((p,j)::b);
            () <- set_count (j + 1n);
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

Definition init_ph_state_def:
  init_ph_state =
    <| bucket_count := ref_init_bucket_count
     ; count := ref_init_count
     ; buckets := rarray_init_buckets |>
End
