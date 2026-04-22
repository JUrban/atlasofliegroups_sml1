use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  File: atlas-scripts-sml/BigUnitaryHash.sml

  Purpose
  - Simple hash-set for Atlas parameters used by the F4 verification pipeline.
  - Kept for parity with the `.at` scripts’ `big_unitary_hash` usage; newer code
    often prefers `ParamHash` because it provides stable indices and cloning.

  Ownership
  - This structure stores parameter handles *as given* (no cloning). The caller
    is responsible for ensuring inserted handles remain valid.
  - Call `freeAll` to free all stored handles (typical usage: the hash “owns”
    what you insert).
*)
structure BigUnitaryHash = struct
  type param = AtlasFFI.param

  type t =
    { buckets: param list array ref
    , count: int ref
    }

  (* Create a new hash-set with `bucketCount` buckets. *)
  fun create bucketCount : t =
    if bucketCount <= 0 then raise Fail "BigUnitaryHash.create: bucketCount must be positive"
    else {buckets = ref (Array.array (bucketCount, [])), count = ref 0}

  (* Number of stored parameters. *)
  fun size ({count, ...}: t) = !count

  (* Clear buckets and reset count without freeing handles. *)
  fun clear ({buckets, count}: t) =
    (buckets := Array.array (Array.length (!buckets), []);
     count := 0)

  (* Bucket membership test using `atlas_param_equal`. *)
  fun memberInBucket (p: param) (ps: param list) =
    List.exists (fn q => AtlasFFI.atlas_param_equal (p, q) = 1) ps

  (* Membership test in the set. *)
  fun contains ({buckets, ...}: t) (p: param) =
    let
      val bs = !buckets
      val m = Array.length bs
      val h = AtlasFFI.atlas_param_hash (p, m)
      val idx =
        if h < 0 then raise Fail ("BigUnitaryHash.contains: hash failed: " ^ AtlasFFI.atlas_last_error ())
        else h
    in
      memberInBucket p (Array.sub (bs, idx))
    end

  (* Insert `p` if not already present; returns true iff inserted. *)
  fun insert ({buckets, count}: t) (p: param) =
    let
      val bs = !buckets
      val m = Array.length bs
      val h = AtlasFFI.atlas_param_hash (p, m)
      val idx =
        if h < 0 then raise Fail ("BigUnitaryHash.insert: hash failed: " ^ AtlasFFI.atlas_last_error ())
        else h
      val bucket = Array.sub (bs, idx)
    in
      if memberInBucket p bucket
      then false
      else (Array.update (bs, idx, p :: bucket); count := !count + 1; true)
    end

  (* Return all stored parameters as a list (bucket order). *)
  fun list ({buckets, ...}: t) =
    Array.foldl (fn (bucket, acc) => bucket @ acc) [] (!buckets)

  (* Free all stored parameter handles and reset the set. *)
  fun freeAll (t: t) =
    let
      val ps = list t
      val () = List.app AtlasFFI.atlas_param_free ps
    in
      clear t
    end
end
