use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AtlasParam.sml";

(*
  File: atlas-scripts-sml/ParamHash.sml

  Purpose
  - A lightweight “set with stable indexing” for Atlas parameters, used to
    deduplicate large computed collections (e.g. F4 FPP point sets).

  Design
  - Parameters are bucketed using the Atlas-provided hash function
    `atlas_param_hash(p, m)` and compared using `atlas_param_equal`.
  - `match` inserts a clone of the parameter and returns its assigned index;
    repeated `match` on an equal parameter returns the existing index.

  Ownership
  - The hash table owns the stored parameter handles (cloned at insertion).
  - Call `freeAll` when done to free all stored handles.
  - Callers should still free any temporary parameters they create; `match`
    does not consume its input handle.
*)
structure ParamHash = struct
  type param = AtlasParam.t

  type entry = {p: param, idx: int}

  type t =
    { buckets: entry list array ref
    , params: param array ref
    , count: int ref
    }

  (* Create a new table with `bucketCount` buckets (must be positive). *)
  fun create bucketCount : t =
    if bucketCount <= 0 then raise Fail "ParamHash.create: bucketCount must be positive"
    else
      let
        val initialCap = Int.max (16, bucketCount)
      in
        { buckets = ref (Array.array (bucketCount, []))
        , params = ref (Array.array (initialCap, Foreign.Memory.null))
        , count = ref 0
        }
      end

  (* Number of stored parameters. *)
  fun size ({count, ...}: t) = !count

  (* Clear buckets and reset count, but do not free stored parameters. *)
  fun clear ({buckets, count, ...}: t) =
    (buckets := Array.array (Array.length (!buckets), []);
     count := 0)

  (* Free all stored parameters and reset the table to empty. *)
  fun freeAll (t: t) =
    let
      val {params, count, ...} = t
      val a = !params
      val n = !count
      fun loop i =
        if i = n then
          ()
        else
          (AtlasFFI.atlas_param_free (Array.sub (a, i)); loop (i + 1))
    in
      loop 0;
      clear t
    end

  (* Ensure backing array capacity for at least `need` stored parameters. *)
  fun ensureCapacity (t: t) (need: int) =
    let
      val {params, ...} = t
      val a = !params
      val cap = Array.length a
    in
      if need <= cap then
        ()
      else
        let
          val newCap = Int.max (need, cap * 2)
          val b = Array.array (newCap, Foreign.Memory.null)
          fun copy i =
            if i = cap then
              ()
            else
              (Array.update (b, i, Array.sub (a, i)); copy (i + 1))
        in
          copy 0;
          params := b
        end
    end

  (* Scan a bucket to find an equal parameter, returning its index if present. *)
  fun findInBucket (p: param) (xs: entry list) : int option =
    case xs of
      [] => NONE
    | {p = q, idx} :: rest =>
        if AtlasParam.eq (p, q) then SOME idx else findInBucket p rest

  (* Compute the bucket index for `p` using Atlas' hash function. *)
  fun bucketIndex (bs: entry list array) (p: param) : int =
    AtlasParam.hash_mod (p, Array.length bs)

  (* Lookup the index of `p`, or `~1` if absent. *)
  fun lookup ({buckets, ...}: t) (p: param) : int =
    let
      val bs = !buckets
      val idx = bucketIndex bs p
    in
      case findInBucket p (Array.sub (bs, idx)) of
        NONE => ~1
      | SOME j => j
    end

  (* Membership test. *)
  fun contains (t: t) (p: param) : bool =
    lookup t p >= 0

  (* Fetch the stored parameter handle by index. *)
  fun index ({params, count, ...}: t) (j: int) : param =
    if j < 0 orelse j >= !count then
      raise Subscript
    else
      Array.sub (!params, j)

  (* Return the stored parameters as a list in insertion order. *)
  fun list (t: t) : param list =
    let
      val {params, count, ...} = t
      val a = !params
      val n = !count
      fun loop (i, acc) =
        if i < 0 then acc else loop (i - 1, Array.sub (a, i) :: acc)
    in
      loop (n - 1, [])
    end

  (* Insert-or-match: returns the index of an equal stored parameter.
     On insertion, clones `p` so that the table owns the stored handle. *)
  fun match (t: t) (p: param) : int =
    let
      val {buckets, params, count, ...} = t
      val bs = !buckets
      val bidx = bucketIndex bs p
      val bucket = Array.sub (bs, bidx)
    in
      case findInBucket p bucket of
        SOME j => j
      | NONE =>
          let
            val p2 = AtlasFFI.atlas_param_clone p
            val () =
              if p2 = Foreign.Memory.null then
                raise Fail ("ParamHash.match: clone failed: " ^ AtlasFFI.atlas_last_error ())
              else
                ()
            val j = !count
            val () = ensureCapacity t (j + 1)
            val () = Array.update (!params, j, p2)
            val () = count := j + 1
            val () = Array.update (bs, bidx, {p = p2, idx = j} :: bucket)
          in
            j
          end
    end
end
