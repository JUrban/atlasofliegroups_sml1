use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";

(*
  File: atlas-scripts-sml/KTypeHash.sml

  Purpose
  - An owning hash table for `KType` handles (analogous to `ParamHash` and
    `KTypePolHash`).
  - Intended as a building block for ports of `.at` code that relies on
    `KType_hash` tables (e.g. face/graph infrastructure).

  Implementation
  - Uses C++ FFI helpers:
      - `atlas_ktype_hash_code(t, m)` for bucketing
      - `atlas_ktype_equal(a, b)` for equality
  - On insertion, clones the K-type so the table owns its stored handle.

  Ownership
  - The table owns all stored `KType` handles (cloned at insertion).
  - Call `freeAll` to free all stored handles.
*)
structure KTypeHash = struct
  type ktype = AtlasFFI.ktype

  type entry = {t: ktype, idx: int}

  type t =
    { buckets: entry list array ref
    , ks: ktype array ref
    , count: int ref
    }

  fun create bucketCount : t =
    if bucketCount <= 0 then
      raise Fail "KTypeHash.create: bucketCount must be positive"
    else
      let
        val initialCap = Int.max (16, bucketCount)
      in
        { buckets = ref (Array.array (bucketCount, []))
        , ks = ref (Array.array (initialCap, Foreign.Memory.null))
        , count = ref 0
        }
      end

  fun size ({count, ...}: t) = !count

  fun clear ({buckets, count, ...}: t) =
    (buckets := Array.array (Array.length (!buckets), []);
     count := 0)

  fun freeAll (t: t) =
    let
      val {ks, count, ...} = t
      val a = !ks
      val n = !count
      fun loop i =
        if i = n then
          ()
        else
          (KType.free (Array.sub (a, i)); loop (i + 1))
    in
      loop 0;
      clear t
    end

  fun ensureCapacity (t: t) (need: int) =
    let
      val {ks, ...} = t
      val a = !ks
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
          ks := b
        end
    end

  fun bucketIndex (bs: entry list array) (t: ktype) : int =
    let
      val m = Array.length bs
      val h = AtlasFFI.atlas_ktype_hash_code (t, m)
    in
      if h < 0 then
        raise Fail ("KTypeHash: hash failed: " ^ AtlasFFI.atlas_last_error ())
      else
        Int.mod (h, m)
    end

  fun findInBucket (t: ktype) (xs: entry list) : int option =
    case xs of
      [] => NONE
    | {t = u, idx} :: rest =>
        if AtlasFFI.atlas_ktype_equal (t, u) = 1 then SOME idx else findInBucket t rest

  fun lookup ({buckets, ...}: t) (k: ktype) : int =
    let
      val bs = !buckets
      val idx = bucketIndex bs k
    in
      case findInBucket k (Array.sub (bs, idx)) of
        NONE => ~1
      | SOME j => j
    end

  fun contains (t: t) (k: ktype) : bool = lookup t k >= 0

  fun index ({ks, count, ...}: t) (j: int) : ktype =
    if j < 0 orelse j >= !count then raise Subscript else Array.sub (!ks, j)

  fun list (t: t) : ktype list =
    let
      val {ks, count, ...} = t
      val a = !ks
      val n = !count
      fun loop (i, acc) = if i < 0 then acc else loop (i - 1, Array.sub (a, i) :: acc)
    in
      loop (n - 1, [])
    end

  fun match (t: t) (k: ktype) : int =
    let
      val {buckets, ks, count, ...} = t
      val bs = !buckets
      val bidx = bucketIndex bs k
      val bucket = Array.sub (bs, bidx)
    in
      case findInBucket k bucket of
        SOME j => j
      | NONE =>
          let
            val k2 = AtlasFFI.atlas_ktype_clone k
            val () =
              if k2 = Foreign.Memory.null then
                raise Fail ("KTypeHash.match: clone failed: " ^ AtlasFFI.atlas_last_error ())
              else
                ()
            val j = !count
            val () = ensureCapacity t (j + 1)
            val () = Array.update (!ks, j, k2)
            val () = count := j + 1
            val () = Array.update (bs, bidx, {t = k2, idx = j} :: bucket)
          in
            j
          end
    end
end

