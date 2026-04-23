use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KTypePol.sml";

(*
  File: atlas-scripts-sml/KTypePolHash.sml

  Purpose
  - An owning hash table for `KTypePol` handles (analogous to `ParamHash`).
  - Mirrors the `.at` `KTypePol_hash` usage pattern: insert-or-match and stable
    indexing, used to deduplicate many computed K-type character polynomials.

  Implementation
  - Uses C++ FFI helpers:
      - `atlas_ktypepol_hash_code(pol, m)` for bucketing
      - `atlas_ktypepol_equal(a, b)` for equality
  - On insertion, clones the polynomial so the table owns its stored handle.

  Ownership
  - The table owns all stored `KTypePol` handles (cloned at insertion).
  - Call `freeAll` to free all stored handles.
*)
structure KTypePolHash = struct
  type ktypepol = AtlasFFI.ktypepol

  type entry = {p: ktypepol, idx: int}

  type t =
    { buckets: entry list array ref
    , pols: ktypepol array ref
    , count: int ref
    }

  fun create bucketCount : t =
    if bucketCount <= 0 then
      raise Fail "KTypePolHash.create: bucketCount must be positive"
    else
      let
        val initialCap = Int.max (16, bucketCount)
      in
        { buckets = ref (Array.array (bucketCount, []))
        , pols = ref (Array.array (initialCap, Foreign.Memory.null))
        , count = ref 0
        }
      end

  fun size ({count, ...}: t) = !count

  fun clear ({buckets, count, ...}: t) =
    (buckets := Array.array (Array.length (!buckets), []);
     count := 0)

  fun freeAll (t: t) =
    let
      val {pols, count, ...} = t
      val a = !pols
      val n = !count
      fun loop i =
        if i = n then
          ()
        else
          (KTypePol.free (Array.sub (a, i)); loop (i + 1))
    in
      loop 0;
      clear t
    end

  fun ensureCapacity (t: t) (need: int) =
    let
      val {pols, ...} = t
      val a = !pols
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
          pols := b
        end
    end

  fun bucketIndex (bs: entry list array) (p: ktypepol) : int =
    let
      val m = Array.length bs
      val h = AtlasFFI.atlas_ktypepol_hash_code (p, m)
    in
      if h < 0 then
        raise Fail ("KTypePolHash: hash failed: " ^ AtlasFFI.atlas_last_error ())
      else
        Int.mod (h, m)
    end

  fun findInBucket (p: ktypepol) (xs: entry list) : int option =
    case xs of
      [] => NONE
    | {p = q, idx} :: rest =>
        if AtlasFFI.atlas_ktypepol_equal (p, q) = 1 then SOME idx else findInBucket p rest

  fun lookup ({buckets, ...}: t) (p: ktypepol) : int =
    let
      val bs = !buckets
      val idx = bucketIndex bs p
    in
      case findInBucket p (Array.sub (bs, idx)) of
        NONE => ~1
      | SOME j => j
    end

  fun contains (t: t) (p: ktypepol) : bool = lookup t p >= 0

  fun index ({pols, count, ...}: t) (j: int) : ktypepol =
    if j < 0 orelse j >= !count then raise Subscript else Array.sub (!pols, j)

  fun list (t: t) : ktypepol list =
    let
      val {pols, count, ...} = t
      val a = !pols
      val n = !count
      fun loop (i, acc) = if i < 0 then acc else loop (i - 1, Array.sub (a, i) :: acc)
    in
      loop (n - 1, [])
    end

  fun match (t: t) (p: ktypepol) : int =
    let
      val {buckets, pols, count, ...} = t
      val bs = !buckets
      val bidx = bucketIndex bs p
      val bucket = Array.sub (bs, bidx)
    in
      case findInBucket p bucket of
        SOME j => j
      | NONE =>
          let
            val p2 = AtlasFFI.atlas_ktypepol_clone p
            val () =
              if p2 = Foreign.Memory.null then
                raise Fail ("KTypePolHash.match: clone failed: " ^ AtlasFFI.atlas_last_error ())
              else
                ()
            val j = !count
            val () = ensureCapacity t (j + 1)
            val () = Array.update (!pols, j, p2)
            val () = count := j + 1
            val () = Array.update (bs, bidx, {p = p2, idx = j} :: bucket)
          in
            j
          end
    end
end
