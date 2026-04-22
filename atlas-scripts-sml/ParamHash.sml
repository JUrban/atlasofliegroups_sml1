use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure ParamHash = struct
  type param = AtlasFFI.param

  type entry = {p: param, idx: int}

  type t =
    { buckets: entry list array ref
    , params: param array ref
    , count: int ref
    }

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

  fun size ({count, ...}: t) = !count

  fun clear ({buckets, count, ...}: t) =
    (buckets := Array.array (Array.length (!buckets), []);
     count := 0)

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

  fun findInBucket (p: param) (xs: entry list) : int option =
    case xs of
      [] => NONE
    | {p = q, idx} :: rest =>
        if AtlasFFI.atlas_param_equal (p, q) = 1 then SOME idx else findInBucket p rest

  fun bucketIndex (bs: entry list array) (p: param) : int =
    let
      val m = Array.length bs
      val h = AtlasFFI.atlas_param_hash (p, m)
    in
      if h < 0 then raise Fail ("ParamHash: hash failed: " ^ AtlasFFI.atlas_last_error ()) else h
    end

  fun lookup ({buckets, ...}: t) (p: param) : int =
    let
      val bs = !buckets
      val idx = bucketIndex bs p
    in
      case findInBucket p (Array.sub (bs, idx)) of
        NONE => ~1
      | SOME j => j
    end

  fun contains (t: t) (p: param) : bool =
    lookup t p >= 0

  fun index ({params, count, ...}: t) (j: int) : param =
    if j < 0 orelse j >= !count then
      raise Subscript
    else
      Array.sub (!params, j)

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
