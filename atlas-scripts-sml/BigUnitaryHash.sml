use "atlas-scripts-sml/ffi/AtlasFFI.sml";

structure BigUnitaryHash = struct
  type param = AtlasFFI.param

  type t =
    { buckets: param list array ref
    , count: int ref
    }

  fun create bucketCount : t =
    if bucketCount <= 0 then raise Fail "BigUnitaryHash.create: bucketCount must be positive"
    else {buckets = ref (Array.array (bucketCount, [])), count = ref 0}

  fun size ({count, ...}: t) = !count

  fun clear ({buckets, count}: t) =
    (buckets := Array.array (Array.length (!buckets), []);
     count := 0)

  fun memberInBucket (p: param) (ps: param list) =
    List.exists (fn q => AtlasFFI.atlas_param_equal (p, q) = 1) ps

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

  fun list ({buckets, ...}: t) =
    Array.foldl (fn (bucket, acc) => bucket @ acc) [] (!buckets)

  fun freeAll (t: t) =
    let
      val ps = list t
      val () = List.app AtlasFFI.atlas_param_free ps
    in
      clear t
    end
end
