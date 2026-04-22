use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/hash.sml

  Purpose
  - SML translation of `atlas-scripts/hash.at`.
  - Provides a generic hash table abstraction with stable indexing of inserted
    elements, plus a few hashing helpers for common Atlas-script datatypes.

  Design
  - Open addressing with linear probing, mirroring the `.at` implementation.
  - Storage:
      - `seq_nbrs`: array mapping hash slots -> sequence number (index in values)
      - `values`: dynamic array of inserted values (in insertion order)

  API
  - `Hash.hash_info`: a pair of functions `(hash_code, eq)`.
  - `Hash.make_hash`: create a table from `hash_info`.
  - Table operations match the `.at` `Hash<T>` record:
      - `match`: insert-or-find, returns index
      - `could_add`: whether insertion was new
      - `search`: lookup without insertion
      - `index`, `list`, `iterator`, `fill`, `clear`, `size`, `capacity`, `grow`
  - `Hash.exhaust`: orbit generation helper used by many `.at` scripts.
*)

structure Hash = struct
  type 'a hash_code = 'a * int -> int
  type 'a eq = 'a * 'a -> bool
  type 'a hash_info = {hash_code: 'a hash_code, eq: 'a eq}

  type 'a iterator = {peek: unit -> 'a option, advance: unit -> unit}

  type 'a t =
    { size: unit -> int
    , capacity: unit -> int
    , grow: int -> unit
    , fill: 'a list -> unit
    , clear: unit -> unit
    , list: unit -> 'a list
    , index: int -> 'a
    , match: 'a -> int
    , could_add: 'a -> bool
    , search: 'a -> int option
    , iterator: unit -> 'a iterator
    }

  val empty = ~1

  fun isPowerOfTwo (m: int) : bool =
    m > 0 andalso (Word.andb (Word.fromInt m, Word.fromInt (m - 1)) = 0w0)

  fun nextPow2 (n: int) : int =
    let
      fun loop p = if p >= n then p else loop (p * 2)
    in
      if n <= 2 then 2 else loop 2
    end

  fun clampHash (h: int, cap: int) : int =
    let
      val () = if cap > 0 then () else raise Fail "Hash: nonpositive capacity"
      val hh = if h < 0 then ~h else h
      val r = Int.mod (hh, cap)
    in
      if r < 0 then r + cap else r
    end

  (* `lift` from `hash.at`: build a hash_info on `S` from one on `T` by projection `f : S -> T`. *)
  fun lift (f: 's -> 't, info: 't hash_info) : 's hash_info =
    { hash_code = (fn (x, m) => #hash_code info (f x, m))
    , eq = (fn (a, b) => #eq info (f a, f b))
    }

  (* “Trivial” hash_info for ints; uses bit masking when modulus is power of 2. *)
  val triv_hash_info : int hash_info =
    { hash_code =
        (fn (a, m) =>
          if m <= 0 then raise Fail "Hash.triv_hash_info: nonpositive modulus"
          else if isPowerOfTwo m then
            Word.toIntX (Word.andb (Word.fromInt a, Word.fromInt (m - 1)))
          else
            clampHash (a, m))
    , eq = (op =)
    }

  (* The polynomial hashing helper `eval_at_mod` from `hash.at`.
     Requires `m` be a power of 2 and > 1. *)
  fun eval_at_mod (n: int, m: int) : int list -> int =
    let
      val () = if n > 1 andalso m > 1 andalso isPowerOfTwo m then () else raise Fail "Hash.eval_at_mod: bad args"
      val mask = Word.fromInt (m - 1)
      val base = Word.fromInt n
      fun step (e: int, sum: Word.word) : Word.word =
        Word.andb (mask, Word.fromInt e + Word.* (base, sum))
      fun run (v: int list) =
        Word.toIntX (List.foldr step 0w0 v)
    in
      run
    end

  (* Default vec hash code from `hash.at`. *)
  fun hash_code_vec (v: int list, m: int) : int =
    eval_at_mod (8647, m) v

  (* Hash a row-major int matrix by concatenating rows. *)
  fun hash_code_mat (rows: int list list, m: int) : int =
    hash_code_vec (List.concat rows, m)

  type ratvec = {den: int, nums: int list}

  fun hash_code_ratvec (rv: ratvec, m: int) : int =
    hash_code_vec (#nums rv @ [#den rv], m)

  (* Build a new hash table. This is the `make_hash` constructor from `hash.at`. *)
  fun make_hash_with (info: 'a hash_info, reserveN: int, data: 'a list) : 'a t =
    let
      val {hash_code, eq} = info
      val values : 'a option array ref = ref (Array.array (Int.max (16, reserveN), NONE))
      val count : int ref = ref 0

      val cap0 = nextPow2 ((3 * Int.max (0, reserveN)) div 2)
      val seq_nbrs : int array ref = ref (Array.array (cap0, empty))

      fun max_fill () = (2 * Array.length (!seq_nbrs)) div 3

      fun ensureValuesCapacity (need: int) : unit =
        let
          val a = !values
          val cap = Array.length a
        in
          if need <= cap then
            ()
          else
            let
              val newCap = Int.max (need, cap * 2)
              val b = Array.array (newCap, NONE)
              fun copy i =
                if i = cap then () else (Array.update (b, i, Array.sub (a, i)); copy (i + 1))
            in
              copy 0;
              values := b
            end
        end

      fun getValue i =
        (case Array.sub (!values, i) of
           SOME v => v
         | NONE => raise Fail "Hash: internal: missing value")

      fun locate (x: 'a) : int * bool =
        let
          val cap = Array.length (!seq_nbrs)
          val h0 = clampHash (hash_code (x, cap), cap)
          fun probe code =
            let
              val i = Array.sub (!seq_nbrs, code)
            in
              if i = empty then
                (code, true)
              else if eq (getValue i, x) then
                (code, false)
              else
                let
                  val code' = if code + 1 = cap then 0 else code + 1
                in
                  probe code'
                end
            end
        in
          probe h0
        end

      fun rehashTo (newCap: int) : unit =
        let
          val newCap = nextPow2 newCap
          val oldValues = !values
          val n = !count
          val newSeq = Array.array (newCap, empty)
          fun locateInNew (x: 'a) : int =
            let
              val h0 = clampHash (hash_code (x, newCap), newCap)
              fun probe code =
                let
                  val i = Array.sub (newSeq, code)
                in
                  if i = empty then code
                  else
                    let
                      val code' = if code + 1 = newCap then 0 else code + 1
                    in
                      probe code'
                    end
                end
            in
              probe h0
            end
          fun ins i =
            if i = n then
              ()
            else
              (case Array.sub (oldValues, i) of
                 NONE => raise Fail "Hash: internal: missing value during rehash"
               | SOME v =>
                   let
                     val code = locateInNew v
                   in
                     Array.update (newSeq, code, i);
                     ins (i + 1)
                   end)
        in
          ins 0;
          seq_nbrs := newSeq
        end

      fun grow (extra: int) : unit =
        let
          val extra = Int.max (0, extra)
          val need = !count + extra
          val proposed = nextPow2 ((3 * need) div 2)
          val cur = Array.length (!seq_nbrs)
        in
          if proposed > cur then rehashTo proposed else ()
        end

      fun acquire (x: 'a) : bool * int =
        let
          val (code, isNew) = locate x
          val seq = !count
        in
          if isNew then
            let
              val () = ensureValuesCapacity (seq + 1)
              val () = Array.update (!values, seq, SOME x)
              val () = count := seq + 1
              val () =
                if seq >= max_fill () then
                  (rehashTo (Array.length (!seq_nbrs) * 2);
                   (* After rehash, recompute the insertion slot. *)
                   let
                     val (code2, new2) = locate x
                     val () = if new2 then raise Fail "Hash: rehash lost element" else ()
                   in
                     Array.update (!seq_nbrs, code2, seq)
                   end)
                else
                  Array.update (!seq_nbrs, code, seq)
            in
              (true, seq)
            end
          else
            (false, Array.sub (!seq_nbrs, code))
        end

      fun size () = !count
      fun capacity () = Array.length (!seq_nbrs)

      fun clear () =
        (seq_nbrs := Array.array (2, empty);
         count := 0)

      fun list () : 'a list =
        let
          val n = !count
          fun loop (i, acc) =
            if i < 0 then acc else loop (i - 1, getValue i :: acc)
        in
          loop (n - 1, [])
        end

      fun index i =
        if i < 0 orelse i >= !count then raise Subscript else getValue i

      fun match x = #2 (acquire x)
      fun could_add x = #1 (acquire x)

      fun search x =
        let
          val (code, isNew) = locate x
        in
          if isNew then NONE else SOME (Array.sub (!seq_nbrs, code))
        end

      fun iterator () : 'a iterator =
        let
          val i = ref 0
          fun peek () =
            if !i < !count then SOME (getValue (!i)) else NONE
          fun advance () = i := !i + 1
        in
          {peek = peek, advance = advance}
        end

      fun fill xs =
        (grow (length xs); List.app (fn x => ignore (acquire x)) xs)

      val t =
        { size = size
        , capacity = capacity
        , grow = grow
        , fill = fill
        , clear = clear
        , list = list
        , index = index
        , match = match
        , could_add = could_add
        , search = search
        , iterator = iterator
        }

      val () = fill data
    in
      t
    end

  fun make_hash (info: 'a hash_info) : 'a t = make_hash_with (info, 0, [])
  fun make_hash_data (info: 'a hash_info, data: 'a list) : 'a t = make_hash_with (info, length data, data)
  fun make_hash_reserve (info: 'a hash_info, reserveN: int) : 'a t = make_hash_with (info, reserveN, [])

  (* `lookup` from `hash.at`: return index or ~1 if absent. *)
  fun lookup (h: 'a t) (x: 'a) : int =
    case #search h x of NONE => ~1 | SOME i => i

  (* Orbit/exhaust helper from `hash.at`. *)
  fun exhaust (actions: ('a -> 'a) list, h: 'a t) : 'a list =
    let
      val it = #iterator h ()
      fun loop acc =
        (case #peek it () of
           NONE => List.rev acc
         | SOME x =>
             (#advance it ();
              List.app (fn act => ignore (#match h (act x))) actions;
              loop (x :: acc)))
    in
      loop []
    end

  (* Convenience hash builders mirroring the `.at` versions for common types. *)
  fun make_vec_hash () : int list t =
    make_hash {hash_code = hash_code_vec, eq = (op =)}

  fun make_vec_hash_data (data: int list list) : int list t =
    make_hash_data ({hash_code = hash_code_vec, eq = (op =) }, data)

  fun make_mat_hash () : int list list t =
    make_hash {hash_code = hash_code_mat, eq = (op =)}

  fun make_ratvec_hash () : ratvec t =
    make_hash {hash_code = hash_code_ratvec, eq = (op =)}
end

