use "atlas-scripts-sml/basic.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/misc.sml

  Purpose
  - Partial SML translation of `atlas-scripts/misc.at`.
  - The original `.at` file is a grab bag of utilities. This port starts with
    the parts that are broadly useful as dependencies for other scripts:
      - Cartesian-product enumerators (`box`, `all_words`)
      - bit / subset helpers (`to_binary`, `generate_all_subsets`)
      - list cleanup helpers (`delete_trailing_zeros`, `delete_leading_zeros`)

  Scope / limitations
  - This is intentionally incremental; many representation-theoretic helpers in
    `misc.at` depend on other large `.at` modules and are not yet ported here.
*)

structure Misc = struct
  type vec = int list
  type ratvec = Lattice.ratvec

  (* `all_words(heights)` = Cartesian product [0..heights[0]-1]×... *)
  fun all_words (heights: int list) : int list list =
    let
      fun loop [] = [[]]
        | loop (h :: hs) =
            if h < 0 then raise Fail "Misc.all_words: negative height"
            else
              let
                val tails = loop hs
              in
                List.concat (List.tabulate (h, fn i => List.map (fn t => i :: t) tails))
              end
    in
      loop heights
    end

  (* `box(height,rank)` from `misc.at`: all vectors of length `rank` with entries < height. *)
  fun box (height: int, rank: int) : vec list =
    if rank < 0 then raise Fail "Misc.box: negative rank"
    else all_words (List.tabulate (rank, fn _ => height))

  (* `box(heights)` from `misc.at`. *)
  fun box_heights (heights: int list) : vec list = all_words heights

  (* Flatten a list of lists. *)
  fun flatten (xss: 'a list list) : 'a list = List.concat xss

  (* Convert a nonnegative integer to a binary vector of fixed length (LSB at end). *)
  fun to_binary (length: int, n: int) : vec =
    if length < 0 then raise Fail "Misc.to_binary: negative length"
    else if n < 0 then raise Fail "Misc.to_binary: negative n"
    else
      let
        val v = Array.array (length, 0)
        fun loop (k, m) =
          if k = 0 orelse m = 0 then ()
          else
            let
              val q = m div 2
              val r = m mod 2
              val idx = k - 1
            in
              Array.update (v, idx, r);
              loop (k - 1, q)
            end
        val () = loop (length, n)
      in
        Array.foldr (op ::) [] v
      end

  (* Generate all subsets of a list (power set). *)
  val generate_all_subsets = Basic.power_set

  fun is_proper_subset (a: int list, b: int list) : bool =
    let
      fun member x xs = List.exists (fn y => y = x) xs
      val sub = List.all (fn x => member x b) a
      val strict = not (List.all (fn x => member x a) b)
    in
      sub andalso strict
    end

  (* Delete trailing zeros from an int list. *)
  fun delete_trailing_zeros (xs: int list) : int list =
    let
      fun dropWhile0 ys =
        (case ys of
           [] => []
         | 0 :: rest => dropWhile0 rest
         | _ => ys)
    in
      List.rev (dropWhile0 (List.rev xs))
    end

  (* Delete leading zeros from an int list. *)
  fun delete_leading_zeros (xs: int list) : int list =
    let
      fun dropWhile0 ys =
        (case ys of
           [] => []
         | 0 :: rest => dropWhile0 rest
         | _ => ys)
    in
      dropWhile0 xs
    end
end

