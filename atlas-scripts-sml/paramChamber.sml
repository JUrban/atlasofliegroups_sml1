use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/tabulate.sml";

(*
  File: atlas-scripts-sml/paramChamber.sml

  Purpose
  - Partial SML translation of `atlas-scripts/paramChamber.at`.
  - Groups a list of parameters by:
      - KGB index `x(p)`
      - `lambda(p)`
      - the “affine chamber vector” of `gamma(p)`:
          chamber(p)[i] = <poscoroot_i, gamma(p)> \ 1
      - `nu(p)`
    and reports multiplicities when multiple parameters share the same triple.

  Notes on semantics
  - The `.at` operator `r \ 1` is used throughout Atlas scripts to coerce a
    rational number/vector to an integral one (typically by floor division).
    Here we implement `rat \ 1` as integer floor `num div den` (with `den>0`).
    For the intended uses in `paramChamber.at`, the pairings are expected to be
    integral in practice.

  Status
  - The table-building and printing logic is implemented.
  - This file does not attempt to re-create Atlas’ rich `KGBElt` object type;
    we key only by the integer KGB index `x`.

  Ownership
  - This module does not allocate or free `Param` handles; it only inspects
    them via the FFI.
  - It allocates a temporary `RootDatum` handle per `chamber(p)` call and frees
    it internally.
*)

structure ParamChamber = struct
  type param = AtlasFFI.param
  type group = AtlasFFI.group
  type ratvec = Lattice.ratvec
  type chamber = int list

  (* A nested associative structure mirroring the `.at` types:
       ChamberValue = [ (lambda, [ (chamber, [nu]) ]) ]
       paramChamberTable = [ (x, ChamberValue) ] *)
  type chamber_list = (chamber * ratvec list) list
  type chamber_value = (ratvec * chamber_list) list
  type table = (int * chamber_value) list

  fun expect (where': string, b: bool, msg: string) : unit =
    if b then () else raise Fail ("ParamChamber." ^ where' ^ ": " ^ msg)

  fun parseRatWeight (s: string) : ratvec = AllParameters.parseRatWeightText s

  fun vecToString (xs: int list) : string =
    "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"

  fun dotIntInf (xs: int list, ys: int list) : IntInf.int =
    let
      fun loop ([], [], acc) = acc
        | loop (a :: as', b :: bs', acc) =
            loop (as', bs', acc + IntInf.fromInt a * IntInf.fromInt b)
        | loop _ = raise Fail "ParamChamber.dotIntInf: length mismatch"
    in
      loop (xs, ys, 0)
    end

  fun floorRat (num: IntInf.int, den: IntInf.int) : int =
    if den <= 0 then
      raise Fail "ParamChamber.floorRat: nonpositive denominator"
    else
      let
        val q = IntInf.div (num, den)
      in
        IntInf.toInt q handle _ => raise Fail "ParamChamber.floorRat: overflow"
      end

  (* Determine chamber vector for affine Weyl group of `gamma(p)`. *)
  fun chamber (p: param) : chamber =
    let
      val g = AtlasFFI.atlas_param_group_handle p
      val () = expect ("chamber", g <> Foreign.Memory.null, "null group handle in param")
      val rd = AtlasFFI.atlas_group_rootdatum_new g
      val () = expect ("chamber", rd <> Foreign.Memory.null, "failed to get root datum from group")

      val gamma = parseRatWeight (AtlasFFI.atlas_param_gamma_text p)
      val gamma = Lattice.ratvecNormalize gamma
      val den = IntInf.fromInt (#den gamma)
      val nums = #nums gamma
      val coroots = RootDatum.posCorootsCols rd
      val entries =
        List.map
          (fn a => floorRat (dotIntInf (a, nums), den))
          coroots

      val () = RootDatum.free rd
    in
      entries
    end

  fun findIndexBy (pred: 'a -> bool) (xs: 'a list) : int =
    let
      fun loop ([], _) = ~1
        | loop (x :: rest, i) = if pred x then i else loop (rest, i + 1)
    in
      loop (xs, 0)
    end

  fun insert_nu (nus: ratvec list, nu: ratvec) : ratvec list = nu :: nus

  fun insert_chamber (chList: chamber_list, ch: chamber, nu: ratvec) : chamber_list =
    let
      val i = findIndexBy (fn (c, _) => c = ch) chList
    in
      if i < 0 then
        (ch, [nu]) :: chList
      else
        let
          val (c0, nus0) = List.nth (chList, i)
          val nus1 = insert_nu (nus0, nu)
          fun upd (j, entry) = if j = i then (c0, nus1) else entry
        in
          List.tabulate (length chList, fn j => upd (j, List.nth (chList, j)))
        end
    end

  fun insert_lambda (lamList: chamber_value, lam: ratvec, ch: chamber, nu: ratvec) : chamber_value =
    let
      val i = findIndexBy (fn (l, _) => l = lam) lamList
    in
      if i < 0 then
        (lam, [(ch, [nu])]) :: lamList
      else
        let
          val (l0, ch0) = List.nth (lamList, i)
          val ch1 = insert_chamber (ch0, ch, nu)
          fun upd (j, entry) = if j = i then (l0, ch1) else entry
        in
          List.tabulate (length lamList, fn j => upd (j, List.nth (lamList, j)))
        end
    end

  fun insert_x (t: table, x: int, lam: ratvec, ch: chamber, nu: ratvec) : table =
    let
      val i = findIndexBy (fn (y, _) => y = x) t
    in
      if i < 0 then
        (x, [(lam, [(ch, [nu])])]) :: t
      else
        let
          val (x0, lam0) = List.nth (t, i)
          val lam1 = insert_lambda (lam0, lam, ch, nu)
          fun upd (j, entry) = if j = i then (x0, lam1) else entry
        in
          List.tabulate (length t, fn j => upd (j, List.nth (t, j)))
        end
    end

  fun param_chamber_table (params: param list) : table =
    let
      fun step (p, acc) =
        let
          val x = AtlasFFI.atlas_param_x p
          val lam = Lattice.ratvecNormalize (parseRatWeight (AtlasFFI.atlas_param_lambda_text p))
          val nu = Lattice.ratvecNormalize (parseRatWeight (AtlasFFI.atlas_param_nu_text p))
          val ch = chamber p
        in
          insert_x (acc, x, lam, ch, nu)
        end
    in
      List.foldl step [] params
    end

  fun count_params (t: table) : int =
    let
      fun countNus nus = length nus
      fun countCh (chList: chamber_list) =
        List.foldl (fn ((_, nus), a) => a + countNus nus) 0 chList
      fun countLam (lamList: chamber_value) =
        List.foldl (fn ((_, chList), a) => a + countCh chList) 0 lamList
    in
      List.foldl (fn ((_, lamList), a) => a + countLam lamList) 0 t
    end

  fun count_chambers (t: table) : int =
    let
      fun countLam (lamList: chamber_value) =
        List.foldl (fn ((_, chList), a) => a + length chList) 0 lamList
    in
      List.foldl (fn ((_, lamList), a) => a + countLam lamList) 0 t
    end

  fun multiplicities (t: table) : int list =
    let
      fun multCh (chList: chamber_list, acc: int list) =
        List.foldl (fn ((_, nus), a) => if length nus > 1 then length nus :: a else a) acc chList
      fun multLam (lamList: chamber_value, acc: int list) =
        List.foldl (fn ((_, chList), a) => multCh (chList, a)) acc lamList
    in
      List.rev (List.foldl (fn ((_, lamList), a) => multLam (lamList, a)) [] t)
    end

  fun show (t: table) : unit =
    let
      val num_params = count_params t
      val num_chambers = count_chambers t
      val mults = multiplicities t
      val () = print ("number of parameters: " ^ Int.toString num_params ^ "\n")
      val () = print ("number of distinct chambers: " ^ Int.toString num_chambers ^ "\n")
      val () =
        print
          ("multiplicities: "
           ^ vecToString mults
           ^ "\n")

      fun rowFor (x: int, lam: ratvec, ch: chamber, nus: ratvec list) : string list list =
        List.map
          (fn nu =>
             [ Int.toString x
             , RootDatum.ratvecToText lam
             , vecToString ch
             , RootDatum.ratvecToText nu
             , Int.toString (length nus)
             ])
          nus

      val rows =
        List.concat
          (List.map
             (fn (x, lamList) =>
                List.concat
                  (List.map
                     (fn (lam, chList) =>
                        List.concat (List.map (fn (ch, nus) => rowFor (x, lam, ch, nus)) chList))
                     lamList))
             t)
      val data = ["x", "lambda", "chamber", "nu", "mult"] :: rows
    in
      Tabulate.tabulate (data, "lllll", 2, " ")
    end

  fun show_table (params: param list) : unit = show (param_chamber_table params)

  fun show_short (t: table) : unit =
    let
      val num_params = count_params t
      val num_chambers = count_chambers t
      val mults = multiplicities t
      val () = print ("number of parameters: " ^ Int.toString num_params ^ "\n")
      val () = print ("number of distinct chambers: " ^ Int.toString num_chambers ^ "\n")
      val () = print ("multiplicities: " ^ vecToString mults ^ "\n")
    in
      ()
    end

  fun show_short_params (params: param list) : unit =
    show_short (param_chamber_table params)
end

