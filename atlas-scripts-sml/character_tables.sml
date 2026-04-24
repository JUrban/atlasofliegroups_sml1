use "atlas-scripts-sml/class_tables.sml";

(*
  File: atlas-scripts-sml/character_tables.sml

  Purpose
  - Partial SML translation of `atlas-scripts/character_tables.at`.
  - The `.at` file defines the `CharacterTable` type and a large API (tensor
    products, symmetric/exterior powers, decomposition, printing, etc.).

  Scope of this port (current)
  - This module provides a minimal, *useful* subset focused on:
      - representing a character table as integer rows over a fixed
        `WeylClassTable.t` ordering,
      - computing the standard inner product using class sizes,
      - checking orthogonality of irreducible rows,
      - basic accessors.
  - Advanced operations from the `.at` library (sym/ext powers, explicit Weyl
    elements, reordering, etc.) are not yet ported.
*)

structure CharacterTables = struct
  type char_row = int list

  structure CharacterTable = struct
    type t =
      { class_table: WeylClassTable.t
      , class_names: string list
      , irreducible_names: string list
      , table: char_row list (* rows; `table[i][j]` is value of irrep i on class j *)
      , deg_spec: (int * int) list (* (degree, special-index) per irrep *)
      }
  end

  fun expect (where', cond, msg) =
    if cond then () else raise Fail ("CharacterTables." ^ where' ^ ": " ^ msg)

  fun n_classes (ct: CharacterTable.t) : int = #n_classes (#class_table ct)
  fun n_irreps (ct: CharacterTable.t) : int = length (#table ct)

  (* Basic “free” operations on class functions, mirroring the `.at` file. *)
  fun sum_rows (v: char_row, w: char_row) : char_row =
    (ListPair.mapEq (op +) (v, w) handle _ => raise Fail "CharacterTables.sum_rows: length mismatch")

  fun tensor_product_rows (v: char_row, w: char_row) : char_row =
    (ListPair.mapEq (op * ) (v, w) handle _ => raise Fail "CharacterTables.tensor_product_rows: length mismatch")

  val tensor_rows = tensor_product_rows

  fun cartesian_power_row (x: char_row, n: int) : char_row =
    if n < 0 then raise Fail "CharacterTables.cartesian_power_row: negative n"
    else List.map (fn v => n * v) x

  fun tensor_power_row (x: char_row, power: int) : char_row =
    if power < 0 then
      raise Fail "CharacterTables.tensor_power_row: negative power"
    else
      let
        fun ipow (a: int, k: int) : int =
          if k = 0 then 1 else a * ipow (a, k - 1)
      in
        List.map (fn v => ipow (v, power)) x
      end

  fun id_class (ct: CharacterTable.t) : int =
    let
      val wct = #class_table ct
      val orders = #class_orders wct
      val sizes = #class_sizes wct
      val n = #n_classes wct
      fun loop i =
        if i = n then
          raise Fail "CharacterTables.id_class: no identity class found"
        else if List.nth (orders, i) = 1 andalso List.nth (sizes, i) = 1 then
          i
        else
          loop (i + 1)
    in
      loop 0
    end

  fun class_size (ct: CharacterTable.t, j: int) : int =
    List.nth (#class_sizes (#class_table ct), j)

  fun order_W (ct: CharacterTable.t) : int =
    List.foldl (op +) 0 (#class_sizes (#class_table ct))

  fun character (ct: CharacterTable.t, i: int) : char_row =
    List.nth (#table ct, i)

  fun class_label (ct: CharacterTable.t, j: int) : string =
    List.nth (#class_names ct, j)

  fun irreducible_label (ct: CharacterTable.t, i: int) : string =
    List.nth (#irreducible_names ct, i)

  fun degree (ct: CharacterTable.t, i: int) : int =
    #1 (List.nth (#deg_spec ct, i))

  fun special (ct: CharacterTable.t, i: int) : int =
    #2 (List.nth (#deg_spec ct, i))

  fun dimension_row (ct: CharacterTable.t, chi: char_row) : int =
    List.nth (chi, id_class ct)

  fun dimension (ct: CharacterTable.t, i: int) : int =
    dimension_row (ct, character (ct, i))

  fun inner (ct: CharacterTable.t, x: char_row, y: char_row) : int =
    let
      val n = n_classes ct
      val () = expect ("inner", length x = n andalso length y = n, "wrong character length")
      val w = order_W ct
      val s =
        List.foldl
          (op +)
          0
          (List.tabulate
             (n, fn j =>
                class_size (ct, j) * List.nth (x, j) * List.nth (y, j)))
    in
      if w = 0 then raise Fail "CharacterTables.inner: |W|=0"
      else if s mod w <> 0 then
        raise Fail "CharacterTables.inner: non-integral inner product (unexpected for irreps)"
      else
        s div w
    end

  fun scalar_product (ct: CharacterTable.t, i: int, k: int) : int =
    inner (ct, character (ct, i), character (ct, k))

  fun norm2 (ct: CharacterTable.t, x: char_row) : int = inner (ct, x, x)

  fun characters (ct: CharacterTable.t) : char_row list = #table ct

  fun character_index (ct: CharacterTable.t, chi: char_row) : int =
    let
      fun loop ([], _) = raise Fail "CharacterTables.character_index: not found"
        | loop (row :: rows, i) = if row = chi then i else loop (rows, i + 1)
    in
      loop (characters ct, 0)
    end

  (* Decompose a class function into the irreducible basis using the inner
     product: multiplicity of chi_i is <v, chi_i>. *)
  fun decompose (ct: CharacterTable.t, v: char_row) : int list =
    let
      val n = n_irreps ct
    in
      List.tabulate (n, fn i => inner (ct, v, character (ct, i)))
    end

  fun tensor (ct: CharacterTable.t, i: int, k: int) : char_row =
    tensor_rows (character (ct, i), character (ct, k))

  fun check_orthogonality (ct: CharacterTable.t) : bool =
    let
      val n = n_irreps ct
      fun okPair (i, k) =
        let
          val ip = scalar_product (ct, i, k)
        in
          if i = k then ip = 1 else ip = 0
        end
    in
      List.all okPair
        (List.concat (List.tabulate (n, fn i => List.tabulate (n, fn k => (i, k)))))
    end

  fun assert_orthogonality (ct: CharacterTable.t) : unit =
    if check_orthogonality ct then () else raise Fail "CharacterTables: orthogonality check failed"

  (* Build a `CharacterTable.t` from already-prepared data.

     `degrees` are supplied by the caller (unlike the `.at` version which
     computes them via `sym_power_refl`). *)
  fun make
    ( wct: WeylClassTable.t
    , class_names: string list
    , irreps: (char_row * string) list
    , degrees: int list
    , to_special: int -> int
    ) : CharacterTable.t =
    let
      val n = #n_classes wct
      val () = expect ("make", length class_names = n, "wrong number of class names")
      val () = expect ("make", length irreps = n, "expected square character table")
      val () = expect ("make", length degrees = n, "wrong number of degrees")
      val table = List.map #1 irreps
      val names = List.map #2 irreps
      val () = List.app (fn row => expect ("make", length row = n, "wrong character row length")) table
      val deg_spec = List.tabulate (n, fn i => (List.nth (degrees, i), to_special i))
    in
      { class_table = wct
      , class_names = class_names
      , irreducible_names = names
      , table = table
      , deg_spec = deg_spec
      }
    end
end
