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

