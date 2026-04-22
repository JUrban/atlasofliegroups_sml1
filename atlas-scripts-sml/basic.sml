use "atlas-scripts-sml/MatrixAT.sml";

(* Minimal SML analogue of `atlas-scripts/basic.at` (partial).
   Provide core constructors used throughout `.at` scripts. *)
structure Basic = struct
  type mat = MatrixAT.mat

  val null = MatrixAT.null
  val id_mat = MatrixAT.id_mat
  val block_matrix = MatrixAT.block_matrix

  fun vector (n: int, f: int -> 'a) : 'a list =
    if n < 0 then raise Fail "Basic.vector: negative length" else List.tabulate (n, f)

  fun matrix ((nRows: int, nCols: int), f: int * int -> int) : mat =
    if nRows < 0 orelse nCols < 0 then
      raise Fail "Basic.matrix: negative shape"
    else
      List.tabulate (nRows, fn i => List.tabulate (nCols, fn j => f (i, j)))
end

