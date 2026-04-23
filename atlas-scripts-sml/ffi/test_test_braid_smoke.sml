use "atlas-scripts-sml/test_braid.sml";

(*
  File: atlas-scripts-sml/ffi/test_test_braid_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/test_braid.sml`.
  - Build simple-reflection matrices for `A2` in the simple-root basis and
    check braid relations.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

fun simple_reflection_matrices (rd: RootDatum.t) : IntMatrix.mat list =
  let
    val c = RootDatum.cartanMatrix rd
    val n = RootDatum.semisimpleRank rd

    (* We need a_{ij} = <alpha_i^vee, alpha_j>. `RootDatum.cartanMatrix` stores
       dot(root_i, coroot_j) = a_{ji}`, so take transpose. *)
    fun aij (i: int, j: int) : int = List.nth (List.nth (c, j), i)

    fun refl i : IntMatrix.mat =
      let
        fun entry (r: int, col: int) : int =
          if r = col andalso col = i then
            ~1
          else if r = col then
            1
          else if r = i then
            ~ (aij (i, col))
          else
            0
        fun row r = List.tabulate (n, fn col => entry (r, col))
      in
        List.tabulate (n, row)
      end
  in
    List.tabulate (n, refl)
  end

val rd = RootDatum.newSimple (#"A", 2, false);
val ops = simple_reflection_matrices rd;
val (ok, _) = Test_braid.test_braid (rd, ops);
val () = assert "A2 braid relations" ok;
val () = RootDatum.free rd;

