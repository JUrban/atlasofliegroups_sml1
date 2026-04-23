use "atlas-scripts-sml/Gaussian_elim_Jeff.sml";

(* Smoke test for `Gaussian_elim_Jeff`: solve identity matrix. *)

structure T = struct
  open Gaussian_elim_Jeff

  fun rat (n: int) : rat = BigRat.fromInt n

  val id2 : ratmat =
    [ [rat 1, rat 0]
    , [rat 0, rat 1]
    ]

  val y : ratvec = [rat 7, rat (~3)]

  val x = solve (id2, y)

  val () =
    if ListPair.allEq (fn (a, b) => BigRat.equal (a, b)) (x, y) then
      ()
    else
      raise Fail "Gaussian_elim_Jeff smoke failed"
end;

