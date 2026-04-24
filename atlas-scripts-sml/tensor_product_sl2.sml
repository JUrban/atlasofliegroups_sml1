use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/KType.sml";

(*
  File: atlas-scripts-sml/tensor_product_sl2.sml

  Purpose
  - SML translation of `atlas-scripts/tensor_product_sl2.at`.
  - The `.at` script defines:
        tensor_sl2(mu,tau) = K_type(k_highest_weight(x0, hw(mu,x0)+hw(tau,x0)))
    where `x0 = KGB(G,0)` is in the distinguished fiber.

  Current status / limitations
  - The full `.at` implementation depends on `K_highest_weights.at`’s
    `highest_weight(mu,x0)` and the `KHighestWeight` datatype from `K.at`.
  - Those components are not yet fully ported to SML.

  Implementation provided here
  - We implement a conservative *approximation* that is sufficient for some
    semisimple-rank-1 experiments:
      - interpret `KType.lambdaRhoText` (Atlas “lambda_minus_rho” vector) as the
        weight to be added
      - use `x0 = 0`
      - return `KType.newFromXAndLambdaRhoText(g,x0,...)` with the componentwise
        sum of those vectors.

  Safety
  - This function checks that both input K-types belong to the same Atlas group
    handle; otherwise it raises `Fail`.

  Ownership
  - The returned `KType.ktype` handle is owned by the caller and must be freed
    with `KType.free`.
*)
structure Tensor_product_sl2 = struct
  type ktype = KType.ktype
  type group = AtlasFFI.group

  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("Tensor_product_sl2: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun vecFromLambdaRhoText (s: string) : int list =
    (case parseInts s of
       n :: rest =>
         if length rest = n then
           rest
         else
           raise Fail "Tensor_product_sl2: lambda_rho text length mismatch"
     | _ => raise Fail "Tensor_product_sl2: empty lambda_rho text")

  fun vecTextWithHeader (xs: int list) : string =
    let
      fun intToCText n =
        let
          val s = Int.toString n
        in
          if String.size s > 0 andalso String.sub (s, 0) = #"~" then "-" ^ String.extract (s, 1, NONE) else s
        end
    in
      Int.toString (length xs) ^ " " ^ String.concatWith " " (List.map intToCText xs)
    end

  fun groupOfKType (t: ktype) : group =
    let
      val p = KType.parameter t
      val g = AtlasFFI.atlas_param_group_handle p
      val () = AtlasFFI.atlas_param_free p
    in
      g
    end

  (* Approximate tensor product by adding `lambda_minus_rho` coordinates at `x0=0`. *)
  fun tensor_sl2 (mu: ktype, tau: ktype) : ktype =
    let
      val gMu = groupOfKType mu
      val gTau = groupOfKType tau
      val () =
        if gMu = gTau then
          ()
        else
          raise Fail "Tensor_product_sl2.tensor_sl2: K-types from different groups"

      val a = vecFromLambdaRhoText (KType.lambdaRhoText mu)
      val b = vecFromLambdaRhoText (KType.lambdaRhoText tau)
      val () = if length a = length b then () else raise Fail "Tensor_product_sl2.tensor_sl2: rank mismatch"
      val c = ListPair.mapEq (op +) (a, b)
      val x0 = 0
    in
      KType.newFromXAndLambdaRhoText (gMu, x0, vecTextWithHeader c)
    end
end

