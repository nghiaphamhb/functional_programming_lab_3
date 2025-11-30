open Point

(* Interpolations' general interface *)
module type S = sig
  val name : string
  (* Interpolation's name *)

  val eval : Point.t list -> float -> float
  (* [eval pts x] calculate y interpolation at x, based on points list [pts].*)
end

