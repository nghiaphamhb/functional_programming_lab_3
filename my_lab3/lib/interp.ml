open Point

(* Interpolations' general interface *)
module type S = sig
  val name : string
  (* Interpolation's name *)

  val eval : Point.t list -> float -> float
  (* [eval pts x] calculate y interpolation at x, based on points list [pts].*)
end

(* Linear interpolation (2 points) *)
module Linear : S = struct
  let name = "linear"

  let eval pts x =
    match pts with
    | p1 :: p2 :: _ ->
        let x1, y1 = (p1.x, p1.y) in
        let x2, y2 = (p2.x, p2.y) in
        if x2 = x1 then invalid_arg "Linear.eval: x1 = x2 (division by zero)";
        let t = (x -. x1) /. (x2 -. x1) in
        y1 +. (t *. (y2 -. y1))
    | _ -> invalid_arg "Linear.eval: need at least 2 points"
end

(* Newton interpolation (n points) *)

module Newton = struct
  let name = "newton"

  (* Get the first element k *)
  let rec take k lst =
    if k <= 0 then []
    else match lst with [] -> [] | h :: t -> h :: take (k - 1) t

  (* Attach index to list: [(0,p0); (1,p1); ...] *)
  let with_index lst =
    let rec aux i = function [] -> [] | h :: t -> (i, h) :: aux (i + 1) t in
    aux 0 lst

  (* Lagrange interpolation on first n points *)
  let eval_n n pts x =
    let pts_used = take n pts in
    match pts_used with
    | [] -> invalid_arg "Newton.eval_n: empty point list"
    | _ ->
        let indexed = with_index pts_used in
        let rec outer acc = function
          | [] -> acc
          | (i, pi) :: rest ->
              let xi = pi.x and yi = pi.y in
              let num, den =
                List.fold_left
                  (fun (num, den) (j, pj) ->
                    if i = j then (num, den)
                    else
                      let xj = pj.x in
                      (num *. (x -. xj), den *. (xi -. xj)))
                  (1., 1.) indexed
              in
              let li = num /. den in
              outer (acc +. (yi *. li)) rest
        in
        outer 0. indexed
end
