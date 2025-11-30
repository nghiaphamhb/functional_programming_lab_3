open My_lab3
open Point
open Interp

(* Helper to construct a point from x and y coordinates *)
let p x y = { x; y }

(* Floating-point comparator for Alcotest with epsilon = 1e-6 *)
let float_eps = Alcotest.float 1e-6

(* ===== Test 1: Linear.eval ===== *)

(* Test that Linear.eval correctly reproduces y = x
   on the segment between (0,0) and (2,2). *)
let test_linear_simple () =
  let module L = Linear in
  let pts = [ p 0. 0.; p 2. 2. ] in
  (* For each x_expected, compute y = L.eval pts x_expected
     and check that y ≈ x_expected. *)
  let check_x x_expected =
    let y = L.eval pts x_expected in
    Alcotest.check float_eps "y = x (linear)" x_expected y
  in
  (* Test several sample points on [0,2] *)
  List.iter check_x [ 0.; 0.5; 1.; 1.5; 2. ]

(* Test Linear.eval on a shifted line y = 2x + 1
   using two points (1,3) and (3,7). *)
let test_linear_shifted () =
  let module L = Linear in
  (* line y = 2x + 1 passing through (1,3) and (3,7) *)
  let pts = [ p 1. 3.; p 3. 7. ] in
  (* (x, expected y = 2x+1) *)
  let cases = [ (1., 3.); (2., 5.); (3., 7.) ] in
  List.iter
    (fun (x, y_expected) ->
      let y = L.eval pts x in
      Alcotest.check float_eps "y = 2x+1" y_expected y)
    cases

(* ===== Test 2: Newton.eval_n on y = x^2 ===== *)

(* Test that Newton.eval_n with 4 points on y = x^2
   reconstructs the polynomial exactly (within epsilon). *)
let test_newton_x2 () =
  let module N = Newton in
  (* Sample points on y = x^2 *)
  let pts = [ p 0. 0.; p 1. 1.; p 2. 4.; p 3. 9. ] in
  (* A set of x values where we will test the interpolating polynomial *)
  let cases = [ 0.; 0.5; 1.; 1.5; 2.; 2.5; 3. ] in
  List.iter
    (fun x ->
      let y = N.eval_n 4 pts x in
      let y_expected = x *. x in
      Alcotest.check float_eps "Newton x^2" y_expected y)
    cases

(* ===== Test 3: small pure streaming simulation for linear ===== *)

(* We simulate the behavior of run_linear in a pure way:
   - No stdin/stdout, only functions and lists.
   - Given a list of input points and a step,
     we produce the list of (x, y) points that would be printed. *)

let simulate_linear_stream ~step (points : Point.t list) : (float * float) list
    =
  let module L = Linear in
  (* Internal state:
     prev  : previous point in the sliding window (option)
     curr  : current point in the sliding window (option)
     next_x: next x from which we will start/continue sampling (option)
     acc   : accumulator of produced (x, y) results in reverse order
   *)
  let rec feed_points prev curr next_x acc pts =
    match pts with
    | [] ->
        (* No more input points: return accumulated results in correct order *)
        List.rev acc
    | p :: rest -> (
        (* Update the sliding window of size 2: (prev', curr') *)
        let prev', curr' =
          match (prev, curr) with
          | None, None -> (Some p, None)
          | Some prev0, None -> (Some prev0, Some p)
          | Some _prev0, Some curr0 -> (Some curr0, Some p)
          | None, Some _ -> failwith "invalid state"
        in
        match (prev', curr') with
        | Some prevp, Some currp ->
            (* We have two points: interpolate between prevp.x and currp.x *)
            let start_x =
              match next_x with
              | None -> prevp.x (* first time on this segment *)
              | Some x -> x (* continue from previous position *)
            in
            (* produce: iteratively generate (x, y) pairs on the segment
               [start_x, currp.x] with step "step". Returns:
               - next_x' : the x value just past currp.x
               - acc'    : updated accumulator with newly generated points
             *)
            let rec produce x acc =
              if x > currp.x then (x, acc)
              else
                let y = L.eval [ prevp; currp ] x in
                produce (x +. step) ((x, y) :: acc)
            in
            let next_x', acc' = produce start_x acc in
            (* Recurse with updated state and remaining input points *)
            feed_points prev' curr' (Some next_x') acc' rest
        | _ ->
            (* Not enough points to interpolate yet, continue reading input *)
            feed_points prev' curr next_x acc rest)
  in
  feed_points None None None [] points

(* Test that simulate_linear_stream reproduces the expected sequence
   of (x, y) points for a simple y = x case. *)
let test_linear_stream () =
  let points = [ p 0. 0.; p 1. 1.; p 2. 2. ] in
  let result = simulate_linear_stream ~step:0.5 points in
  let expected = [ (0., 0.); (0.5, 0.5); (1., 1.); (1.5, 1.5); (2., 2.) ] in
  Alcotest.(check (list (pair float_eps float_eps)))
    "linear streaming" expected result

