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

