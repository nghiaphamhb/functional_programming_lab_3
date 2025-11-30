open My_lab3
open Point
open Interp

(* Parameters' record *)
type config = {
  use_linear : bool;
  use_newton : bool;
  step : float;
  newton_n : int;
}

(* Default parameters *)
let default_config =
  { use_linear = false; use_newton = false; step = 0.5; newton_n = 4 }

