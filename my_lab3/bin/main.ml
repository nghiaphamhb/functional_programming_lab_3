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

(** parse_args : config -> string list -> config Function to parse command line
    arguments into config record. *)
let rec parse_args cfg args =
  match args with
  (* End of recursion *)
  | [] -> cfg
  (* linear interpolation *)
  | "--linear" :: rest ->
      let cfg' = { cfg with use_linear = true } in
      parse_args cfg' rest
  (* newton interpolation *)
  | "--newton" :: rest ->
      let cfg' = { cfg with use_newton = true } in
      parse_args cfg' rest
  (* step size *)
  | "--step" :: value :: rest ->
      let step =
        try float_of_string value
        with Failure _ -> failwith ("--step expects a float, got: " ^ value)
      in
      let cfg' = { cfg with step } in
      parse_args cfg' rest
  (* number of points for newton *)
  | "-n" :: value :: rest ->
      let n =
        try int_of_string value
        with Failure _ -> failwith ("-n expects an int, got: " ^ value)
      in
      let cfg' = { cfg with newton_n = n } in
      parse_args cfg' rest
  (* unknown argument *)
  | arg :: _ -> failwith ("Unknown argument: " ^ arg)

(** Call this function to active function parse_args get_config : unit -> config
*)
let get_config () =
  let args =
    Sys.argv |> Array.to_list |> function
    | [] -> []
    | _prog_name :: rest -> rest
    (* Skip argv[0] *)
  in
  parse_args default_config args

(** Function parses input line into point's record parse_line : string ->
    Point.t *)
let parse_line (line : string) : Point.t option =
  let line = String.trim line in
  if line = "" then None
  else
    (* separators = ';' or '\t' or ' ' *)
    let sep =
      if String.contains line ';' then ';' (* x;y *)
      else if String.contains line '\t' then '\t' (* x[tab]y *)
      else ' ' (* x y *)
    in
    let parts =
      line |> String.split_on_char sep |> List.filter (fun s -> s <> "")
      (* filter empty char *)
    in
    match parts with
    | [ sx; sy ] ->
        let x = float_of_string sx in
        let y = float_of_string sy in
        Some { x; y }
    | _ ->
        prerr_endline ("Cannot parse line: " ^ line);
        None

(** function prints result print_result : string -> float -> float -> unit *)
let print_result algo_name x y = Printf.printf "> %s: %g %g\n%!" algo_name x y

(** streaming linear interpolation:
    + Infinite loop reading lines from stdin.
    + Each line is parsed into a point (x, y).
    + Keep a sliding window of 2 most recent points (prev_point, curr_point).
    + When we have 2 points, interpolate from prev_point.x to curr_point.x with
      the given step.
    + Print results to stdout.
    + Then read next point, slide the window, and repeat. run_linear :
      step:float -> unit *)
let run_linear ~step =
  let module A = Linear in
  (* tail-recursive loop:
     prev_point : previous point in the window (or None)
     curr_point : current point in the window (or None)
     next_x_opt : next x at which we should start interpolating
   *)
  let rec loop prev_point curr_point next_x_opt =
    match input_line stdin with
    (* Successfully read one line from stdin *)
    | line -> (
        (* Try to parse line into a point (x, y) *)
        match parse_line line with
        | None ->
            (* Invalid or empty line: skip and continue with the same state *)
            loop prev_point curr_point next_x_opt
        | Some p -> (
            (* Update the sliding window with the new point p *)
            let prev', curr' =
              match (prev_point, curr_point) with
              | None, None ->
                  (* First point: window has only the new point p *)
                  (Some p, None)
              | Some prev, None ->
                  (* Second point: now window has (prev, p) *)
                  (Some prev, Some p)
              | Some _prev, Some curr ->
                  (* More points: slide window forward: (curr, p) *)
                  (Some curr, Some p)
              | None, Some _ ->
                  (* This state should never happen if logic is correct *)
                  failwith "Invalid state"
            in
            (* If we have 2 points in the window, perform interpolation *)
            match (prev', curr') with
            | Some prev, Some curr ->
                (* Determine the starting x for interpolation:
                     - if we never interpolated in this segment: start at prev.x
                     - otherwise: resume from the previously stored next_x
                   *)
                let start_x =
                  match next_x_opt with None -> prev.x | Some x -> x
                in
                (* Produce interpolated points from start_x up to curr.x
                     with the given step. Returns the last x used (just past curr.x).
                   *)
                let rec produce x =
                  if x > curr.x then
                    (* Stop when x goes beyond the current segment *)
                    x
                  else
                    (* Interpolate at x using the two points [prev; curr] *)
                    let y = A.eval [ prev; curr ] x in
                    (* Print result in the required "algo_name: x y" format *)
                    print_result A.name x y;
                    (* Move to the next sample point *)
                    produce (x +. step)
                in
                (* Store the next x from which to continue in the next iteration *)
                let next_x' = Some (produce start_x) in
                (* Recurse with updated window and next_x *)
                loop prev' curr' next_x'
            | _ ->
                (* Not enough points yet (we only have 0 or 1 point),
                     just continue reading input.
                   *)
                loop prev' curr' next_x_opt))
    | exception End_of_file ->
        (* End of input: stop the loop *)
        ()
  in
  (* Initial call: no points, no next_x *)
  loop None None None

(* Function add 1 element to the end of the list *)
let append_one x lst = lst @ [ x ]

(* Function cuts list to keep maximum k end element only *)
let trim_last_k k lst =
  let len = List.length lst in
  let drop = len - k in
  let rec drop_n n l =
    if n <= 0 then l else match l with [] -> [] | _ :: t -> drop_n (n - 1) t
  in
  if drop <= 0 then lst else drop_n drop lst

(* Function takes the last 2 elements of the list, if any *)
let last_two lst =
  match List.rev lst with
  | a :: b :: _ -> Some (b, a) (* take b first *)
  | _ -> None

(** streaming Newton interpolation with sliding window of n points:

    + Infinite loop reading lines from stdin.
    + Each line is parsed into a point (x, y).
    + Keep a sliding window "window" of the last at most n points.
    + When we have at least n points in the window:

    - Define an x-interval [x_min, x_max] based on the window.
    - For x from start_x to x_max, with step "step", evaluate the Newton
      interpolating polynomial built from the n points.
    - Print results to stdout.

    + Then read the next point, slide the window, and repeat. run_newton :
      step:float -> n:int -> unit *)
let run_newton ~step ~n =
  (* Alias for the Newton interpolation module *)
  let module A = Newton in
  (* tail-recursive loop:
     window     : list of recent points (kept to at most n points)
     next_x_opt : next x from which we should continue interpolation
   *)
  let rec loop window next_x_opt =
    match input_line stdin with
    (* Successfully read one line from stdin *)
    | line -> (
        (* Try to parse the line into a point (x, y) *)
        match parse_line line with
        | None ->
            (* Invalid or empty line: ignore and keep the same state *)
            loop window next_x_opt
        | Some p ->
            (* Add the new point p to the window, 
                 then trim so that we keep at most n last points. *)
            let window' =
              window
              |> append_one p (* append at the end *)
              |> trim_last_k n (* keep only the last n points *)
            in
            (* If we have at least n points, we can interpolate *)
            if List.length window' >= n then
              (* Determine starting x:
                   - If this is the first time we interpolate in this window,
                     start at the smallest x in the window (x of the first point).
                   - Otherwise, continue from the previously stored next_x.
                 *)
              let start_x =
                match next_x_opt with
                | None -> (
                    (* start from x of the first point in the window *)
                    match window' with
                    | [] -> p.x (* should not happen if length >= n *)
                    | first :: _ -> first.x)
                | Some x -> x
              in
              (* x_max is the x-coordinate of the last point in the window
                   (i.e., the right boundary of the interpolation interval). *)
              let x_max =
                match List.rev window' with
                | [] -> p.x (* should not happen if length >= n *)
                | last :: _ -> last.x
              in
              (* Produce interpolated points on [start_x, x_max]
                   with the given step, using Newton interpolation on n points. *)
              let rec produce x =
                if x > x_max then
                  (* Stop when x goes past the current window interval *)
                  x
                else
                  (* Evaluate Newton polynomial of degree (n-1) at x,
                       using the n points in window'. *)
                  let y = A.eval_n n window' x in
                  (* Print result in "algo_name: x y" format *)
                  print_result A.name x y;
                  (* Move to the next sample point *)
                  produce (x +. step)
              in
              (* Store the next x from which we should continue
                   the next time we enter this window. *)
              let next_x' = Some (produce start_x) in
              (* Recurse with updated window and next_x *)
              loop window' next_x'
            else
              (* Not enough points yet (we have less than n),
                   so we cannot build a Newton interpolating polynomial.
                   Just continue reading input. *)
              loop window' next_x_opt)
    | exception End_of_file ->
        (* End of input: stop the loop *)
        ()
  in
  (* Initial call: empty window, no next_x yet *)
  loop [] None

(** streaming both Linear and Newton interpolation on the same input stream:

    + Infinite loop reading lines from stdin.
    + Each line is parsed into a point (x, y).
    + Keep a sliding window "window" of the last at most n points.
    + For each x in the current interval:

    - Always try to compute Linear interpolation using the last two points.
    - Additionally, if we have at least n points, compute Newton interpolation
      on the whole window of n points.

    + Print both results (Linear + Newton) to stdout when applicable. run_both :
      step:float -> n:int -> unit *)
let run_both ~step ~n =
  (* Aliases for the two interpolation modules *)
  let module L = Linear in
  let module N = Newton in
  (* tail-recursive loop:
     window     : list of recent points (up to n points)
     next_x_opt : next x from which to continue sampling
   *)
  let rec loop window next_x_opt =
    match input_line stdin with
    (* Successfully read one line from stdin *)
    | line -> (
        match parse_line line with
        | None ->
            (* Empty line / parse error – skip this line and keep state *)
            loop window next_x_opt
        | Some p ->
            (* Update the sliding window: append p and keep at most n points *)
            let window' = window |> append_one p |> trim_last_k n in
            let len = List.length window' in

            (* Only process when we have at least 2 points (Linear needs 2) *)
            if len >= 2 then
              (* Determine starting x:
                   - If this is the first time in this window, start from
                     the x of the first point in the window.
                   - Otherwise, resume from the previously stored next_x.
                 *)
              let start_x =
                match next_x_opt with
                | None -> (
                    (* Start from x of the first point in the window *)
                    match window' with
                    | [] -> p.x (* should not happen if len >= 2 *)
                    | first :: _ -> first.x)
                | Some x -> x
              in
              (* x_max: x-coordinate of the last point in the window
                   (right boundary of the current segment). *)
              let x_max =
                match List.rev window' with
                | [] -> p.x (* should not happen if len >= 2 *)
                | last :: _ -> last.x
              in

              (* Precompute some helpers for the produce loop *)
              let last2 = last_two window' in
              let has_newton = len >= n in
              let first_x =
                match window' with [] -> p.x | first :: _ -> first.x
              in

              (* Produce both Linear and Newton results on [start_x, x_max]
                   with step "step". For each x:
                     - Linear interpolation uses only the last two points.
                     - Newton interpolation (if available) uses all n points
                       in the current window.
                 *)
              let rec produce x =
                if x > x_max then
                  (* Stop when x is beyond the current interval *)
                  x
                else (
                  (* ----- Linear interpolation part ----- *)
                  (* Use the segment formed by the last two points in the window *)
                  (match last2 with
                  | Some (p1, p2) when x >= p1.x && x <= p2.x ->
                      let y_lin = L.eval [ p1; p2 ] x in
                      print_result L.name x y_lin
                  | _ ->
                      (* Either we don't have two points, or x is outside
                            the last segment: do nothing for Linear. *)
                      ());

                  (* ----- Newton interpolation part ----- *)
                  (* If we have at least n points, and x is within [first_x, x_max],
                       evaluate the Newton interpolant on the full window. *)
                  (if has_newton && x >= first_x && x <= x_max then
                     let y_new = N.eval_n n window' x in
                     print_result N.name x y_new);

                  (* Move to the next sample point *)
                  produce (x +. step))
              in

              (* Compute new next_x to continue from next time *)
              let next_x' = Some (produce start_x) in
              (* Recurse with updated window and next_x *)
              loop window' next_x'
            else
              (* Not enough points (less than 2) to do Linear or Newton,
                   just continue reading input. *)
              loop window' next_x_opt)
    | exception End_of_file ->
        (* End of input: stop the loop *)
        ()
  in
  (* Initial call: empty window, no next_x *)
  loop [] None

