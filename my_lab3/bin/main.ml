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

