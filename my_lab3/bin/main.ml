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
let run_both ~step ~n ~use_linear ~use_newton =
  let module L = Linear in
  let module N = Newton in
  let is_interactive = Unix.isatty Unix.stdin in

  (* === Main logic: check and run algorithm === *)
  let rec loop window next_x_opt =
    if is_interactive then (
      output_string stderr "< ";
      flush stderr);
    match input_line stdin with
    | line -> (
        match parse_line line with
        | None -> loop window next_x_opt
        | Some p ->
            let window' = window |> append_one p |> trim_last_k n in
            let len = List.length window' in

            if len >= 2 then
              let start_x =
                match next_x_opt with
                | None -> (
                    match window' with [] -> p.x | first :: _ -> first.x)
                | Some x -> x
              in
              let x_max =
                match List.rev window' with [] -> p.x | last :: _ -> last.x
              in
              (* === Choose algorithm by label === *)
              let last2 = last_two window' in
              let has_newton = len >= n in
              let first_x =
                match window' with [] -> p.x | first :: _ -> first.x
              in

              let rec produce x =
                if x > x_max then x
                else (
                  (* ----- Linear interpolation part ----- *)
                  (if use_linear then
                     match last2 with
                     | Some (p1, p2) when x >= p1.x && x <= p2.x ->
                         let y_lin = L.eval [ p1; p2 ] x in
                         print_result L.name x y_lin
                     | _ -> ());

                  (* ----- Newton interpolation part ----- *)
                  (if use_newton && has_newton && x >= first_x && x <= x_max
                   then
                     let y_new = N.eval_n n window' x in
                     print_result N.name x y_new);

                  produce (x +. step))
              in
              (* Done the loop *)
              let next_x' = Some (produce start_x) in
              loop window' next_x'
            else loop window' next_x_opt)
    | exception End_of_file -> ()
  in
  loop [] None

(* ========= Main ========= *)
let () =
  let cfg =
    try get_config ()
    with Failure msg ->
      prerr_endline ("Argument error: " ^ msg);
      exit 1
  in
  if (not cfg.use_linear) && not cfg.use_newton then (
    prerr_endline
      "Error: choose at least one algorithm: --linear and/or --newton";
    exit 1);
  (* both algorithms  *)
  if cfg.use_linear && cfg.use_newton then
    run_both ~step:cfg.step ~n:cfg.newton_n ~use_linear:true ~use_newton:true
    (* only newton *)
  else if cfg.use_newton then run_newton ~step:cfg.step ~n:cfg.newton_n
  (* only linear *)
    else run_linear ~step:cfg.step