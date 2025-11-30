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

