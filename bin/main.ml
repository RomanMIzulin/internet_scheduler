open Lwt
open Cmdliner

(* Types *)
type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

type time = { hour : int; minute : int }

type schedule = {
  days : day list;
  start_time : time;
  end_time : time;
  enabled : bool
}

type action = Enable | Disable

(* Schedule management *)
let schedules : schedule list ref = ref []

let add_schedule schedule =
  schedules := schedule :: !schedules

let remove_schedule index =
  schedules := List.filteri (fun i _ -> i <> index) !schedules

let list_schedules () =
  List.iteri (fun i s ->
    Printf.printf "[%d] %s %02d:%02d-%02d:%02d %s\n"
      i
      (String.concat "," (List.map (function
        | Mon -> "Mon" | Tue -> "Tue" | Wed -> "Wed" | Thu -> "Thu"
        | Fri -> "Fri" | Sat -> "Sat" | Sun -> "Sun") s.days))
      s.start_time.hour s.start_time.minute
      s.end_time.hour s.end_time.minute
      (if s.enabled then "Enabled" else "Disabled")
  ) !schedules

(* Parsing *)
let parse_time str =
  Scanf.sscanf str "%d:%d" (fun h m -> {hour = h; minute = m})

let parse_days str =
  String.split_on_char ',' str
  |> List.map (function
    | "Mon" -> Mon | "Tue" -> Tue | "Wed" -> Wed | "Thu" -> Thu
    | "Fri" -> Fri | "Sat" -> Sat | "Sun" -> Sun
    | _ -> failwith "Invalid day")

let parse_schedule str =
  match String.split_on_char ' ' str with
  | [days; times] ->
      let days = parse_days days in
      let start_time, end_time = match String.split_on_char '-' times with
        | [start; end_] -> parse_time start, parse_time end_
        | _ -> failwith "Invalid time format"
      in
      {days; start_time; end_time; enabled = true}
  | _ -> failwith "Invalid schedule format"

(* Iptables control *)
let run_iptables_command = function
  | Enable ->
      Lwt_process.exec ("iptables", [|"iptables"; "-D"; "OUTPUT"; "1"|]) >>= fun _ ->
      Lwt_process.exec ("iptables", [|"iptables"; "-D"; "INPUT"; "1"|])
  | Disable ->
      Lwt_process.exec ("iptables", [|"iptables"; "-I"; "OUTPUT"; "1"; "-p"; "all"; "-j"; "DROP"|]) >>= fun _ ->
      Lwt_process.exec ("iptables", [|"iptables"; "-I"; "INPUT"; "1"; "-p"; "all"; "-j"; "DROP"|])

(* Scheduler *)
let day_of_week d =
  match d with
  | 0 -> Sun | 1 -> Mon | 2 -> Tue | 3 -> Wed | 4 -> Thu | 5 -> Fri | 6 -> Sat
  | _ -> invalid_arg "day_of_week"

let time_to_float {hour; minute} =
  float_of_int (hour * 60 + minute) /. 60.

let check_schedule schedule now =
  let open Unix in
  let tm = localtime now in
  let current_day = day_of_week tm.tm_wday in
  let current_time = time_to_float {hour = tm.tm_hour; minute = tm.tm_min} in
  List.mem current_day schedule.days &&
  current_time >= time_to_float schedule.start_time &&
  current_time < time_to_float schedule.end_time

let rec scheduler_loop () =
  let now = Unix.time () in
  let action = List.fold_left (fun acc schedule ->
    if schedule.enabled && check_schedule schedule now then Enable else acc
  ) Disable !schedules in
  run_iptables_command action >>= fun _ ->
  Lwt_unix.sleep 60.0 >>= fun () ->
  scheduler_loop ()

(* CLI commands *)
let add_cmd =
  let doc = "Add a new schedule" in
  let schedule =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SCHEDULE" ~doc)
  in
  Term.(const (fun s -> add_schedule (parse_schedule s))
        $ schedule),
  Term.info "add" ~doc

let remove_cmd =
  let doc = "Remove a schedule" in
  let index =
    Arg.(required & pos 0 (some int) None & info [] ~docv:"INDEX" ~doc)
  in
  Term.(const remove_schedule $ index),
  Term.info "remove" ~doc

let list_cmd =
  let doc = "List all schedules" in
  Term.(const list_schedules $ const ()),
  Term.info "list" ~doc

let default_cmd =
  let doc = "Iptables scheduler" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "iptables-scheduler" ~version:"0.1" ~doc

let () =
  let cmds = [add_cmd; remove_cmd; list_cmd] in
  Term.(exit @@ eval_choice default_cmd cmds)

(* Main *)
let main () =
  Lwt_main.run (scheduler_loop ())

let () =
  if Array.length Sys.argv = 1 then main ()
