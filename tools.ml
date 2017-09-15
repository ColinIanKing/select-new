exception ProcessError of Unix.process_status * (string list)

let cmd_to_list command =
    (* Open process and retrieve stdout output as list*)
    let chan = Unix.open_process_in command in
    let res = ref ([] : string list) in

    (* Concatenate output lines into list *)
    let rec process_otl_aux () =
        let e = input_line chan in
        res := e::!res;
        process_otl_aux()
    in

    try process_otl_aux ()
    with End_of_file ->
        let status = Unix.close_process_in chan in
        match status with
        | Unix.WEXITED(0) -> List.rev !res
        | _ -> raise (ProcessError(status, List.rev !res))


let is_file_in_paths file paths =
    List.exists (function path ->
        Str.string_match (Str.regexp_string path) file 0
    ) paths


let check_command s =
    let dir = Sys.getcwd() in
    let res = Sys.command
        (Printf.sprintf "%s > err%s 2>&1" s (Filename.basename dir))
    in
    if not (res = 0)
        then failwith (Printf.sprintf "failure on %s in %s" s dir)


let print_progress total index =
    let max_index = total / (Parmap.get_ncores ()) in
    let rank = Parmap.get_rank () in
    let index =
        if rank = -1
            then (index*50)/max_index
            else 50 - (index*50/max_index)
    in
    let get_char = function
    | i when (i = index) -> '>'
    | i when (i < index) -> '='
    | _ -> ' '
    in

    let to_print = "[" ^ (String.init 50 get_char) ^ "]" in

    Printf.eprintf "\r%s%!" to_print


let git_setup version =
  (* Clean the current git repository and checkout to `version` *)
  check_command "git clean -dfx"; (* Remove untracked files *)
  check_command "git reset --hard"; (* Remove uncommited tracked files *)
  check_command ("git checkout "^version); (* Set files to version *)
  if (version != "master") then check_command "make allyesconfig"
