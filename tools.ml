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

;;
