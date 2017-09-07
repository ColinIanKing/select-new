open Printf

type modification =
    | Created
    | Deleted
    | AddOnly
    | Modified

type show_line =
    | Hash of string
    | Stat of int * int * string
    | None

type commit_file = { file_name : string; modification : modification }
type commit = { hash : string; files: commit_file list }


let git_log_common =
    (* Common command line for git log
     * We only need commits that add something *)
    "git log --oneline --no-merges --pretty=format:\"%h\" --diff-filter=A "


let list_by_dates start_date end_date =
    (* Returns the list of short hashes of commits
    * that happened between start_date and end_date*)
    let git_command = git_log_common ^ (sprintf "--since=\"%s\" --until=\"%s\"" start_date end_date) in
    Tools.cmd_to_list git_command


let list_by_range range =
    (* Returns the list of short hashes of commits
    * that happened in range *)
    let git_command = git_log_common ^ range in
    Tools.cmd_to_list git_command


let list_by_files files =
    (* Returns the list of short hashes of commits
    * that concerns files *)
    let git_command = git_log_common ^ (String.concat " " files) in
    Tools.cmd_to_list git_command

let list_by_hash_list hashes =
    (* Reformat the hashes to short hash format *)
    let git_command =
        "git show --pretty=format:\"%h\" -s " ^ (String.concat " " hashes)
    in
    Tools.cmd_to_list git_command


let parse_commits commits =
    let nb_commits = List.length commits in
    let table = Hashtbl.create nb_commits in

    let git_show_common = "git show --numstat --pretty=format:\"%h\"" in
    let commits_string = String.concat " " commits in
    let created_command = git_show_common ^ " --diff-filter=A " ^ commits_string in
    let deleted_command = git_show_common ^ " --diff-filter=D " ^ commits_string in
    let modified_command = git_show_common ^ " --diff-filter=M " ^ commits_string in

    let get_pieces line =
        (* Extract information from git show --numstat *)
        match Str.split (Str.regexp "[\t ]+") line with
        | [hash] when not (hash = "") -> Hash(hash)
        | [added; removed; file] when not (added = "-" || removed = "-") ->
            (* - used for binary files, such as png doc, don't care *)
            Stat(int_of_string added, int_of_string removed, file)
        | _ -> None
    in

    let parse_created () =
        let created = Tools.cmd_to_list created_command in
        let parse hash line =
            match get_pieces line with
            | Hash(hash) -> hash (* Update current commit hash *)
            | Stat(added, 0, file) -> (* Note that added can be 0 if permissions changed *)
                (* Add files in commit to the table *)
                Hashtbl.add table hash {file_name=file; modification=Created}; hash 
            | Stat(_, deleted, _) -> failwith "Error: Nothing should be removed !"
            | None -> hash
        in
        ignore (List.fold_left parse "" created)
    in

    let parse_deleted () =
        let deleted = Tools.cmd_to_list deleted_command in
        let parse hash line =
            match get_pieces line with
            | Hash(hash) -> hash (* Update current commit hash *)
            | Stat(0, deleted, file) ->
                (* Add files in commit to the table *)
                Hashtbl.add table hash {file_name=file; modification=Deleted}; hash 
            | Stat(added, _, _) -> failwith "Error: Nothing should be added !"
            | None -> hash
        in
        ignore (List.fold_left parse "" deleted)
    in

    let parse_modified () =
        let modified = Tools.cmd_to_list modified_command in
        let parse hash line =
            match get_pieces line with
            | Hash(hash) -> hash (* Update current commit hash *)
            | Stat(_, 0, file) ->
                (* Add files in commit to the table *)
                Hashtbl.add table hash {file_name=file; modification=AddOnly}; hash 
            | Stat(_, _, file) ->
                (* Add files in commit to the table *)
                Hashtbl.add table hash {file_name=file; modification=Modified}; hash 
            | None -> hash
        in
        ignore (List.fold_left parse "" modified);
    in

    parse_created ();
    parse_deleted ();
    parse_modified ();

    let table_to_commit hash =
        let commit_files = Hashtbl.find_all table hash in
        { hash=hash; files=commit_files }
    in
    List.map table_to_commit commits
;;
