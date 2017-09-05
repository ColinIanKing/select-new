open Printf

type commit_hash = string

let list_by_dates start_date end_date =
    (* Returns the list of short hashes of commits 
    * that happened between start_date and end_date*)
    let git_command = sprintf 
        "git log --oneline --no-merges --pretty=format:\"%%h\" --since=\"%s\" --until=\"%s\"" 
        start_date end_date in
    Tools.cmd_to_list git_command

let list_by_range range =
    (* Returns the list of short hashes of commits 
    * that happened in range *)
    let git_command = sprintf "git log --oneline --no-merges --pretty=format:\"%%h\" %s" range in
    Tools.cmd_to_list git_command

let list_by_files files =
    (* Returns the list of short hashes of commits 
    * that concerns files *)
    let git_command = sprintf
        "git log --oneline --no-merges --pretty=format:\"%%h\" %s"
        (String.concat " " files) in
    Tools.cmd_to_list git_command


;;
