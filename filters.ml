let driver_creation_filter requirement antirequirement commit =
    (* Checks if the commit introduce a new driver
     * Implementation of conditions precised at the start of this file
     *)
    List.exists
    ( function a -> match a with
    | {Commits.file_name=file_name;
       Commits.modification=Commits.Created}
        when (
            Tools.is_c_file file_name &&
            Tools.is_file_in_paths file_name requirement &&
            not (Tools.is_file_in_paths file_name antirequirement)
        ) -> true
    | _ -> false
    ) commit.Commits.files &&

    List.for_all
    ( function a -> match a with
    | {Commits.file_name=file_name;
       Commits.modification=Commits.Created}
        when (
            (Tools.is_c_file file_name &&
            Tools.is_file_in_paths file_name requirement &&
            not (Tools.is_file_in_paths file_name antirequirement)
            )
        ) -> true
    | {Commits.file_name=file_name;
       Commits.modification=Commits.AddOnly}
        when (
            (Tools.is_c_file file_name &&
            not (Tools.is_file_in_paths file_name requirement &&
                not (Tools.is_file_in_paths file_name antirequirement))
            )
        ) -> true
    | {Commits.file_name=file_name;
       Commits.modification= Commits.Created | Commits.AddOnly}
        when (Tools.is_h_file file_name) -> true
    | {Commits.file_name=file_name; _ }
        when not (
            (Tools.is_c_file file_name) ||
            (Tools.is_h_file file_name)
        ) -> true
    | _ -> false
    ) commit.Commits.files &&

    List.exists
    ( function a -> match a with
    | {Commits.file_name=file_name;
       Commits.modification= Commits.Created | Commits.AddOnly}
        when (
            List.mem (Filename.basename file_name)
            ["Makefile";"Kconfig";"Kbuild"]
        ) -> true
    | _ -> false
    ) commit.Commits.files


let keep_added requirement antirequirement commits =
    (* Filter commit list to keep only those which add a driver *)
    List.filter (driver_creation_filter requirement antirequirement) commits


let keep_existing target commits =
    (* Filter commit list to keep only those files still exist
     * in target version *)
    Tools.git_setup target;

    let files_exists files =
        List.for_all (function file ->
            Sys.file_exists file.Commits.file_name)
            files
    in
    List.filter (function commit -> files_exists commit.Commits.files) commits

