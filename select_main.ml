(* Goal: select drivers added in a time range and highly modified since then.
Could specify a directory as well. *)

(* This is a bit more complex than requiring that all files are added,
because it seems that some make files, kconfig files, and testing files can
be modified, rather than added.

.c files in the matching directory should only be added, and there should
be at least one of these
.c files in other directories (eg tools/testing) should only contain additions
.h files should be added or only contain additions
Makefile, Kconfig, and Kbuild should only contain additions

There are no constraints on other files. *)

let git = ref "/run/shm/linux"
let giti i = Printf.sprintf "%s%d" !git i
let home = Sys.getcwd ()
let target = ref "4.6"
let cores = ref 1
let start_time = ref "Jan 1, 2015"
let end_time = ref "Dec 31, 2015"
let range = ref ""
let list = ref []
let work_dir = ref "2015"
let requirement = ref ["drivers/"]
let antirequirement = ref ["drivers/staging/"]
let backport = ref false
let debug = ref false


let make_absolute path =
    if (Filename.is_relative path)
        then home ^ "/" ^ path
        else path

let get_dirs working_dir =
    (* Returns subdirectories use by the application *)
    let tmp_dir = working_dir ^ "/tmp" in
    let files_dir = working_dir ^ "/files" in
    let results_dir = working_dir ^ "/results" in
    (tmp_dir, files_dir, results_dir)

let read_to_file all ofile =
  let rec loop = function
      [] -> []
    | x::xs ->
	match Str.split (Str.regexp "[ \t]+") x with
	  ["CC";file] when file = ofile -> xs
	| _ -> loop xs in
  loop all

let error_warning_note all =
  let isint s = try let _ = int_of_string s in true with _ -> false in
  List.filter
    (function x ->
      match Str.split (Str.regexp ":") x with
	file::line::col::" error"::_
      | file::line::col::" warning"::_ when isint line && isint col -> true
      | _ -> false)
    all

let error_warning_note_reduced all = error_warning_note all (*
  let isint s = try let _ = int_of_string s in true with _ -> false in
  List.filter
    (function x ->
      match Str.split (Str.regexp ":") x with
	_::"Line"::line::" error"::_
      | _::"Line"::line::" warning"::_ when isint line ->
	  true
      | _ -> false)
    all *)

let to_ul s = String.concat "_" (Str.split (Str.regexp "/") s)

let debug_output output =
    if !debug
        then List.iter (function x -> Printf.eprintf "DEBUG: %s\n%!" x) output;
    output


let run_compile file =
    (* Try to compile file and get make output*)
    let ofile = (Filename.chop_extension file) ^ ".o" in
    Tools.cmd_to_list (Printf.sprintf "make %s 2>&1" ofile)


let filter_file_errors file make_output =
    (* Filter to keep only errors or warnings of file *)
    let ofile = (Filename.chop_extension file) ^ ".o" in
    let filtered_errors = read_to_file make_output ofile in
    let count = List.length (error_warning_note filtered_errors) in
    (count, filtered_errors)



let call_gcc_reduce res_chan =
    let buffer = ref [] in
    Read.read_input_strings "" buffer res_chan;
    let filtered_output = List.filter (fun x -> not (x = "")) !buffer in
    (* TODO: legacy code, rewrite with meaningful variable names *)
    let y = Types.errStructs_of_strings filtered_output in
    let err = Rules2.process_error_list ([],[]) y y in

    let error_types = List.map Binding.parse_gcc_reduce_err err in
    (*
    let z = List.map Generate.chosen_args z in

    let chosen_args = List.map (function err -> err.Types.chosen_args) z in
    *)
    let reduced = List.map (function err ->
        Str.split (Str.regexp_string "\n") err.Types.msg) err
    in

    (error_types, List.concat reduced)

let compile_test file commit =
    let tmp_dir, _, _ = get_dirs !work_dir in
    let resfile =
        Printf.sprintf "%s/%s_%s" tmp_dir (to_ul file) commit in
    let res, compiler_output =
        if Sys.file_exists resfile
        then
        (* Dump content of file into list *)
            let lines = ref [] in
            let res_chan = open_in resfile in
            try
                while true; do
                    lines := input_line res_chan :: !lines
                done; (1, List.rev !lines)
            with End_of_file ->
                close_in res_chan;
                (1, List.rev !lines)
        else
            let output =
                try
                    run_compile file
                with Tools.ProcessError(Unix.WEXITED(_), output) ->
                    output
            in
            let (res, lines) = filter_file_errors file (debug_output output) in
            if res > 0
                then begin
                    (* Dump content of compiler output into a resfile *)
                    let o = open_out resfile in
                    List.iter (function x -> Printf.fprintf o "%s\n" x) lines;
                    close_out o;
                    (res, lines)
                end
                else (0, [])
    in
    if res > 0
    then begin
        let res_chan = open_in resfile in
        let error_types, reduced = call_gcc_reduce res_chan in
        close_in res_chan;
        let count pattern =
            let matching_lines =
                List.filter (function line ->
                    Str.string_match (Str.regexp pattern) line 0)
                compiler_output
            in
            List.length matching_lines
        in
        let originalres =
            count ".*: error: "
            + count ".*: warning: "
            - count ".*(near initialization for "
        in
        let reducedres = List.length (error_warning_note_reduced reduced) in
        let message =
            Printf.sprintf "reduced res %d -> %d\n" originalres reducedres
        in
        ignore (debug_output [message]);
        if reducedres > 0
            then begin
                let resfile =
                    Printf.sprintf "%s/%s_myreduced_%s" tmp_dir (to_ul file)
                    commit
                in
                let o = open_out resfile in
                List.iter (function x -> Printf.fprintf o "%s\n" x) reduced;
                close_out o
            end;
            (res, reducedres, error_types)
    end
        else (0,0, [])

(* put all files in the _files directory, even the ones without errors, for
reference in the message reduction process *)
let pre_preparedir commit =
    let dir, resdir =
        let open Commits in
        let _, files_dir, results_dir = get_dirs !work_dir in
        let dir_name = Printf.sprintf "%s:%s" commit.hash commit.meta.date in
        (
            files_dir ^ "/" ^ dir_name,
            results_dir ^ "/" ^ dir_name
        )
    in
    (if not (Sys.file_exists dir) then
    begin
        let rec extract_chfiles prev = function
        | [] -> prev
        | {Commits.file_name=file; _}::tail
            when (Tools.is_c_file file || Tools.is_h_file file) -> extract_chfiles (file::prev) tail
        | head::tail -> extract_chfiles prev tail
        in
        let chfiles = extract_chfiles [] commit.Commits.files in

        Tools.create_dir dir false;
      (* copy the files into the current directory *)
      List.iter
	(function file ->
	  ignore
	    (Sys.command
	      (Printf.sprintf "git show %s:%s > %s/%s"
		 (if !backport then ("v" ^ !target) else commit.Commits.hash)
		 file dir (to_ul file)))) chfiles
	end);
    Tools.create_dir resdir false

let preparedir (commit, files) =
    let dir, resdir =
        let open Commits in
        let _, files_dir, results_dir = get_dirs !work_dir in
        let dir_name = Printf.sprintf "%s:%s" commit.hash commit.meta.date in
        (
            files_dir ^ "/" ^ dir_name,
            results_dir ^ "/" ^ dir_name
        )
    in
    let count = List.fold_left (fun prev (_,(n,_,_)) -> prev + n) 0 files in
    if count = 0
        then let _ = Sys.command ("/bin/rm -rf " ^ resdir) in ()
    else begin
      (* make patch queries *)
        List.iter (function (file,(n,_,error_types)) ->
            if n > 0
            then begin
                let cwd = Sys.getcwd () in
                let template_dir = home ^ "/templates" in
                let report_options =
                    (!backport, file, commit.Commits.hash, error_types)
                in
                Sys.chdir resdir;
                Report.do_report ("v" ^ !target)
                    !git template_dir report_options;
                Sys.chdir cwd;

                let create_error_tree i error_type =
                    (* Create directory tree to group patch by error type *)
                    let issue = Report.type_to_normalized_name error_type in
                    let hash = commit.Commits.hash in
                    let report_dir_name =
                        Printf.sprintf "%s_%s" (to_ul file) hash
                    in
                    let directory =
                        Printf.sprintf "%s/errors-by-type/%s/%s"
                            !work_dir issue report_dir_name
                    in
                    Tools.create_dir directory false;
                    let attrs = [Open_wronly; Open_append; Open_creat] in
                    let makefile =
                        open_out_gen attrs 0o644 (directory ^ "/Makefile")
                    in
                    (if not (Sys.file_exists (directory ^ "/results"))
                    then begin
                        let cmd = Printf.sprintf "ln -s %s/%s %s/results"
                            resdir report_dir_name directory
                        in
                        ignore (Sys.command cmd);
                        Printf.fprintf makefile "all:\n\t$(MAKE) -C results ";
                    end
                    );
                    Printf.fprintf makefile "step%d " (i+1);
                    close_out makefile;
                    let cocci_file = Printf.sprintf "step%d.cocci" (i+1) in
                    let cmd = Printf.sprintf "ln -s %s/%s/%s %s/%s"
                        resdir report_dir_name cocci_file directory cocci_file
                    in
                    ignore (Sys.command cmd)
                in
                List.iteri create_error_tree error_types;
            end
        ) files;
      (* make redo infrastructure *)
      let chfiles =
	List.filter (function file -> Tools.is_c_file file || Tools.is_h_file file)
	  (List.map fst files) in
      let cfiles = List.filter Tools.is_c_file chfiles in
      let hfiles = List.filter Tools.is_h_file chfiles in
      (* make the makefile *)
      let makefile = Printf.sprintf "%s/Makefile" dir in
      let o = open_out makefile in
      Printf.fprintf o "all:\n";
      Printf.fprintf o "\tcd %s; git clean -dfx > /dev/null 2>&1; \\\n" !git;
      Printf.fprintf o "\tgit reset --hard %s > /dev/null 2>&1; \\\n"
	(if !backport then commit.Commits.hash else ("v" ^ !target));
      Printf.fprintf o "\tmake allyesconfig > /dev/null 2>&1\n";
      List.iter
	(function file ->
	  if String.length file > 20
	  then Printf.fprintf o "\tcp %s \\\n\t%s/%s\n" (to_ul file) !git file
	  else Printf.fprintf o "\tcp %s %s/%s\n" (to_ul file) !git file)
	chfiles;
      Printf.fprintf o "\tcd %s; git diff --stat\n" !git;
      List.iter
	(function file ->
	  Printf.fprintf o "\tcd %s; make %s.o > /dev/null\n" !git
	    (Filename.chop_extension file))
	cfiles;
      Printf.fprintf o "\ncocci:\n";
      List.iter
	(function file ->
	  let (n,_,_) = List.assoc file files in
	  if n > 0
	  then
	    begin
	      Printf.fprintf o
		"\tfor i in `ls ../../*/%s/%s_%s/redo*cocci`; do \\\n"
		(Filename.basename resdir) (Filename.basename file) commit.Commits.hash;
	      Printf.fprintf o "\ttmp=$$(basename \"$$i\"); \\\n";
	      Printf.fprintf o "\techo \"virtual before\" > $${tmp}; \\\n";
	      Printf.fprintf o "\techo \"virtual after\" >> $${tmp}; \\\n";
	      Printf.fprintf o "\tcat $$i >> $${tmp}; \\\n";
	      Printf.fprintf o
		"\tsed s+fresh\\ identifier+symbol+g $${tmp} > $${tmp}.xx; \\\n";
	      Printf.fprintf o "\tmv $${tmp}.xx $${tmp}; \\\n";
	      Printf.fprintf o "\tspatch $${tmp} -D before --in-place . \\\n";
	      List.iter
		(function hfile ->
		  Printf.fprintf o "\t--include %s \\\n" (to_ul hfile))
		hfiles;
	      Printf.fprintf o "\t--include-headers-for-types; \\\n";
	      List.iter
		(function hfile ->
		  Printf.fprintf o
		    "\tspatch $${tmp} -D before --in-place %s; \\\n"
		    (to_ul hfile))
		hfiles;
	      Printf.fprintf o "\tdone\n"
	    end)
	cfiles;
      close_out o
    end


let keep_compiling commits =
    let try_compile total i commit =
        let rank = Parmap.get_rank () in
        (if ((Parmap.get_ncores ()) != 1)
            then Sys.chdir (giti rank)
        );

        Tools.git_setup commit.Commits.hash;

        if rank = 0
            then Tools.print_progress total i;

        try
            let is_compiling = List.for_all (fun file ->
                if Tools.is_c_file file.Commits.file_name
                    then
                    let output =
                        debug_output (run_compile file.Commits.file_name)
                    in
                    fst(filter_file_errors file.Commits.file_name output) = 0

                    else true
            ) commit.Commits.files
            in
            if is_compiling
                then [commit]
                else []
        with Tools.ProcessError(Unix.WEXITED(_), output) ->
            ignore(debug_output output);
            []

    in
    let total = List.length commits in
    let res =
        Parmap.parmapi (try_compile total) (Parmap.L(commits)) ~ncores:(!cores)
    in

    Printf.eprintf "\n%!";
    List.concat res


let compile total i commit =
    let rank = Parmap.get_rank () in
    (if ((Parmap.get_ncores ()) != 1)
        then Sys.chdir (giti rank)
    );

    if rank = 0
        then Tools.print_progress total i;

    (* Extract file name of files still existing in the commit *)
    let files = List.map (function a -> a.Commits.file_name)
        commit.Commits.files
    in

    let meta =
        let open Commits in
        (commit.hash ^ ":" ^ commit.meta.date ^ ":" ^ commit.meta.author)
    in

    let version = if !backport
        then commit.Commits.hash
        else ("v" ^ !target)
    in
    Tools.git_setup version;

    List.iter (function file ->
        let com = if !backport
            then ("v" ^ !target)
            else commit.Commits.hash
        in
        if Tools.is_c_file file || Tools.is_h_file file then
            (* Copy driver files into repository *)
            ignore (Sys.command
            (Printf.sprintf "git show %s:%s > %s" com file file))
    ) files;
    pre_preparedir commit;

    let compile_res =
        List.map (function file ->
            let ct =
                if Tools.is_c_file file
                    then compile_test file commit.Commits.hash
                    else (0,0,[])
            in
            (file, ct))
        files
    in
    let res = (commit, compile_res) in
    preparedir res;
    (meta, compile_res)


 let process l =
  let unsome =
    List.fold_left
      (function prev ->
	function (meta,info) ->
	  let (tot,totreduced,cs) =
	    List.fold_left
	      (fun (prev_orig,prev_reduced,cs)
		  (file,(ct,ctreduced,_)) ->
		if Tools.is_c_file file
		then (ct + prev_orig,ctreduced + prev_reduced,cs+1)
		else (prev_orig,prev_reduced,cs))
	      (0,0,0) info in
	  (tot, totreduced, meta, info) :: prev)
      [] l in
  let l = List.rev (List.sort compare unsome) in
  List.iter
    (function (tot,totreduced,meta,info) ->
      Printf.printf "%d -> %d: %s\n" tot totreduced meta;
      List.iter
	(function (file,(ct,ctreduced, _)) ->
	  Printf.printf "   %s: %d -> %d\n" file ct ctreduced)
	info)
    l;
  let _, _, results_dir = get_dirs !work_dir in
  let data =
    Tools.cmd_to_list
      (Printf.sprintf "cd %s; find . -name Makefile" results_dir) in
  let data =
    List.map (fun x -> List.hd (Str.split (Str.regexp "/Makefile") x)) data in
  let o = open_out (Printf.sprintf "%s/runall" results_dir) in
  List.iter (function dir -> Printf.fprintf o "cd %s ; make -j15 all ; cd ../..\n" dir)
    data;
  close_out o

(* ------------------------------------------------------------------------ *)

let options =
  ["--start", Arg.Set_string start_time, "starting time";
    "--end", Arg.Set_string end_time, "ending time";
    "--range", Arg.Set_string range, "commit range";
    "--list", Arg.String (fun x -> list := Str.split (Str.regexp ",") x),
       "commits, comma separated";
    "--target", Arg.Set_string target, "target directory";
    "--key", Arg.Set_string work_dir, "subdir for results";
    "--subsystem", Arg.String (fun x -> requirement := [x]),
    "targeted directory";
    "--cores", Arg.Set_int cores, "number of cores";
    "--git", Arg.Set_string git, "Linux source code";
    "--debug", Arg.Set debug, "Print debug informations";
    "--backport", Arg.Set backport, "backport from dest to src"]

let anonymous s = failwith "no anonymous arguments"

let usage = ""

let _ =
    Arg.parse (Arg.align options) anonymous usage;

    git := make_absolute !git;
    work_dir := make_absolute !work_dir;

    Tools.create_dir !work_dir true;
    let tmp_dir, files_dir, results_dir = get_dirs !work_dir in
    Tools.create_dir tmp_dir true;
    Tools.create_dir files_dir false;
    Tools.create_dir results_dir true;
    Tools.create_dir (!work_dir ^ "/errors-by-type") true;

    (* Clean the git repository *)
    Sys.chdir !git;
    Tools.git_setup "master";

    (* Fetch commit hash based on program arguments *)
    Printf.eprintf "Listing commits\n%!";
    let commits =
      if not (!list = [])
      then Commits.list_by_hash_list !list
      else if not (!range = "")
      then Commits.list_by_range !range
      else Commits.list_by_dates !start_time !end_time
    in
    Printf.eprintf "Found %d commits\n%!" (List.length commits);

    (* Transform commits hashes into structure with files and metadata *)
    let parsed = Commits.parse_commits commits in
    Printf.eprintf "Filtering commits\n%!";

    (* Filter commits to keep only those which add a driver *)
    let driver_add = Filters.keep_added !requirement !antirequirement parsed in
    Printf.eprintf "%d commits are adding a driver\n%!"
        (List.length driver_add);

    (* Filter commits to keep only those which files still exist *)
    let version = "v" ^ !target in
    let driver_exist = Filters.keep_existing version driver_add in
    Printf.eprintf "%d commits still have the same files in %s \n%!"
        (List.length driver_add) version;

    Printf.eprintf "Checking compilation in introduction commit version\n%!";
    let driver_compile = keep_compiling driver_exist in
    Printf.eprintf "%d/%d drivers compile in their original version\n%!"
        (List.length driver_compile) (List.length driver_add);

    (* Test compilation and apply gcc-reduce *)
    let res =
        Parmap.parmapi (compile (List.length driver_compile))
            (Parmap.L(driver_compile)) ~ncores:(!cores)
    in
    Printf.eprintf "\n%!";
    process res
