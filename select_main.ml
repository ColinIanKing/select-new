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

type commit = Commits.commit

let git = ref "/run/shm/linux"
let giti i = Printf.sprintf "%s%d" !git i
let home = Filename.dirname (Array.get Sys.argv 0)
let target = ref "4.6"
let reference = ref ""
let cores = ref 22
let start_time = ref "Jan 1, 2015"
let end_time = ref "Dec 31, 2015"
let range = ref ""
let list = ref []
let key = ref "2015"
let requirement = ref ["drivers/"]
let antirequirement = ref ["drivers/staging/"]
let avg = ref false
let cc_count = ref false
let backport = ref false

let c_file file = Filename.check_suffix file ".c"
let h_file file = Filename.check_suffix file ".h"

let check_command s =
  let dir = Sys.getcwd() in
  let res =
    Sys.command (Printf.sprintf "%s > err%s 2>&1" s (Filename.basename dir)) in
  if not (res = 0)
  then failwith (Printf.sprintf "failure on %s in %s" s dir)

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

let run_compile file =
    (* Try compiling file and display make output to stderr 
     * Returns (number_of_errors, first lines of error on file) *)
  let ofile = (Filename.chop_extension file) ^ ".o" in
  let all = Tools.cmd_to_list (Printf.sprintf "make %s 2>&1" ofile) in
  List.iter (function x -> Printf.eprintf "-> %s\n" x) all;
  let all = read_to_file all ofile in
  let res = List.length (error_warning_note all) in
  (res,all)

let compile_test file commit =
  let resfile =
    Printf.sprintf "%s/%s/%s_%s" home !key (to_ul file) commit in
  let res =
    if Sys.file_exists resfile
    then 1
    else
      let (res,all) = run_compile file in
      if res > 0
      then
	begin
	  let o = open_out resfile in
	  List.iter (function x -> Printf.fprintf o "%s\n" x) all;
	  close_out o;
	  res
	end
      else res in
  if res > 0
  then
    begin

      (*let cmd =
	Printf.sprintf "%s/process --sp %s --linux %s/ %s" home home !git
	  resfile in*)
      let cmd =
	Printf.sprintf "%s/../../../gcc-reduce/gcc-reduce %s" home resfile in
      Printf.eprintf "cmd %s\n" cmd; flush stderr;
      let reduced = Tools.cmd_to_list cmd in
      let (chosen_args,reduced) =
	let (ca,red) =
	  List.fold_left
	    (fun (ca,red) l ->
	      match Str.split_delim (Str.regexp "chosen args: ") l with
		["";l] -> (l::ca,red)
	      | [l] -> (ca,l::red)
	      | _ -> failwith "unexpected chosen args")
	    ([],[]) reduced in
	(List.rev ca,List.rev red) in
      let originalres =
	int_of_string
	  (List.hd
	     (Tools.cmd_to_list
		(Printf.sprintf "grep -c \": error: \" %s" resfile))) +
	int_of_string
	  (List.hd
	     (Tools.cmd_to_list
		(Printf.sprintf "grep -c \": warning: \" %s" resfile))) -
	int_of_string
	  (List.hd
	     (Tools.cmd_to_list
		(Printf.sprintf "grep -c \"(near initialization for \" %s"
		   resfile))) in
      let reducedres = List.length (error_warning_note_reduced reduced) in
      Printf.eprintf "reduced res %d -> %d\n" originalres reducedres;
      flush stderr;
      (if reducedres > 0
      then
	begin
	  let resfile =
	    Printf.sprintf "%s/%s/%s_myreduced_%s" home !key (to_ul file)
	      commit in
	  let o = open_out resfile in
	  List.iter (function x -> Printf.fprintf o "%s\n" x) reduced;
	  close_out o
	end);
      (res,reducedres,String.concat " " chosen_args)
    end
  else (0,0,"")

(* put all files in the _files directory, even the ones without errors, for
reference in the message reduction process *)
let pre_preparedir (meta,files) =
  let (commit,dir,resdir) =
    match Str.split (Str.regexp ":") meta with
      commit::date::_ ->
	(commit,Printf.sprintf "%s/%s_files/%s:%s" home !key commit date,
	 Printf.sprintf "%s/%s_results/%s:%s" home !key commit date)
    | _ -> failwith "bad metadata" in
  (if not (Sys.file_exists dir)
  then
    begin
      let chfiles =
	List.filter (function file -> c_file file || h_file file) files in
      let _ = Sys.command ("mkdir -p "^dir) in
      (* copy the files into the current directory *)
      List.iter
	(function file ->
	  ignore
	    (Sys.command
	      (Printf.sprintf "git show %s:%s > %s/%s"
		 (if !backport then ("v" ^ !target) else commit)
		 file dir (to_ul file))))
	chfiles
    end);
  (if not (Sys.file_exists resdir)
  then
    let _ = Sys.command ("mkdir -p "^resdir) in
    ()
  else
    let _ = Sys.command ("/bin/rm -rf "^resdir^"/*") in
    ())

let preparedir (meta,files) =
  let (commit,dir,resdir) =
    match Str.split (Str.regexp ":") meta with
      commit::date::_ ->
	(commit,Printf.sprintf "%s/%s_files/%s:%s" home !key commit date,
	 Printf.sprintf "%s/%s_results/%s:%s" home !key commit date)
    | _ -> failwith "bad metadata" in
  let count = List.fold_left (fun prev (_,(n,_,_)) -> prev + n) 0 files in
  if count = 0
  then let _ = Sys.command ("/bin/rm -rf "^resdir) in ()
  else
    begin
      (* make patch queries *)
      List.iter
	(function (file,(n,_,chosen_args)) ->
	  if n > 0
	  then
	    begin
	      let cmd =
		Printf.sprintf "cd %s; %s/report %s %s %s %s"
		  resdir home (if !backport then "--backport" else "")
		  file commit chosen_args in
	      let _ = Sys.command cmd in
	      ()
	    end)
	files;
      (* make redo infrastructure *)
      let chfiles =
	List.filter (function file -> c_file file || h_file file)
	  (List.map fst files) in
      let cfiles = List.filter c_file chfiles in
      let hfiles = List.filter h_file chfiles in
      (* make the makefile *)
      let makefile = Printf.sprintf "%s/Makefile" dir in
      let o = open_out makefile in
      Printf.fprintf o "all:\n";
      Printf.fprintf o "\tcd %s; git clean -dfx > /dev/null 2>&1; \\\n" !git;
      Printf.fprintf o "\tgit reset --hard %s > /dev/null 2>&1; \\\n"
	(if !backport then commit else ("v" ^ !target));
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
		(Filename.basename resdir) (Filename.basename file) commit;
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

let git_setup version =
  check_command "git clean -dfx";
  check_command ("git reset --hard "^version);
  check_command "make allyesconfig"

let select_added commits i _ =
  (if !cores > 1 then Sys.chdir (giti i));
  List.concat
    (List.mapi
       (fun j commit ->
	 if i = j mod !cores
	 then
	   let meta =
	     Printf.sprintf
            (* format: short hash:commit date:author name *)
	       "git show %s --date=short --pretty=format:\"%%h:%%cd:%%an\""
	       commit.Commits.hash in
	   let is_driver_creation =
        (* Checks if the commit introduce a new driver
         * Implementation of conditions precised at the start of this file
         *)
	    List.exists
	    ( function a -> match a with
        | {Commits.file_name=file_name;
           Commits.modification=Commits.Created}
            when (
                c_file file_name &&
                Tools.is_file_in_paths file_name !requirement &&
                not (Tools.is_file_in_paths file_name !antirequirement)
            ) -> true
        | _ -> false
        ) commit.Commits.files &&

	    List.for_all
        ( function a -> match a with
        | {Commits.file_name=file_name;
           Commits.modification=Commits.Created}
            when (
                (c_file file_name &&
                Tools.is_file_in_paths file_name !requirement &&
                not (Tools.is_file_in_paths file_name !antirequirement)
                )
            ) -> true
        | {Commits.file_name=file_name;
           Commits.modification=Commits.AddOnly}
            when (
                (c_file file_name &&
                not (Tools.is_file_in_paths file_name !requirement &&
                    not (Tools.is_file_in_paths file_name !antirequirement))
                )
            ) -> true
        | {Commits.file_name=file_name;
           Commits.modification= Commits.Created | Commits.AddOnly}
            when (h_file file_name) -> true
        | {Commits.file_name=file_name; _ }
            when not (
                (c_file file_name) ||
                (h_file file_name)
            ) -> true
        | _ -> false
        ) commit.Commits.files &&

	    List.exists
	    ( function a -> match a with
        | {Commits.file_name=file_name;
           Commits.modification= Commits.Created | Commits.AddOnly}
            when (
                List.mem (Filename.basename file_name) ["Makefile";"Kconfig";"Kbuild"]
            ) -> true
        | _ -> false
        ) commit.Commits.files
        in

	   if is_driver_creation
	   then
	     begin
             (* Extract file name of files still existing in the commit *)
             let files = List.map (function a -> a.Commits.file_name) commit.Commits.files in
             (* Check if files still exist in the target directory *)
	       let ok =
		 List.for_all
		   (function file -> Sys.file_exists (!reference^"/"^file))
		   files in
	       let ok =
		 ok &&
		 ((not !cc_count) ||
		  (* check compilation in the old version *)
		  (git_setup commit.Commits.hash;
		   List.for_all
		     (fun file ->
                (* No compilation errors must be present *)
		       not (c_file file) || (fst(run_compile file)) = 0)
		     files)) in
	       if ok
	       then
		 begin
		   (if !cc_count
		   then
		     begin
		       git_setup
			 (if !backport then commit.Commits.hash else ("v" ^ !target));
		       List.iter
			 (function file ->
			   let com =
			     if !backport then ("v" ^ !target) else commit.Commits.hash in
			   if c_file file || h_file file
			   then
			     let _ =
			       Sys.command
				 (Printf.sprintf "git show %s:%s > %s"
				    com file file) in
			     ())
			 files
		     end);
		   let meta = List.hd (Tools.cmd_to_list meta) in
		   (if !cc_count then pre_preparedir (meta,files));
		   let res =
		     (meta,
		       (List.map
			  (function file ->
			    let ct =
			      if !cc_count
			      then
				if c_file file
				then compile_test file commit.Commits.hash
				else (0,0,"")
			      else (* count commits *)
				let cmd =
				  Printf.sprintf
				    "git log --oneline %s..v%s -- %s | wc -l"
				    commit.Commits.hash !target file in
				(int_of_string(List.hd(Tools.cmd_to_list cmd)),
				 0, "") in
			    (file,ct))
			  files)) in
		   (if !cc_count then preparedir res);
		   [res]
		 end
	       else []
	     end
	   else []
	 else [])
       commits)

let process l =
  let unsome =
    List.fold_left
      (function prev ->
	function (meta,info) ->
	  let (tot,totreduced,cs) =
	    List.fold_left
	      (fun (prev_orig,prev_reduced,cs)
		  (file,(ct,ctreduced,chosen_args)) ->
		if c_file file
		then (ct + prev_orig,ctreduced + prev_reduced,cs+1)
		else (prev_orig,prev_reduced,cs))
	      (0,0,0) info in
	  ((if !avg then (tot/cs) else tot),
	   (if !avg then (totreduced/cs) else totreduced),
	   meta,info) :: prev)
      [] l in
  let l = List.rev (List.sort compare unsome) in
  List.iter
    (function (tot,totreduced,meta,info) ->
      Printf.printf "%d -> %d: %s\n" tot totreduced meta;
      List.iter
	(function (file,(ct,ctreduced,chosen_args)) ->
	  Printf.printf "   %s: %d -> %d\n" file ct ctreduced)
	info)
    l;
  let data =
    Tools.cmd_to_list
      (Printf.sprintf "cd %s/%s_results; find . -name Makefile" home !key) in
  let data =
    List.map (fun x -> List.hd (Str.split (Str.regexp "/Makefile") x)) data in
  let o = open_out (Printf.sprintf "%s/%s_results/runall" home !key) in
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
    "--key", Arg.Set_string key, "subdir for results";
    "--subsystem", Arg.String (fun x -> requirement := [x]),
    "targeted directory";
    "--avg", Arg.Set avg, "order by avg modifs";
    "--cc-count", Arg.Set cc_count, "order by number of compiler output lines";
    "--cores", Arg.Set_int cores, "number of cores";
    "--git", Arg.Set_string git, "Linux source code";
    "--backport", Arg.Set backport, "backport from dest to src"]

let anonymous s = failwith "no anonymous arguments"

let usage = ""

let _ =
  Arg.parse (Arg.align options) anonymous usage;

  (* Get first directory for target 
   * TODO: catch exception if locate fails 
   * Maybe use --limit=1 *)
  reference := List.hd (Tools.cmd_to_list ("locate /linux-" ^ !target));
  let dir = Printf.sprintf "%s/%s" home !key in
  let _ =
      (* Create a working directory, purge the content if the dir exist
       * TODO: check that dir is a directory
       * Remove error message if dir already empty *)
    if Sys.file_exists dir
    then Sys.command (Printf.sprintf "/bin/rm %s/*" dir)
    else Sys.command (Printf.sprintf "mkdir %s" dir) in

  Sys.chdir !git;

  Printf.eprintf "Listing commits\n%!";
  let commits =
      if not (!list = [])
      then !list
      else if not (!range = "")
      then Commits.list_by_range !range
      else Commits.list_by_dates !start_time !end_time
  in
  Printf.eprintf "Found %d commits\n%!" (List.length commits);

  let parsed = Commits.parse_commits commits in
  Printf.eprintf "Filtering commits\n%!";
  let res =
    if !cores = 1
    then [select_added parsed 0 ()]
    else
      Parmap.parmapi (select_added parsed) ~ncores:(!cores)
	(Parmap.L (Array.to_list (Array.make !cores ()))) in
  process (List.concat res)
