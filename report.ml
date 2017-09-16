type sp =
    UnknownFunction of string
  | UnknownFunction2 of string * string
  | UnknownVariable of string
  | UnknownType of string
  | UnknownTypedef of string
  | UnknownField of string * string
  | UnknownFieldOrType of string * string
  | UnknownFieldGeneric of string
  | Void1 of string
  | Void2 of string * string
  | TypeChange of string
  | TypeChange2 of string * string
  | BadInitType of string * string
  | BadNonfnInit of string * string
  | BadArgType of string * string
  | TooManyArgs of string
  | TooFewArgs of string
  | Unknown


let copy_file o file =
  let i = open_in file in
  let rec loop _ =
    let l = input_line i in
    Printf.fprintf o "%s\n" l;
    loop() in
  try loop() with _ -> (close_in i; ())

let sedsafe s =
  String.concat "\\ "
    (Str.split (Str.regexp " ")
       (String.concat "\\>" (Str.split (Str.regexp ">") s)))
let texsafe s = String.concat "\\_" (Str.split (Str.regexp "_") s)
let texsafe2 s = texsafe(texsafe(texsafe(texsafe s)))


let type_to_string = function
    (* Convert type to name *)
    | Unknown -> "Unknown"
    | UnknownFunction(_) -> "Unknown function"
    | UnknownFunction2(_, _) -> "Unknown function 2"
    | UnknownVariable(_) -> "Unknown constant"
    | UnknownType(_) -> "Unknown type"
    | UnknownTypedef(_) -> "Unknown typedef"
    | UnknownField(_, _) -> "Unknown field"
    | UnknownFieldOrType(_, _) -> "Unknown field or type"
    | UnknownFieldGeneric(_) -> "Unknown field generic"
    | Void1(_) -> "Void 1"
    | Void2(_, _) -> "Void 2"
    | TypeChange(_) -> "Type change"
    | TypeChange2(_, _) -> "Type change 2"
    | BadInitType(_, _) -> "Bad init type"
    | BadNonfnInit(_, _) -> "Bad nonfn init"
    | BadArgType(_, _) -> "Bad arg type"
    | TooManyArgs(_) -> "Too many args"
    | TooFewArgs(_) -> "Too few args"

let type_to_cocci_file error_type =
    (* Convert type to coccinelle file name *)
    let error_string = type_to_string error_type in
    let lowercase = String.lowercase error_string in
    let basename = Str.global_replace (Str.regexp_string " ") "_" lowercase in
    basename ^ ".cocci"

let msg error_type = match error_type with
  (* Convert type to error message *)
    | Unknown -> type_to_string error_type

    | UnknownFunction(arg)
    | UnknownVariable(arg)
    | UnknownType(arg)
    | UnknownTypedef(arg)
    | UnknownFieldGeneric(arg)
    | Void1(arg)
    | TypeChange(arg)
    | TooManyArgs(arg)
    | TooFewArgs(arg) -> Printf.sprintf "%s: %s" (type_to_string error_type) arg

    | UnknownField(arg1, arg2)
    | UnknownFieldOrType(arg1, arg2)
    | Void2(arg1, arg2)
    | TypeChange2(arg1, arg2)
    | BadInitType(arg1, arg2)
    | BadNonfnInit(arg1, arg2)
    | BadArgType(arg1, arg2) ->
        Printf.sprintf "%s: %s.%s" (type_to_string error_type) arg1 arg2

    | UnknownFunction2(arg1, arg2) ->
        Printf.sprintf "%s: %s/%s" (type_to_string error_type) arg1 arg2



let to_ul s = String.concat "_" (Str.split (Str.regexp "/") s)

let do_report target linux tdir options =
  let backport, file, commit, args = options in
  (* make the directory *)
  let dir = (to_ul file) ^ "_" ^ commit in
  (if Sys.file_exists dir then failwith (dir^" already exists"));
  let _ = Sys.command ("mkdir "^dir) in
  (* put the semantic patches in the directory *)
  let replace k v = Printf.sprintf "sed s/%s/%s/g" k (sedsafe v) in
  let extra1 =
    if backport then "| sed s/^-/%/ | sed s/^+/-/ | sed s/^%/+/ " else "" in
  let extra2 =
    if backport
    then "| sed s/before/%/ | sed s/after/before/ | sed s/%/after/"
    else "" in
  let extra = extra1 ^ extra2 in
  List.iteri
    (function i ->
      let i = i + 1 in
      function arg ->
	let cmd =
	  match arg with
	    Unknown ->
	      Printf.eprintf "preparing Unknown\n";
	      Printf.sprintf "echo TODO > %s/step%d.cocci" dir i
	  | UnknownFunction(fn) ->
	      Printf.eprintf "preparing UnknownFunction\n";
	      Printf.sprintf
		"cat %s/unknown_function.cocci %s | %s > %s/step%d.cocci"
		 tdir extra (replace "FN" fn) dir i
	  | UnknownFunction2(fn,alt) ->
	      Printf.eprintf "preparing UnknownFunction\n";
	      Printf.sprintf
		"cat %s/unknown_function.cocci %s | %s | %s > %s/step%d.cocci"
		tdir extra (replace "FN" fn) (replace "ALT" alt) dir i
	  | UnknownVariable(cst) ->
	      Printf.eprintf "preparing UnknownVariable\n";
	      Printf.sprintf
	      "cat %s/unknown_variable.cocci %s | %s > %s/step%d.cocci"
		tdir extra (replace "CST" cst) dir i
	  | UnknownType(ty) ->
	      Printf.eprintf "preparing UnknownType\n";
	      Printf.sprintf
		"cat %s/unknown_type.cocci %s | %s > %s/step%d.cocci"
		tdir extra (replace "TY" ty) dir i
	  | UnknownTypedef(ty) ->
	      Printf.eprintf "preparing UnknownTypedef\n";
	      Printf.sprintf
		"cat %s/unknown_typedef.cocci %s | %s > %s/step%d.cocci"
		tdir extra (replace "TY" ty) dir i
	  | UnknownField(str,fld) ->
	      Printf.eprintf "preparing UnknownField\n";
	      Printf.sprintf
		"cat %s/unknown_field.cocci %s | %s | %s > %s/step%d.cocci"
		tdir extra (replace "STR" str) (replace "FLD" fld) dir i
	  | UnknownFieldOrType(str,fld) ->
	      Printf.eprintf "preparing UnknownFieldOrType\n";
	      Printf.sprintf
		"cat %s/unknown_field_or_type.cocci %s | %s | %s > %s/step%d.cocci"
		tdir extra (replace "STR" str) (replace "FLD" fld) dir i
	  | UnknownFieldGeneric(fld) ->
	      Printf.eprintf "preparing UnknownFieldGeneric\n";
	      Printf.sprintf
		"cat %s/unknown_field_generic.cocci %s | %s > %s/step%d.cocci"
		tdir extra (replace "FLD" fld) dir i
	  | Void1(exp) ->
	      Printf.eprintf "preparing Void\n";
	      Printf.sprintf
		"cat %s/void1.cocci %s | %s > %s/step%d.cocci"
		tdir extra (replace "EXP" exp) dir i
	  | Void2(str,fld) ->
	      Printf.eprintf "preparing Void\n";
	      Printf.sprintf
		"cat %s/void2.cocci %s | %s | %s > %s/step%d.cocci"
		tdir extra (replace "STR" str) (replace "FLD" fld) dir i
	  | TypeChange(exp) ->
	      Printf.eprintf "preparing TypeChange\n";
	      Printf.sprintf
		"cat %s/typechange.cocci %s | %s > %s/step%d.cocci"
		tdir extra (replace "EXP" exp) dir i
	  | TypeChange2(str,fld) ->
	      Printf.eprintf "preparing TypeChange\n";
	      Printf.sprintf
		"cat %s/typechange.cocci %s | %s | %s > %s/step%d.cocci"
		tdir extra (replace "STR" str) (replace "FLD" fld) dir i
	  | BadInitType(str,fld) ->
	      Printf.eprintf "preparing BadInitType\n";
	      Printf.sprintf
		"cat %s/bad_init_type.cocci %s | %s | %s > %s/step%d.cocci"
		tdir extra (replace "STR" str) (replace "FLD" fld) dir i
	  | BadNonfnInit(str,fld) ->
	      Printf.eprintf "preparing BadNonfnInit\n";
	      Printf.sprintf
		"cat %s/bad_nonfn_init.cocci %s | %s | %s > %s/step%d.cocci"
		tdir extra (replace "STR" str) (replace "FLD" fld) dir i
	  | BadArgType(fn,argn) ->
	      let argn = string_of_int(int_of_string argn - 1) in
	      Printf.eprintf "preparing BadArgType\n";
	      Printf.sprintf
		"cat %s/bad_arg_type.cocci %s | %s | %s > %s/step%d.cocci"
		tdir extra (replace "FN" fn) (replace "ARG" argn) dir i
	  | TooManyArgs(fn) ->
	      Printf.eprintf "preparing TooManyArgs\n";
	      Printf.sprintf
		"cat %s/too_many_args.cocci %s | %s > %s/step%d.cocci"
		tdir extra (replace "FN" fn) dir i
	  | TooFewArgs(fn) ->
	      Printf.eprintf "preparing TooFewArgs\n";
	      Printf.sprintf
		"cat %s/too_few_args.cocci %s | %s > %s/step%d.cocci"
		tdir extra (replace "FN" fn) dir i in
	let _ = Sys.command cmd in
	())
    args;
  (* put the original file in the directory *)
  let self = Sys.getcwd() in
  let cmd =
    Printf.sprintf "cd %s; git show %s:%s > %s/%s/%s" linux commit file
      self dir (Filename.basename file) in
  let _ = Sys.command cmd in
  let cmd =
    Printf.sprintf "cd %s; git show %s:%s > %s/%s/%s_%s" linux
      target file self dir (Filename.basename file) target in
  let _ = Sys.command cmd in
  (* put the Makefile in the directory *)
  let mout = dir^"/Makefile" in
  let temp = Sys.getenv "USER" in
  let cmd =
    Printf.sprintf
      "sed s/COMMIT/%s/g %s/Makefile | sed s+FILE+%s+g | sed s+NAME+%s+g > %s"
      commit tdir file temp mout in
  let _ = Sys.command cmd in
  let o = open_out_gen [Open_text; Open_append] 0o640 mout in
  (match args with
    [] ->
      Printf.fprintf o "xall: step1\n";
      Printf.fprintf o "all: step1\n\n";
      Printf.fprintf o "step1: step1.cocci\n";
      Printf.fprintf o "\t$(PREQUEL) --sp step1.cocci $(ARGS)\n"
  | _ ->
      Printf.fprintf o "xall:";
      List.iteri (fun i _ -> Printf.fprintf o " step%d" (i+1)) args;
      Printf.fprintf o "\n";
      Printf.fprintf o "all:";
      List.iteri
	(fun i a -> if not (a = Unknown) then Printf.fprintf o " step%d" (i+1))
	args;
      Printf.fprintf o "\n";
      List.iteri
	(fun i arg ->
	  let i = i + 1 in
	  Printf.fprintf o "\nstep%d: step%d.cocci\n" i i;
	  Printf.fprintf o "\t$(PREQUEL) --sp step%d.cocci $(ARGS) \\\n" i;
	  Printf.fprintf o "\t--msg \"%s\"\n" (msg arg))
	args);
  (match args with
    [] ->
      Printf.fprintf o "\nrall: redo1\n\n";
      Printf.fprintf o "redo1: redo1.cocci\n";
      Printf.fprintf o
	"\t$(PREQUEL) --sp redo1.cocci $(RARGS) --gitgcompare ???\n"
  | _ ->
      Printf.fprintf o "\nrall:";
      List.iteri (fun i _ -> Printf.fprintf o " redo%d" (i+1)) args;
      Printf.fprintf o "\n";
      List.iteri
	(fun i arg ->
	  let i = i + 1 in
	  Printf.fprintf o "\nredo%d: redo%d.cocci\n" i i;
	  let kwds =
	    match arg with
	      Unknown -> ["--TODO"]
	    | UnknownFunction(fn) -> ["--gitscompare "^fn]
	    | UnknownFunction2(fn,alt) ->
		["--gitscompare "^fn;"--gitscompare "^alt]
	    | UnknownVariable(cst) -> ["--gitscompare "^cst]
	    | UnknownType(ty) ->
		let ty = List.hd(List.rev(Str.split (Str.regexp " ") ty)) in
		["--gitscompare "^ty]
	    | UnknownTypedef(ty) ->
		let ty = List.hd(List.rev(Str.split (Str.regexp " ") ty)) in
		["--gitscompare "^ty]
	    | UnknownField(_,fld) | UnknownFieldGeneric(fld) ->
		["--gitscompare "^fld]
	    | UnknownFieldOrType(ty,fld) ->
		let ty = List.hd(List.rev(Str.split (Str.regexp " ") ty)) in
		["--gitscompare "^fld;"--gitscompare "^ty]
	    | Void1(exp) -> ["--gitscompare "^exp]
	    | Void2(str,fld) -> ["--gitscompare "^fld]
	    | TypeChange(exp) -> ["--gitscompare "^exp]
	    | TypeChange2(str,fld) -> ["--gitscompare "^fld]
	    | BadInitType(str,fld) -> ["--UNDOABLE"]
	    | BadNonfnInit(str,fld) -> ["--UNDOABLE"]
	    | BadArgType(str,fld) -> ["--UNDOABLE"]
	    | TooManyArgs(fn) -> ["--gitgcompare "^fn]
	    | TooFewArgs(fn) -> ["--gitgcompare "^fn] in
	  List.iter
	    (function kwd ->
	      if String.length kwd > 30
	      then
		Printf.fprintf o
		  "\t$(PREQUEL) --sp redo%d.cocci $(RARGS) \\\n %s \\\n"
		  i kwd
	      else
		Printf.fprintf o
		  "\t$(PREQUEL) --sp redo%d.cocci $(RARGS) %s \\\n"
		  i kwd)
	    kwds;
	  Printf.fprintf o "\t--msg \"%s\"\n" (msg arg))
	args);
  close_out o;
  (* put the report in the directory *)
  let rout = dir^"/report.tex" in
  let cmd =
    Printf.sprintf "sed s/COMMIT/%s/g %s/report.tex | sed s+FILE+%s+g > %s"
      commit tdir (texsafe2 file) rout in
  let _ = Sys.command cmd in
  let o = open_out_gen [Open_text; Open_append] 0o640 rout in
  (match args with
    [] ->
      Printf.fprintf o "\n\\section{}\n\n";
      Printf.fprintf o "\\begin{quote}\n";
      Printf.fprintf o "\\begin{lstlisting}[language=diff]\n";
      Printf.fprintf o "\\end{lstlisting}\n";
      Printf.fprintf o "\\end{quote}\n";
      Printf.fprintf o "\\end{document}\n"
  | _ ->
      let txt = Filename.temp_file "txt" "txt" in
      List.iteri
	(fun i arg ->
	  let i = i + 1 in
	  let sec =
	    match arg with
	      Unknown -> "Unknown"
	    | UnknownFunction(fn) -> texsafe fn
	    | UnknownFunction2(fn,alt) -> texsafe fn ^ "." ^ texsafe alt
	    | UnknownVariable(cst) -> texsafe cst
	    | UnknownType(ty) -> texsafe ty
	    | UnknownTypedef(ty) -> texsafe ty
	    | UnknownField(str,fld)
	    | UnknownFieldOrType(str,fld) -> texsafe str ^ "." ^ texsafe fld
	    | UnknownFieldGeneric(fld) -> texsafe fld
	    | Void1(exp) -> texsafe exp
	    | Void2(str,fld) -> texsafe str ^ "." ^ texsafe fld
	    | TypeChange(exp) -> texsafe exp
	    | TypeChange2(str,fld) -> texsafe str ^ "." ^ texsafe fld
	    | BadInitType(str,fld) -> texsafe str ^ "." ^ texsafe fld
	    | BadNonfnInit(str,fld) -> texsafe str ^ "." ^ texsafe fld
	    | BadArgType(fn,argn) -> texsafe fn ^ ", arg " ^ texsafe argn
	    | TooManyArgs(fn) -> texsafe fn
	    | TooFewArgs(fn) -> texsafe fn in
	  Printf.fprintf o "\n\\section{%s}\n\n" sec;
	  let cmd =
	    match arg with
	      Unknown -> ""
	    | UnknownFunction(fn) ->
		Printf.sprintf "%s %s/unknown_function.txt | %s > %s"
		  (replace "FN" (texsafe2 fn)) tdir
		  (replace "NUM" (string_of_int i)) txt
	    | UnknownFunction2(fn,alt) ->
		Printf.sprintf "%s %s/unknown_function2.txt | %s | %s > %s"
		  (replace "FN" (texsafe2 fn)) tdir
		  (replace "ALT" (texsafe2 alt))
		  (replace "NUM" (string_of_int i)) txt
	    | UnknownVariable(cst) ->
		Printf.sprintf "%s %s/unknown_variable.txt | %s > %s"
		  (replace "CST" (texsafe2 cst)) tdir
		  (replace "NUM" (string_of_int i)) txt
	    | UnknownType(ty) ->
		Printf.sprintf "%s %s/unknown_type.txt | %s > %s"
		  (replace "TY" (texsafe2 ty)) tdir
		  (replace "NUM" (string_of_int i)) txt
	    | UnknownTypedef(ty) ->
		Printf.sprintf "%s %s/unknown_type.txt | %s > %s"
		  (replace "TY" (texsafe2 ty)) tdir
		  (replace "NUM" (string_of_int i)) txt
	    | UnknownField(str,fld) ->
		Printf.sprintf "%s %s/unknown_field.txt | %s | %s > %s"
		  (replace "STR" (texsafe2 str)) tdir
		  (replace "FLD" (texsafe2 fld))
		  (replace "NUM" (string_of_int i)) txt
	    | UnknownFieldOrType(str,fld) ->
		Printf.sprintf "%s %s/unknown_field_or_type.txt | %s | %s > %s"
		  (replace "STR" (texsafe2 str)) tdir
		  (replace "FLD" (texsafe2 fld))
		  (replace "NUM" (string_of_int i)) txt
	    | UnknownFieldGeneric(fld) ->
		Printf.sprintf "%s %s/unknown_field_generic.txt | %s > %s"
		  (replace "FLD" (texsafe2 fld)) tdir
		  (replace "NUM" (string_of_int i)) txt
	    | Void1(exp) ->
		Printf.sprintf "%s %s/void1.txt | %s > %s"
		  (replace "EXP" (texsafe2 exp)) tdir
		  (replace "NUM" (string_of_int i)) txt
	    | Void2(str,fld) ->
		Printf.sprintf "%s %s/void2.txt | %s | %s > %s"
		  (replace "STR" (texsafe2 str)) tdir
		  (replace "FLD" (texsafe2 fld))
		  (replace "NUM" (string_of_int i)) txt
	    | TypeChange(exp) ->
		Printf.sprintf "%s %s/typechange.txt | %s > %s"
		  (replace "EXP" (texsafe2 exp)) tdir
		  (replace "NUM" (string_of_int i)) txt
	    | TypeChange2(str,fld) ->
		Printf.sprintf "%s %s/typechange2.txt | %s | %s > %s"
		  (replace "STR" (texsafe2 str)) tdir
		  (replace "FLD" (texsafe2 fld))
		  (replace "NUM" (string_of_int i)) txt
	    | BadInitType(str,fld) ->
		Printf.sprintf "%s %s/bad_init_type.txt | %s | %s > %s"
		  (replace "STR" (texsafe2 str)) tdir
		  (replace "FLD" (texsafe2 fld))
		  (replace "NUM" (string_of_int i)) txt
	    | BadNonfnInit(str,fld) ->
		Printf.sprintf "%s %s/bad_nonfn_init.txt | %s | %s > %s"
		  (replace "STR" (texsafe2 str)) tdir
		  (replace "FLD" (texsafe2 fld))
		  (replace "NUM" (string_of_int i)) txt
	    | BadArgType(fn,argn) ->
		Printf.sprintf "%s %s/bad_arg_type.txt | %s | %s > %s"
		  (replace "FN" (texsafe2 fn)) tdir
		  (replace "ARG" (texsafe2 argn))
		  (replace "NUM" (string_of_int i)) txt
	    | TooManyArgs(fn) ->
		Printf.sprintf "%s %s/too_many_args.txt | %s > %s"
		  (replace "FN" (texsafe2 fn)) tdir
		  (replace "NUM" (string_of_int i)) txt
	    | TooFewArgs(fn) ->
		Printf.sprintf "%s %s/too_few_args.txt | %s > %s"
		  (replace "FN" (texsafe2 fn)) tdir
		  (replace "NUM" (string_of_int i)) txt in
	  (if not(cmd = "")
	  then let _ = Sys.command cmd in copy_file o txt);
	  Printf.fprintf o "\n\\begin{quote}\n";
	  Printf.fprintf o "\\begin{lstlisting}[language=diff]\n";
	  copy_file o (Printf.sprintf "%s/step%d.cocci" dir i);
	  Printf.fprintf o "\\end{lstlisting}\n";
	  Printf.fprintf o "\\end{quote}\n")
	args;
      let _ = Sys.command ("/bin/rm -f "^txt) in
      Printf.fprintf o "\n\nINCOMPLETE (remove when finished)\n";
      Printf.fprintf o "\\end{document}\n");
  close_out o
