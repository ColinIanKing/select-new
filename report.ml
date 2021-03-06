let debug = ref false

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
    | UnknownFunction2(_, _) -> "Unknown function"
    | UnknownVariable(_) -> "Unknown variable"
    | UnknownType(_) -> "Unknown type"
    | UnknownTypedef(_) -> "Unknown typedef"
    | UnknownField(_, _) -> "Unknown field"
    | UnknownFieldOrType(_, _) -> "Unknown field or type"
    | UnknownFieldGeneric(_) -> "Unknown field generic"
    | Void1(_) -> "Void1"
    | Void2(_, _) -> "Void2"
    | TypeChange(_) -> "Type change"
    | TypeChange2(_, _) -> "Type change"
    | BadInitType(_, _) -> "Bad init type"
    | BadNonfnInit(_, _) -> "Bad nonfn init"
    | BadArgType(_, _) -> "Bad arg type"
    | TooManyArgs(_) -> "Too many args"
    | TooFewArgs(_) -> "Too few args"

let type_to_normalized_name error_type =
    let error_string = type_to_string error_type in
    let lowercase = String.lowercase error_string in
    Str.global_replace (Str.regexp_string " ") "_" lowercase

let type_to_cocci_file error_type =
    (* Convert type to coccinelle file name *)
    (type_to_normalized_name error_type) ^ ".cocci"

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
	  (if !debug
	  then Printf.eprintf "DEBUG: Preparing %s\n" (type_to_string arg);
	  );
      let cocci_file = type_to_cocci_file arg in
	  match arg with
      | Unknown ->
          Printf.sprintf "echo TODO > %s/step%d.cocci" dir i

      | UnknownFunction(fn)
      | TooManyArgs(fn)
      | TooFewArgs(fn) ->
          Printf.sprintf "cat %s/%s %s | %s > %s/step%d.cocci"
          tdir cocci_file extra (replace "FN" fn) dir i

      | UnknownVariable(cst) ->
          Printf.sprintf "cat %s/%s %s | %s > %s/step%d.cocci"
          tdir cocci_file extra (replace "CST" cst) dir i

      | UnknownType(ty)
      | UnknownTypedef(ty) ->
          Printf.sprintf "cat %s/%s %s | %s > %s/step%d.cocci"
          tdir cocci_file extra (replace "TY" ty) dir i

      | UnknownFieldGeneric(fld) ->
          Printf.sprintf "cat %s/%s %s | %s > %s/step%d.cocci"
          tdir cocci_file extra (replace "FLD" fld) dir i

      | Void1(exp)
      | TypeChange(exp) ->
          Printf.sprintf "cat %s/%s %s | %s > %s/step%d.cocci"
          tdir cocci_file extra (replace "EXP" exp) dir i

      | UnknownFunction2(fn,alt) ->
          Printf.sprintf "cat %s/%s %s | %s | %s > %s/step%d.cocci"
          tdir cocci_file extra (replace "FN" fn) (replace "ALT" alt) dir i

      | UnknownField(str,fld)
      | UnknownFieldOrType(str,fld)
      | Void2(str,fld)
      | TypeChange2(str,fld)
      | BadInitType(str,fld)
      | BadNonfnInit(str,fld) ->
          Printf.sprintf "cat %s/%s %s | %s | %s > %s/step%d.cocci"
          tdir cocci_file extra (replace "STR" str) (replace "FLD" fld) dir i

      | BadArgType(fn,argn) ->
          let argn = string_of_int(int_of_string argn - 1) in
          Printf.sprintf "cat %s/%s %s | %s | %s > %s/step%d.cocci"
          tdir cocci_file extra (replace "FN" fn) (replace "ARG" argn) dir i

    in
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
  let sed_cmd (placeholder, replacement) =
    Printf.sprintf "sed 's/\\${%s}/%s/g'" placeholder
      (Str.global_replace (Str.regexp_string "/") "\\/" replacement)
  in
  let to_replace = [ (* Arguments for template substitution *)
      ("COMMIT", commit);
      ("FILE", file);
      ("NAME", temp);
      ("TARGET", target);
      ("GIT", linux)
    ]
  in
  let cmd = Printf.sprintf "cat %s/Makefile | %s > %s"
    tdir (String.concat " | " (List.map sed_cmd to_replace)) mout
  in
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
	  Printf.fprintf o "\t--msg \"%s\"\n" (String.escaped (msg arg)))
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
	    let text_file = (type_to_normalized_name arg) ^ ".txt" in
	    match arg with
        | Unknown ->""

        | UnknownFunction(fn)
        | TooManyArgs(fn)
        | TooFewArgs(fn) ->
            Printf.sprintf "%s %s/%s | %s > %s"
                (replace "FN" (texsafe2 fn)) tdir text_file
                (replace "NUM" (string_of_int i)) txt

        | UnknownVariable(cst) ->
            Printf.sprintf "%s %s/%s | %s > %s"
                (replace "CST" (texsafe2 cst)) tdir text_file
                (replace "NUM" (string_of_int i)) txt

        | UnknownType(ty)
        | UnknownTypedef(ty) ->
            Printf.sprintf "%s %s/%s | %s > %s"
                (replace "TY" (texsafe2 ty)) tdir text_file
                (replace "NUM" (string_of_int i)) txt

        | UnknownFieldGeneric(fld) ->
            Printf.sprintf "%s %s/%s | %s > %s"
                (replace "FLD" (texsafe2 fld)) tdir text_file
                (replace "NUM" (string_of_int i)) txt

        | Void1(exp)
        | TypeChange(exp) ->
            Printf.sprintf "%s %s/%s | %s > %s"
                (replace "EXP" (texsafe2 exp)) tdir text_file
                (replace "NUM" (string_of_int i)) txt

        | UnknownFunction2(fn,alt) ->
            Printf.sprintf "%s %s/%s | %s | %s > %s"
                (replace "FN" (texsafe2 fn)) tdir text_file
                (replace "ALT" (texsafe2 alt))
                (replace "NUM" (string_of_int i)) txt

        | UnknownField(str,fld)
        | UnknownFieldOrType(str,fld)
        | Void2(str,fld)
        | TypeChange2(str,fld)
        | BadInitType(str,fld)
        | BadNonfnInit(str,fld) ->
            Printf.sprintf "%s %s/%s | %s | %s > %s"
                (replace "STR" (texsafe2 str)) tdir text_file
                (replace "FLD" (texsafe2 fld))
                (replace "NUM" (string_of_int i)) txt

        | BadArgType(fn,argn) ->
            Printf.sprintf "%s %s/%s | %s | %s > %s"
                (replace "FN" (texsafe2 fn)) tdir text_file
                (replace "ARG" (texsafe2 argn))
                (replace "NUM" (string_of_int i)) txt

	    in
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
