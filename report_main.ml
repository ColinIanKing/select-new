let target = ref "v4.6"
let linux = "/run/shm/linux"

let rec parse_args (bp,file,commit,args) = let open Report in function
    [] -> (bp,file,commit,List.rev args)
  | "--backport"::rest -> parse_args (true,file,commit,args) rest
  | "--unknown"::rest ->
      parse_args (bp,file,commit,Unknown::args) rest
  | "--unknown-function"::fn::rest ->
      parse_args (bp,file,commit,(UnknownFunction fn)::args) rest
  | "--unknown-function2"::fn::alt::rest ->
      parse_args (bp,file,commit,(UnknownFunction2(fn,alt))::args) rest
  | "--unknown-variable"::cst::rest ->
      parse_args (bp,file,commit,(UnknownVariable cst)::args) rest
  | "--unknown-type"::ty::rest ->
      parse_args (bp,file,commit,(UnknownType ty)::args) rest
  | "--unknown-typedef"::ty::rest ->
      parse_args (bp,file,commit,(UnknownTypedef ty)::args) rest
  | "--unknown-field"::str::fld::rest ->
      if Str.split (Str.regexp "[ \t]+") str = ["struct";"<anonymous>"]
      then parse_args (bp,file,commit,(UnknownFieldGeneric(fld))::args) rest
      else parse_args (bp,file,commit,(UnknownField(str,fld))::args) rest
  | "--unknown-field-or-type"::str::fld::rest ->
      parse_args (bp,file,commit,(UnknownFieldOrType(str,fld))::args) rest
  | "--unknown-field-generic"::fld::rest ->
      parse_args (bp,file,commit,(UnknownFieldGeneric(fld))::args) rest
  | "--void1"::exp::rest ->
      parse_args (bp,file,commit,(Void1 exp)::args) rest
  | "--void2"::str::fld::rest ->
      parse_args (bp,file,commit,(Void2(str,fld))::args) rest
  | "--type-change"::exp::rest ->
      parse_args (bp,file,commit,(TypeChange exp)::args) rest
  | "--type-change2"::str::fld::rest ->
      parse_args (bp,file,commit,(TypeChange2(str,fld))::args) rest
  | "--bad-init-type"::str::fld::rest ->
      parse_args (bp,file,commit,(BadInitType(str,fld))::args) rest
  | "--bad-nonfn-init"::str::fld::rest ->
      parse_args (bp,file,commit,(BadNonfnInit(str,fld))::args) rest
  | "--bad-arg-type"::fn::argn::rest ->
      parse_args (bp,file,commit,(BadArgType(fn,argn))::args) rest
  | "--too-many-args"::fn::rest ->
      parse_args (bp,file,commit,(TooManyArgs fn)::args) rest
  | "--too-few-args"::fn::rest ->
      parse_args (bp,file,commit,(TooFewArgs fn)::args) rest
  | s::rest ->
      let info =
	if file = ""
	then (bp,s,"",args)
	else
	  if commit = ""
	  then (bp,file,s,args)
	  else failwith ("bad arg: "^s) in
      parse_args info rest

let _ =
    let cmd = Array.get Sys.argv 0 in
    let cmddir = Filename.dirname cmd in
    let tdir = cmddir ^ "/templates" in
    let args = List.tl (Array.to_list Sys.argv) in
    let options = parse_args (false,"","",[]) args in
    Report.do_report !target linux tdir options
