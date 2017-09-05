(* 
 * @dpalinsk
 * program to parse linux driver compilation error output and try to remove notes, 
 * includes, redundant errors
 * Started: 17 June 2016
 * Updated: 5 August 2016
 * TODO: Remove (relatively) arbitrary line numbers, replace with semantic patches
 *)
 
(*__________________________ Types and Constants_____________________________*)
let linString = ref "."
let spatch = ref "./"
let file = ref ""
let verbose = ref false
let usage = "A tool to reduce redundant GCC errors. Expects a file with GCC
error output as the last command line argument."
let options = [("--linux", Arg.Set_string linString, "Path to Linux");
               ("--sp", Arg.Set_string spatch, "Directory of Semantic Patches");
               ("-v", Arg.Set verbose, "Enables Verbose Printing")]

type error_types = 
| Unknown | DeclaredInParameterList | WrongType | HasNoMember
| PointerToIncompleteType | VariableUndeclared | FunctionUndeclared | UndefType
| IncompatiblePtrType | ExcessElements | UnknownField | SizeUnknown | UnusedVar
| TooManyArgs | TooFewArgs | NotInRecordOrUnion | NonConstantInitElem 
| BracesAroundScalar | ImproperArrayIndex | MemberNotInStructOrUnion
| Note | IncompElementType | InitButIncompType | BadScope | TooManyMacro
| UnknownTypName | CompOfDistinctPtrTypes | HasIncompleteType | NotAPrototype 
| VoidValueNotIgnored | ControlNonVoid | PtrFromInteger | UsedStruct 
| IncompTypes | ExtraBrace

type errors = {
  lineNumber: int; 
  msg: string; (*string of error*)
  typ: error_types;
}
(*_______________________________Helper Functions____________________________*)
(*skips lines a certain number of times*)

let error_types_to_string err =
  match err.typ with
  | Unknown -> "Unknown" | DeclaredInParameterList -> "Declared in Parameter List"
  | WrongType -> "Wrong Type" | HasNoMember -> "Has No Member" 
  | PointerToIncompleteType -> "Pointer to Incomplete Type"
  | VariableUndeclared -> "Variable Undeclared" 
  | FunctionUndeclared -> "Function Undeclared" 
  | UndefType -> "Undefined Type" | IncompatiblePtrType -> "Incompatible Pointer Type"
  | ExcessElements -> "Excess Elements" | UnknownField -> "Unknown Field" 
  | SizeUnknown -> "Size Unknown" | UnusedVar -> "Unused Variable"
  | TooManyArgs -> "Too Many Args" | TooFewArgs -> "Too Few Args" 
  | NotInRecordOrUnion -> "Not In Record or Union"
  | NonConstantInitElem -> "Initializer Element Not Constant" 
  | BracesAroundScalar -> "Braces Around Scalar Initializer"
  | ImproperArrayIndex -> "Improper Array Index" 
  | MemberNotInStructOrUnion -> "Member Not in Struct or Union"
  | Note -> "Note" | IncompElementType -> "Incomplete Element Type"
  | InitButIncompType -> "Initializer But Incomplete Type"
  | BadScope -> "Scope Only in This Definition or Declaration"
  | TooManyMacro -> "Too Many Arguments Passed to Macro"
  | UnknownTypName -> "Unknown Type Name"
  | CompOfDistinctPtrTypes -> "Comparison of Distinct Pointer Types"
  | HasIncompleteType -> "Has Incomplete Type"
  | NotAPrototype -> "Not a Prototype"
  | VoidValueNotIgnored -> "Void Value Not Ignored"
  | ControlNonVoid -> "Control Reaches End of Non-Void Function"
  | PtrFromInteger -> "Pointer From Integer Without a Cast"
  | UsedStruct -> "Used Struct where Something Else is Required"
  | IncompTypes -> "Incompatible Types" | ExtraBrace -> "Extra Brace Group"

let skip_line times input = 
  for i = 1 to times do
    ignore(input_line input)
  done

(*Tests for the existence of a substring in a string*)
let has_substr str sub =
  let re = Str.regexp_string sub in
  try ignore (Str.search_forward re str 0); true
  with Not_found -> false

(*Converts a string to a list of chars*)
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) []

let implode l = 
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> String.set res i c; imp (i+1) l in
  imp 0 l

let matches s = let chars = explode s in fun c -> List.mem c chars

let alphanum = matches "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
(*Finds the index of a target, returning -1 if the element doesn't exist*)
let find_ind lst targ =
  let rec helper lst targ curr = 
    match lst with
    | [] -> -1
    | hd::tl -> if (hd = targ) then curr else helper tl targ (curr+1) 
  in helper lst targ 0


(*Returns the minimum element of a list*)
let least lst =
  match lst with 
  | [] -> failwith "bad argument to least"
  | x :: xs -> List.fold_left (fun a b -> if a < b then a else b) x xs

(*Prints the string representation of an error*)
let print_err_verbose err =
    Printf.printf "Type: %s\n %s" (error_types_to_string err) err.msg

let print_err err =
  if err.typ <> Note || has_substr err.msg "in expansion of macro"
  then Printf.printf "%s"  err.msg

(*Converts a string to an error type*)
let string_to_err_typ str =
  if has_substr str "declared inside parameter" then DeclaredInParameterList
  else if has_substr str "expects argument of type" then WrongType
  else if has_substr str "has no member" then HasNoMember
  else if has_substr str "pointer to incomplete" then PointerToIncompleteType
  else if (has_substr str "first use in this function" || 
           has_substr str "undeclared here") then
    VariableUndeclared
  else if (has_substr str "implicit declaration of function" || 
           has_substr str "-Wunused-function") then
    FunctionUndeclared
  else if (has_substr str "undefined type") then UndefType
  else if (has_substr str "from incompatible pointer type") then
    IncompatiblePtrType
  else if (has_substr str "excess elements in") then ExcessElements
  else if (has_substr str "extra brace group at end of initializer") then
    ExtraBrace
  else if (has_substr str "unknown field") then UnknownField
  else if (has_substr str "storage size of") then SizeUnknown
  else if (has_substr str "unused variable" || 
           has_substr str "defined but not used") then
    UnusedVar
  else if (has_substr str "too many arguments") then TooManyArgs
  else if (has_substr str "but only" || 
           has_substr str "too few arguments to") then
    TooFewArgs
  else if (has_substr str "not in record or union") then NotInRecordOrUnion
  else if (has_substr str "element is not constant") then NonConstantInitElem
  else if (has_substr str "braces around") then BracesAroundScalar
  else if (has_substr str "array index in non-array" ||  
           has_substr str"array index in initializer") then
    ImproperArrayIndex
  else if (has_substr str "not a structure or union") then
    MemberNotInStructOrUnion
  else if (has_substr str "near initialization" || 
           has_substr str "in expansion of macro") then Note
  else if (has_substr str "incomplete element type") then IncompElementType
  else if (has_substr str "initializer but incomplete type") then
    InitButIncompType
  else if (has_substr str "its scope is only") then BadScope
  else if (has_substr str "but takes just") then TooManyMacro
  else if (has_substr str "unknown type name") then UnknownTypName
  else if (has_substr str "comparison of distinct pointer types") then
    CompOfDistinctPtrTypes
  else if (has_substr str "has incomplete type") then HasIncompleteType
  else if (has_substr str "isn’t a prototype") then NotAPrototype
  else if (has_substr str "void value not ignored") then VoidValueNotIgnored
  else if (has_substr str "control reaches end of") then ControlNonVoid
  else if (has_substr str "pointer from integer without a cast") then
    PtrFromInteger
  else if (has_substr str "used struct type value") then UsedStruct
  else if (has_substr str "incompatible types when assigning") then IncompTypes
  else Unknown

(*Finds converts a list of errors to their line numbers*)
let rec lineNumberList_of_errorList errlst =
  match errlst with
  | [] -> []
  | h :: t ->  h.lineNumber :: lineNumberList_of_errorList t

(* Finds the occurence of an  Error whose typ is typ and is nearest to err and 
 * returns its line number
 *)
let rec find_type_line errList err typ=
  let l = List.filter (fun x -> (x.typ = typ)) errList in
  if (l = []) then -100 else
  let l2 = lineNumberList_of_errorList l in
  let l3 = List.map (fun x -> abs(x - err.lineNumber)) l2 in
  let min = least l3 in 
  let ind = find_ind l2 (abs(min - err.lineNumber)) in
  if (ind != -1) then
    let fetched = List.nth l2 ind in
    (try let retErr = List.find (fun x -> x.lineNumber=fetched) errList in
      retErr.lineNumber
     with Not_found ->
      -100)
  else 
    let ind = find_ind l2 (min + err.lineNumber) in
    if (ind != -1) then let fetched = List.nth l2 ind in
    (try let retErr = List.find (fun x -> x.lineNumber=fetched) errList in
      retErr.lineNumber
    with Not_found ->
      -100)
    else 
      -100

(*From Coccinelle -- coccinelle/commons/common.ml*)
let process_output_to_list2 = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)
let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l
let process_output_to_list = cmd_to_list
let cmd_to_list_and_status = process_output_to_list2
(*End of From Coccinelle -- coccinelle/commons/common.ml*)

(*Builds a list of types from an Error List, unusedd in some predicates*)
let rec buildTypList errList = 
  match errList with
  | [] -> []
  | h :: t -> h.typ :: buildTypList t

(*Converts a String to an Errors struct*)
let errStruct_of_string str =
  let info = Str.split (Str.regexp "\n") str in 
  if (has_substr (List.nth info 0) "In function") then
    let info2 = Str.split (Str.regexp ":") (List.nth info 1) in
      {lineNumber = (int_of_string (List.nth info2 2)); msg = str; 
      typ = (string_to_err_typ  str)}
  else if (has_substr (List.nth info 0) "error" || 
    has_substr (List.nth info 0) "warning") then
      let info2 = Str.split (Str.regexp ":") (List.nth info 0) in
      {lineNumber = (int_of_string (List.nth info2 2)); msg = str; 
       typ = (string_to_err_typ  str)}
  else if (has_substr (List.nth info 0) "near initialization") then
    let info2 = Str.split (Str.regexp ":") (List.nth info 0) in 
      {lineNumber = (int_of_string (List.nth info2 1)); msg = (str ^ "\n"); 
      typ = (string_to_err_typ  str)}
  else if (has_substr (List.nth info 0) "in expansion of macro") then
    let info2 = Str.split (Str.regexp ":") (List.nth info 0) in
      {lineNumber = (int_of_string (List.nth info2 2)); msg = (str ^ "\n")  ; 
       typ = (string_to_err_typ  str)}
  else
    raise (Failure "Unrecognized error")

let rec find_all_errors predicate errList = 
  match errList with
  | [] -> []
  | h :: t -> if (predicate h) then h :: find_all_errors predicate t else
    find_all_errors predicate t

let rec compressNotes l =
    match l with
    | [] -> []
    | [_] -> l
    | h1 :: ((h2 :: _) as tail) ->
        if h1.typ = Note && h2.typ = Note then compressNotes tail else 
        h1 :: compressNotes tail

let extract_id str start typ = 
  let rec extract_id_helper charlst =  
    match charlst with
    | [] -> []
    | h :: t ->
      match alphanum h with 
      | true ->  h :: extract_id_helper t
      | false -> extract_id_helper []
  in
    let x2 = explode str in let charlist = extract_id_helper x2 in
    implode charlist

let find_problem_var err = 
  let l = Str.split (Str.regexp "\n") err.msg in 
  try
    let indicator = List.nth l (List.length l - 1) and
    code = List.nth l (List.length l - 2) in
    let indicator_index = String.length indicator in 
    let ind = String.sub code indicator_index 1 in
    let codelist = Str.split (Str.regexp " ") code in
    (try let snippet = List.find (fun x -> has_substr x ind) codelist in
    print_string snippet;
    extract_id snippet indicator_index err.typ with Not_found -> "Problem in find_problem_var")
  with nth -> failwith "Problem finding bad var"

let get_containing_function err =
  let l2 = Str.split (Str.regexp ":") err.msg in
  (try 
    let file = String.trim (List.find (fun x -> has_substr x "/") l2) in
    let strct = find_problem_var err in 
    print_string "!";
    print_string strct; 
    let cmd = Printf.sprintf "spatch %s/startend.cocci -D id=%s -D ln=%d %s/%s" 
    !spatch strct err.lineNumber !linString file in
    (try List.hd (cmd_to_list cmd) with Failure("hd") -> "")
  with Not_found ->
    Printf.printf "Unrecognized error format. Error should include filename.";
    exit 1)
(*_____________________________Predicate Functions_____________________________*)
(*Declared Inside Parameter List predicate for filtering*)
let dipl_predicate struct_string func dependTypes incTypeLineNumber errlist err = 
  not ((err.typ = HasIncompleteType || err.typ = NotAPrototype)
     && has_substr err.msg struct_string) &&
  not (err.typ = PointerToIncompleteType && 
      (get_containing_function err) = func) &&
  not (err.typ = BadScope && abs(err.lineNumber - 
      (find_type_line errlist err DeclaredInParameterList)) = 0)
  && 
  not ((has_substr err.msg struct_string && 
      ( err.typ = DeclaredInParameterList || err.typ = HasNoMember ||
      err.typ = PointerToIncompleteType || err.typ = UndefType || 
      err.typ = IncompatiblePtrType))
  ||  
  ((List.mem (PointerToIncompleteType) dependTypes) && 
       (err.typ = ExcessElements 
        || err.typ = UnknownField
	|| err.typ = ExtraBrace)  && 
  (abs (incTypeLineNumber   - err.lineNumber) <= 10))
  || 
  (abs(err.lineNumber - (find_type_line errlist err DeclaredInParameterList))
   <= 10) && (err.typ = IncompatiblePtrType))

let dipl_helper h t typList origin singleQuote =
  let l = Str.split singleQuote h.msg and l2 = Str.split (Str.regexp "\n") 
    h.msg in
    (try let x = List.nth l2 (List.length l2 - 2) in
    let l3 = Str.split (Str.regexp " ") x in
    let func = List.hd (List.rev l3) in
    if func.[0] = '*' then
      (let func = String.sub func 1 (String.length func - 1) in
       List.filter (dipl_predicate (List.nth l 1) func  typList 
                 (find_type_line t h HasNoMember) origin) t)
    else 
      (List.filter (dipl_predicate (List.nth l 1) func  typList
                             (find_type_line t h HasNoMember) origin) t)
    with Invalid_argument("List.nth") ->
      t)

(*This one is pretty basic. If we have an undeclared function, and 
something else complains about that function, we get rid of it.*)
let fnct_undeclared_predicate this fnctString err = 
  if (err.typ = FunctionUndeclared) then
  let singleQuote = (Str.regexp "\226\128\152\\|\226\128\153") in
  let l = Str.split singleQuote err.msg in (*Isolate Function Name*)
  (try let errIndex =  List.find (fun x -> has_substr x "warning" 
                                  || has_substr x "error") l in
    let func = (List.nth l ((find_ind l errIndex) + 1)) in
    not (fnctString = func)
  with Not_found -> failwith "Bad Function Here")
  else
    (err.typ = VariableUndeclared || 
      (
      not ((has_substr err.msg fnctString) 
      || (err.typ = BracesAroundScalar && err.lineNumber = this.lineNumber)
      || (err.typ = NonConstantInitElem && err.lineNumber = this.lineNumber))
      )
    )

let fnct_undeclared_helper h t singleQuote =
  let l = Str.split singleQuote h.msg in
  (try let errIndex =  List.find (fun x -> has_substr x "warning" 
                                    || has_substr x "error") l in
    let func = (List.nth l ((find_ind l errIndex) + 1)) in
    List.filter (fnct_undeclared_predicate h func) t
  with 
    Not_found -> Printf.printf 
    "Should have warning or error on undeclared function.\n"; 
    exit(1))

let undefined_type_predicate utLineNumber typeString err=
  not ((err.typ = ExcessElements || err.typ == UnknownField)  && 
  (abs (utLineNumber - err.lineNumber) <= 10) || 
  (has_substr typeString err.msg))

let excess_element_predicate this errList err =   
  not ((abs(this.lineNumber - (find_type_line errList this UndefType)) <= 10))

let unknown_field_predicate this errList field err =
  let nearestExcessElement = find_type_line errList this ExcessElements in
  not ((abs(this.lineNumber - (find_type_line errList this UndefType))) <= 10 
      ||
      (err.typ = UnknownField && (has_substr err.msg field)) ||
      (err.typ = ExcessElements && nearestExcessElement - this.lineNumber <= 2 )
      ||
      (err.typ = IncompatiblePtrType && 
       abs(this.lineNumber - err.lineNumber) <= 2))

let unknown_field_helper h t origin singleQuote= 
  let l = Str.split singleQuote h.msg in
    (try let errIndex =  
      List.find (fun x -> has_substr x "warning" || has_substr x "error") l in
      let field = (List.nth l ((find_ind l errIndex) + 1)) in
      List.filter (unknown_field_predicate h origin field) t
    with Not_found ->
      t)

let unused_variable_predicate varName err = 
  not(has_substr err.msg varName)

let unused_variable_helper h t origin=
  let singleQuote = Str.regexp "\226\128\152\\|\226\128\153" in 
  let l = Str.split singleQuote h.msg and
  l2 = Str.split (Str.regexp ":") h.msg in
  let varName = " " ^ (List.nth l (List.length l - 2)) and
  file = String.trim (List.find (fun x -> has_substr x "/") l2)
  and varName2 = (List.nth l (List.length l - 2)) in
  let cmdString = Printf.sprintf "spatch %s/tfa.cocci -D i=%s -D line=%d %s/%s" 
  !spatch varName2 h.lineNumber !linString file in
  let useList = cmd_to_list cmdString in
  (List.length useList, (find_all_errors (unused_variable_predicate varName) origin))

let storage_size_predicate varName err =
  not ((err.typ = UnusedVar) && has_substr err.msg varName)

let size_unknown_helper h t singleQuote = 
  let l = Str.split singleQuote h.msg in
  let varName = " " ^ (List.nth l (List.length l - 2)) in
  (try let x = (List.find (storage_size_predicate varName) t) in
  ignore(x); true with Not_found -> false)

let incmpt_ptr_type_predicate code err = 
  not ((err.typ = IncompatiblePtrType || err.typ = VariableUndeclared) &&
    has_substr err.msg code)

let incmpt_ptr_type_helper err t= 
  let lst = Str.split (Str.regexp ":") err.msg in
  (try 
    let argIndMinus1 = List.find 
    (fun x -> has_substr x "error" || has_substr x "warning") lst in
    let errthing = (List.nth lst ((find_ind lst argIndMinus1) + 1)) in
    let argNumber = Str.split (Str.regexp "from") errthing in
    let arg = List.hd argNumber in
    if (arg = " initialization " || arg = " assignment ")  then
      let l = Str.split (Str.regexp "\n") err.msg in
      (if List.length l = 1 then
        t
      else  
        (let code = (List.nth l (List.length l - 2)) ^ "\n" ^
        (List.nth l (List.length l - 1)) in
        List.filter (incmpt_ptr_type_predicate code) t))
    else 
      (List.filter (incmpt_ptr_type_predicate arg) t)
  with Not_found -> failwith "Unexpected Error Input")

let no_member_predicate h msgString structString err = 
  if (structString = "") then
    not (has_substr err.msg msgString ||
        (has_substr h.msg "return" && err.typ = ControlNonVoid))
  else
    not (has_substr err.msg msgString || 
    (err.typ = MemberNotInStructOrUnion && has_substr err.msg structString)
    || (has_substr h.msg "return" && err.typ = ControlNonVoid))

(* A more complicated case than what I wanted in the pattern match. 
 * Here, we look for the name of the struct so we can filter out 
 *)
let no_member h t =
  let l = (Str.split (Str.regexp ":") h.msg) in
    (try let noMemString = List.find (fun x -> has_substr x "member") l and 
      msg = List.find (fun x -> has_substr x "struct") l and 
      code = List.nth l (List.length l - 1) and
      singleQuote = (Str.regexp "\226\128\152\\|\226\128\153") in
      let hasmember = Str.split singleQuote msg in 
      let member = List.nth hasmember (List.length hasmember - 1) in
      (try ignore(Str.search_forward 
        (Str.regexp (" [A-Za-z]+->" ^ member)) code 0);
        let s = String.trim (Str.matched_string code) in
          List.filter (no_member_predicate h noMemString s) t
      with Not_found -> List.filter (no_member_predicate h noMemString "") t)
    with Not_found -> List.filter (no_member_predicate h "" "") t)
 
let inc_type_predicate msgString err = 
  not (has_substr err.msg msgString)

let inc_type_helper h t = 
  let l = (Str.split (Str.regexp ":") h.msg) in
  (try 
   let incTypeString = List.find (fun x -> has_substr x "incomplete type") l in
   List.filter (inc_type_predicate incTypeString) t
  with Not_found ->
   t)

let too_few_args_predicate argList macro str err = 
  let singleQuote = (Str.regexp "\226\128\152\\|\226\128\153") in
  if err.typ = FunctionUndeclared then
    (let l = Str.split singleQuote err.msg in
    (try 
      let ind = find_ind l (List.find (fun x -> has_substr x "warning:") l) in
      let id = List.nth l (ind + 1) in
      not (List.mem id argList)
    with Not_found ->
      true))
 else 
  not (has_substr err.msg str || 
      (err.typ = VariableUndeclared && has_substr err.msg macro))

let too_few_args_helper h t = 
  if has_substr h.msg "macro" then
    let l = Str.split (Str.regexp "\"") h.msg and 
    l2 = Str.split (Str.regexp "\n") h.msg  and
    l3 = Str.split (Str.regexp ":") h.msg in
    let code = List.nth l2 (List.length l2 - 2)  and 
    s = List.find (fun x -> has_substr x "but only") l3 in
    let code2 = String.trim code in
    let code3 = Str.split (Str.regexp ",\\|)") code2 in
    let argList = (List.tl (List.rev code3)) in
    (try 
      let indMinus1 = List.find (fun x -> has_substr x "error: macro") l in
      let macro = List.nth l ((find_ind l indMinus1) + 1) in
      List.filter (too_few_args_predicate argList macro s) t
    with Not_found -> 
      t)
  else
    (
    let l = Str.split (Str.regexp ":") h.msg in
    (try let s = List.find (fun x -> has_substr x "but only" || 
                            has_substr x "too few arguments") l in
    List.filter (too_few_args_predicate [] 
    "ThisIsAStringThatWon'tAppear" s) t
    with Not_found ->
      t))

let inconstant_init_predicate str err = 
  not (has_substr err.msg str)

let inconstant_init_helper h t = 
  let l = Str.split (Str.regexp "\n\\|(") h.msg in
  (try let s = List.find (fun x -> (has_substr x "is not constant")) l in
    let ind = (find_ind l s) + 1 in 
    let x = List.nth l ind in 
    List.filter (inconstant_init_predicate x) t
  with nth ->
    t)

let not_in_struct_or_union err errList =
  let noteLine = find_type_line errList err Note and 
  singleQuote = (Str.regexp "\226\128\152\\|\226\128\153") in
  try 
    let note = List.find 
    (fun x -> x.lineNumber = noteLine && (x.typ = Note)) errList in
    let l = Str.split singleQuote note.msg in
    let initd = List.nth l (List.length l - 2) in
    let incTypeLine = find_type_line errList note IncompElementType in
    let incType = List.find 
    (fun x -> x.lineNumber = incTypeLine && 
    (x.typ = IncompElementType)) errList in
    (((err.lineNumber - noteLine) <= 3 && (err.lineNumber - noteLine) >= 0) && 
    (has_substr incType.msg initd))
  with Not_found ->
    false

let var_undeclared_predicate varName err = 
  not (err.typ = VariableUndeclared && has_substr err.msg varName)

let init_incomp_type_predicate this structString err = 
  not (err.typ = UndefType && has_substr err.msg structString
       ||
	 (err.typ = UnusedVar && this.lineNumber = err.lineNumber)
       ||
       (err.typ = UnknownField || err.typ = ExcessElements) && 
        abs(this.lineNumber - err.lineNumber) <= 10)

let init_incomp_type_helper h t = 
  let l = Str.split (Str.regexp "\n") h.msg in
  let l2 = Str.split (Str.regexp " ") (List.nth l (List.length l - 2)) in
  (try let structInd = find_ind l2 (List.find (fun x -> x = "struct") l2) in
  let structString = (List.nth l2 structInd) ^ " " ^ 
    (List.nth l2 (structInd + 1)) in
  List.filter (init_incomp_type_predicate h structString) t
  with Not_found ->
    t)

let too_many_macro_helper h t =
  let l = Str.split (Str.regexp "\n") h.msg in
  let l2 = Str.split (Str.regexp "\"") (List.hd l) in 
  let macro = List.nth l2 (List.length l2 - 2) in
  List.filter (fun x -> not (x.typ = TooManyMacro && has_substr x.msg macro)) t

let ptr_from_int_helper h t =
  let l = Str.split (Str.regexp "\n") h.msg in
  (try let code = List.nth l (List.length l - 2) in
    List.filter (fun x -> not (has_substr x.msg code)) t
   with nth -> 
    t)

let unknown_typ_name_helper h t singleQuote= 
  let l = Str.split singleQuote h.msg in
  let typ = List.nth l (List.length l - 2) in
  List.filter (fun x -> not(x.typ = UnknownTypName && has_substr x.msg typ)) t

let comp_dstnct_ptr_types_helper h t = 
  let l = Str.split (Str.regexp "\n") h.msg in
  let codeSnippet = List.hd (List.rev l) in
  List.filter (fun x -> not (x.typ = CompOfDistinctPtrTypes &&
                             has_substr x.msg codeSnippet)) t

let void_val_not_ignored_predicate this code err = 
  not ((err.typ = ControlNonVoid && abs(this.lineNumber - err.lineNumber) <= 2) ||
       (err.typ = VoidValueNotIgnored && has_substr err.msg code))

let void_val_not_ignored_helper h t =
  let l = Str.split (Str.regexp "=") h.msg in
  let x = List.nth l (List.length l - 1) in
  List.filter (void_val_not_ignored_predicate h x) t

let has_incomp_type_helper h t =
  let l = Str.split (Str.regexp "\n") h.msg in
  (try 
    (let x = List.find (fun x -> has_substr x "struct") l in
    let structLine = Str.split (Str.regexp " ") x in
    let structString = List.nth structLine 1 in
    List.filter (fun x -> not (x.typ = HasIncompleteType && 
                               has_substr x.msg structString)) t)
  with Not_found ->
    t)

let wrong_type_helper singleQuote h t =
  let l = Str.split singleQuote h.msg in
  try let indMinus1 = List.find (fun x -> has_substr x "has type") l in
  (try let typeString = List.nth l ((find_ind l indMinus1) + 1) in 
   List.filter (fun x -> not (x.typ = IncompTypes && 
                              has_substr x.msg typeString)) t
   with nth -> t)
  with Not_found -> t

let macro_note_helper h t =
  if (List.hd t).typ = Note && has_substr (List.hd t).msg 
  "in expansion of macro" then
  let newt = compressNotes t in
  List.filter (fun x -> not (x.lineNumber = h.lineNumber && x.typ = Note)) newt else
  List.filter (fun x -> not (x.lineNumber = h.lineNumber && x.typ = Note)) t
(*____________________________Processing Functions___________________________*)
(*Case where we have 4 lines in a group, indicated by a line containing 
'In function: ____'*)
let process_error_1 str i= 
  let lst = (Str.split   (Str.regexp ":") str) in 
   let str2 = String.trim (List.nth lst 1) in
  let l = input_line i in 
    let lst = (Str.split   (Str.regexp ":") l) in
    let file = Printf.sprintf "\n %s" (List.nth lst 0) in
    let str2 = String.concat ":" 
    [str2; file; "Line"; List.nth lst 1; List.nth lst 3; List.nth lst 4; "\n"] in 
  let l = input_line i in
    let str2 = String.concat "" [str2; l; "\n"] in
  let l = input_line i in 
    let str2 = String.concat "" [str2; l; "\n"] in
  Printf.sprintf "%s" str2

(*Case where we have 3 lines in a group, indicated by the absence of a line 
containing 'In function: ________'*)
let rec process_error_2 str x i=
  let lst = (Str.split   (Str.regexp ":") str) in
    let file = (Printf.sprintf "%s" (List.nth lst 0)) in
    let str2 = String.concat ":" 
    [file; "Line"; List.nth lst 1; List.nth lst 3; List.nth lst 4; "\n"] in  
  let l = input_line i in
    if has_substr l "error:" || has_substr l "warning:" || has_substr l "note" 
    || has_substr l "In function" || has_substr l "make[" || has_substr l "cc1"  then
      (x := str2 :: !x;
      process_error_3 l x i)
    else
      let str2 = String.concat "" [str2; l; "\n"] in
  let l = input_line i in
    let str2 = String.concat "" [str2; l; "\n"] in
    Printf.sprintf "%s" str2

and process_error_3 l x i=
  if (has_substr l "make") || (has_substr l "cc1") then
    Printf.sprintf ""
  else if (has_substr l "In function") then
    (let newString = String.concat "" [""; process_error_1 l i] in
    (Printf.sprintf "%s" newString))
  else if ((has_substr l "error:") || (has_substr l "warning:")) then
    (let newString = String.concat "" [""; process_error_2 l x i] in
    (Printf.sprintf "%s" newString))
  else if (has_substr l "near initialization") then
    Printf.sprintf "%s" l
  else
    Printf.sprintf ""

let read_input_strings x printString lst i=
  try while true do
    let l = input_line i in
    if (has_substr l "make" && not (has_substr l "makes")) || (has_substr l "cc1") then
      (skip_line 1 i;)
    else if (has_substr l "In function") then
      (let newString = String.concat "" [printString; process_error_1 l i] in
      lst := (Printf.sprintf "%s" newString) :: !lst ;)
    else if ((has_substr l "error:") || (has_substr l "warning:") || 
              has_substr l "in expansion of macro") then
      (let newString = String.concat "" [printString; process_error_2 l lst i] 
       in
        lst := (Printf.sprintf "%s" newString) :: !lst;)
    else if (has_substr l "near initialization") then
      lst := l :: !lst
    else
      if ((printString <> "")) then
        skip_line 1 i;
    done;
    with End_of_file ->
      close_in i

let rec process_strings str_lst =
  match str_lst with 
  | [] -> []
  | h :: t -> errStruct_of_string h :: process_strings t

let rec process_error_list err_lst typList origin= 
  match err_lst with
  | [] -> []
  | h :: [] -> 
    (let singleQuote = Str.regexp "\226\128\152\\|\226\128\153" in 
    match h.typ with
      | SizeUnknown -> let remove = size_unknown_helper h [] singleQuote in
        if remove then process_error_list [] typList origin else
        h :: process_error_list [] typList origin
      | UnusedVar -> let x = unused_variable_helper h [] origin in
          if fst x >= 1 then process_error_list [] typList (snd x)
          else h :: process_error_list [] typList (snd x)
      | NotInRecordOrUnion -> 
        let nearNote = find_type_line origin h Note in
        if ((h.lineNumber - nearNote) <= 3 && (h.lineNumber - nearNote) >= 0) 
        then process_error_list [] typList origin else
        h :: process_error_list [] typList origin
      | _ -> h :: process_error_list [] typList origin)
  | h :: t ->
    let singleQuote = Str.regexp "\226\128\152\\|\226\128\153" in 
    match h.typ with
    | DeclaredInParameterList -> 
        let newt = dipl_helper h t typList origin singleQuote in
        h :: process_error_list newt typList origin
         
    | WrongType -> let newt = wrong_type_helper singleQuote h t in 
        h :: process_error_list newt typList origin

    | HasNoMember -> 
        let newt = no_member h t in h :: process_error_list newt typList origin

    | PointerToIncompleteType -> let newt = inc_type_helper h t in
        h :: process_error_list newt typList origin
        
    | VariableUndeclared -> let l = Str.split singleQuote h.msg in 
        if List.length l <= 3 then
          let varName = List.nth l 1 in
          let newt = List.filter (var_undeclared_predicate varName) t in
          h :: process_error_list newt typList origin
        else 
          let varName = List.nth l 3 in
          let newt = List.filter (var_undeclared_predicate varName) t in
          h :: process_error_list newt typList origin
                
    | FunctionUndeclared -> let newt = fnct_undeclared_helper h t singleQuote in
        h :: process_error_list newt typList origin

    | UndefType -> let l = Str.split singleQuote h.msg in
        let func = (List.nth l (List.length l - 2)) in
        let newt = List.filter (fnct_undeclared_predicate h func) t in 
        h :: process_error_list newt typList origin
                
    | IncompatiblePtrType -> let newt = incmpt_ptr_type_helper h t in
        h :: process_error_list newt typList origin
                
    | ExcessElements-> 
        let newt = List.filter (excess_element_predicate h origin) t in
        h :: process_error_list newt typList origin
                
    | UnknownField -> let newt = unknown_field_helper h t origin singleQuote in
        h :: process_error_list newt typList origin
                
    | SizeUnknown -> let remove = size_unknown_helper h t singleQuote in
        if remove then process_error_list t typList origin else
        h :: process_error_list t typList origin
                
    | UnusedVar -> let x = unused_variable_helper h t origin in
        if fst x >= 1 then (let l2 = snd x in process_error_list t typList l2)
        else let l2 = snd x in h :: process_error_list t typList l2

    | TooManyArgs -> let l = Str.split singleQuote h.msg in
  		  let func = List.nth l (List.length l - 2) in
  		  let newt = List.filter 
  		  (fun x -> not (x.typ = TooManyArgs && has_substr x.msg func)) t in
  		  h :: process_error_list newt typList origin

    | TooFewArgs -> let newt = too_few_args_helper h t in 
        h :: process_error_list newt typList origin

    | NotInRecordOrUnion -> let nearNote = find_type_line origin h Note in
        if ((h.lineNumber - nearNote) <= 3 && (h.lineNumber - nearNote) >= 0) 
        then process_error_list t typList origin else
        h :: process_error_list t typList origin
    
    | NonConstantInitElem -> let newt = inconstant_init_helper h t in
      h :: process_error_list newt typList origin

    | BracesAroundScalar -> h :: process_error_list t typList origin

    | ImproperArrayIndex -> 
        (try let remove = not_in_struct_or_union h origin in
        let l = Str.split (Str.regexp "\n") h.msg in
        let remove = remove || (List.length l) <= 1 in
        if remove then process_error_list t typList origin else
          h :: process_error_list t typList origin
        with Not_found ->
          h :: process_error_list t typList origin)
    
    | MemberNotInStructOrUnion -> (try let remove = not_in_struct_or_union h origin in
        if remove then process_error_list t typList origin else
        h :: process_error_list t typList origin
        with Not_found ->
          h :: process_error_list t typList origin)

    | Note -> if has_substr h.msg "in expansion of macro" then
        let newt = macro_note_helper h t in
        h :: process_error_list newt typList origin else
        h :: process_error_list t typList origin

    | IncompElementType ->  h :: process_error_list t typList origin

    | InitButIncompType -> let newt = init_incomp_type_helper h t in
        h :: process_error_list newt typList origin

    | TooManyMacro -> let newt = too_many_macro_helper h t in
        h :: process_error_list newt typList origin

    | UnknownTypName -> let newt = unknown_typ_name_helper h t singleQuote in
        h :: process_error_list newt typList origin

    | CompOfDistinctPtrTypes -> let newt = comp_dstnct_ptr_types_helper h t in
        h :: process_error_list newt typList origin

    | VoidValueNotIgnored -> let newt = void_val_not_ignored_helper h t in
        h :: process_error_list newt typList origin

    | PtrFromInteger -> let newt = ptr_from_int_helper h t in
        h :: process_error_list newt typList origin

    | HasIncompleteType -> let newt = has_incomp_type_helper h t in
        h :: process_error_list newt typList origin

    | UsedStruct -> h :: process_error_list t typList origin

    | _ ->  let x = h.typ :: typList in 
            h :: process_error_list t x origin

(*____________________________________Main___________________________________*)

let anonymous s = 
  match s with
  | "" -> failwith "Please specify a file"
  | _ -> file := s
;;
Arg.parse options anonymous usage;;
try 
  let i = open_in !file in
  let x = ref [] in 
  read_input_strings 0 "" x i;
  x := List.filter (fun x -> not (x = "")) !x;
  let y = List.rev (process_strings !x) in
  let typList = buildTypList y in
  let z = process_error_list y typList y in
  Printf.printf "Final:\n";
  if !verbose then
    (ignore(List.map print_err_verbose z);)
  else
    ignore(List.map print_err z);
    Printf.printf "Done Processing\n"
with Invalid_argument("index out of bounds") -> 
    print_string "Please specify a file to process\n"; exit 1
