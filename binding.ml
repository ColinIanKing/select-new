(* Bind gcc reduce type to report types *)

module T = Types (* GCC reduce types *)
module R = Report (* Report Types *)

let warn err =
  Printf.eprintf "warning: not handling\n%s\n" err.T.msg;
  R.Unknown

let fail err =
  Printf.eprintf "FAILURE: should not occur\n%s\n" err.T.msg;
  R.Unknown

let quote s = "\""^s^"\""

let unknown_type s =
    match Str.split (Str.regexp "[ \t]+") s with
    | ("struct"|"union"|"enum")::_ -> R.UnknownType(quote s)
    | [s] -> R.UnknownTypedef(s)
    | _ -> R.UnknownType(quote s)

let safe_unknown_type err =
    let ty = List.hd err.T.data in
    match Str.split (Str.regexp "[ \t]+") ty with
    | ("struct"|"union"|"enum")::_ -> R.UnknownType(quote ty)
    | _ -> warn err

let parse_gcc_reduce_err err =
    if err.T.fromnotes
    then R.UnknownFunction(List.hd err.T.data)
    else match err.T.typ with
    | T.Unknown -> R.Unknown
    | T.DeclaredInParameterList -> unknown_type (List.hd err.T.data)
    | T.HasNoMember ->
        R.UnknownField(quote (List.hd err.T.data), List.nth err.T.data 1)
    | T.PointerToIncompleteType -> safe_unknown_type err
    | T.Sizeof -> safe_unknown_type err
    | T.VariableUndeclared -> (
        match err.T.data with
        | [] -> warn err
        | [v] -> R.UnknownVariable(v)
        | _ -> fail err
    )
    | T.FunctionUndeclared -> (
        match err.T.data with
        | [fn] -> R.UnknownFunction(fn)
        | [fn;alt] -> R.UnknownFunction2(fn, alt)
        | _ -> fail err
    )
    | T.UndefType -> unknown_type(quote(List.hd err.T.data))
    | T.BadCallback -> (
        match err.T.data with
        | [ty;fld] -> R.BadInitType(quote ty, fld)
        | _ -> warn err
    )
    | T.BadNonfnInit -> (
        match err.T.data with
        | [ty;fld] -> R.BadNonfnInit(quote ty, fld)
        | _ -> warn err
    )
    | T.IncompatiblePtrType -> warn err
    | T.IncompatibleArgType ->
        R.BadArgType(quote(List.hd err.T.data), List.nth err.T.data 1)
    | T.BadContainerOf -> (
        match err.T.data with
        | ["container_of"] -> warn err
        | [ty;fld] -> R.UnknownFieldOrType(quote ty, fld)
        | [ty] -> unknown_type ty
        | _ -> fail err
    )
    | T.InvalidBinary -> warn err
    | T.UnknownField -> (
        match err.T.data with
        | [fld] -> R.UnknownFieldGeneric(fld)
        | [str;fld] -> R.UnknownField(quote str, fld)
        | _ -> fail err
    )
    | T.SizeUnknown -> unknown_type (List.hd err.T.data)
    | T.TooManyArgs -> R.TooManyArgs(quote(List.hd err.T.data))
    | T.TooFewArgs -> R.TooFewArgs(quote(List.hd err.T.data))
    | T.NonConstantInitElem -> (
        match err.T.data with
        | [] -> warn err
        | [v] -> R.UnknownVariable(v)
        | _ -> fail err
    )
    | T.BracesAroundScalar -> fail err
    | T.MemberNotInStructOrUnion -> fail err
    | T.IncompElementType -> unknown_type (List.hd err.T.data)
    | T.TooManyMacro -> R.TooManyArgs(quote(List.hd err.T.data))
    | T.TooFewMacro -> R.TooManyArgs(quote(List.hd err.T.data))
    | T.WrongFnDecl -> warn err
    | T.ExpectedParen -> R.UnknownTypedef(List.hd err.T.data)
    | T.ParamNoType -> warn err
    | T.UnknownTypName -> R.UnknownTypedef(List.hd err.T.data)
    | T.CompOfDistinctPtrTypes -> warn err
    | T.HasIncompleteType -> unknown_type (List.hd err.T.data)
    | T.VoidValueNotIgnored -> (
        match err.T.data with
        | [name] -> R.Void1(name)
        | [ty;fld] -> R.Void2(ty, fld)
        | _ -> failwith "wrong number of args"
    )
    | T.BadContext ->
        R.TypeChange2(quote(List.hd err.T.data), List.nth err.T.data 1)
    | T.AssignPtrAndInteger -> warn err
    | T.InitPtrAndInteger -> R.UnknownVariable(List.hd err.T.data)
    | T.PtrAndInteger ->
        R.BadArgType(quote(List.hd err.T.data), List.nth err.T.data 1)
    | T.UsedStruct -> warn err
    | T.IncompTypes -> warn err
    | T.NotLoadComputable -> R.UnknownVariable(quote(List.hd err.T.data))
    | T.InitButIncompType -> unknown_type (List.hd err.T.data)
    | _ -> fail err

