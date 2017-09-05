let cmd_to_list command =
  (* Open process and retrieve stdout output as list*)
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in

  (* Concatenate output lines into list *)
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in

  try process_otl_aux ()
  with End_of_file ->
    ignore (Unix.close_process_in chan);

  List.rev !res


;;
