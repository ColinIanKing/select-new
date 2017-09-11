let target = ref "v4.6"
let linux = "/run/shm/linux"


let _ =
    let cmd = Array.get Sys.argv 0 in
    let cmddir = Filename.dirname cmd in
    let tdir = cmddir ^ "/templates" in
    let args = List.tl (Array.to_list Sys.argv) in
    Report.do_report !target linux tdir args
