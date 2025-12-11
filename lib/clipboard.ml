let is_macos =
  lazy (
    let ic = Unix.open_process_in "uname -s" in
    let os = input_line ic in
    close_in ic;
    os = "Darwin"
  )

let copy_to_clipboard text =
  let clipboard_cmd =
    if Lazy.force is_macos then "pbcopy"
    else "xclip -selection clipboard"
  in
  (* Write text to a temp file since we can't use pipes with spawn *)
  let tmp = Filename.temp_file "clip" ".txt" in
  let oc = open_out_bin tmp in
  output_string oc text;
  close_out oc;
  (* Use Spawn which uses vfork (safe with OCaml 5 domains).
     SIGCHLD is blocked globally at startup, so no signal will interrupt poll(). *)
  let cmd = Printf.sprintf "cat %s | %s; rm -f %s" (Filename.quote tmp) clipboard_cmd (Filename.quote tmp) in
  let pid = Spawn.spawn ~prog:"/bin/sh" ~argv:[ "/bin/sh"; "-c"; cmd ] () in
  (* Wait for child - SIGCHLD is blocked so waitpid won't be interrupted *)
  let rec wait () =
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 -> Ok ()
    | _, _ -> Error "Clipboard command failed"
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  wait ()
