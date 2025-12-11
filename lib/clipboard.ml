let is_macos =
  lazy (
    try
      let ic = Unix.open_process_in "uname -s" in
      let os = try input_line ic with End_of_file -> "" in
      ignore (Unix.close_process_in ic);
      os = "Darwin"
    with _ -> false
  )

let copy_to_clipboard text =
  let clipboard_cmd =
    if Lazy.force is_macos then "pbcopy"
    else "xclip -selection clipboard"
  in
  (* Write text to a temp file since we can't use pipes with spawn *)
  let tmp = Filename.temp_file "clip" ".txt" in
  Fun.protect ~finally:(fun () -> try Sys.remove tmp with _ -> ()) (fun () ->
    let oc = open_out_bin tmp in
    output_string oc text;
    close_out oc;
    (* Use Spawn which uses vfork (safe with OCaml 5 domains).
       SIGCHLD is blocked globally at startup, so no signal will interrupt poll(). *)
    let cmd = Printf.sprintf "cat %s | %s" (Filename.quote tmp) clipboard_cmd in
    let pid = Spawn.spawn ~prog:"/bin/sh" ~argv:[ "/bin/sh"; "-c"; cmd ] () in
    (* Wait for child - SIGCHLD is blocked so waitpid won't be interrupted *)
    let rec wait () =
      match Unix.waitpid [] pid with
      | _, Unix.WEXITED 0 -> Ok ()
      | _, _ -> Error "Clipboard command failed"
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
    in
    wait ()
  )
