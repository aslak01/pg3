let copy_to_clipboard text =
  let is_macos =
    let ic = Unix.open_process_in "uname -s" in
    let os = input_line ic in
    close_in ic;
    os = "Darwin"
  in
  let cmd =
    if is_macos then Printf.sprintf "printf '%%s' %s | pbcopy" (Filename.quote text)
    else Printf.sprintf "printf '%%s' %s | xclip -selection clipboard" (Filename.quote text)
  in
  let exit_code = Unix.system cmd in
  match exit_code with Unix.WEXITED 0 -> Ok () | _ -> Error "Failed to copy to clipboard"
