open Pg_lib

(* Block SIGCHLD at program startup, before any domains/threads are spawned.
   All subsequently spawned threads will inherit this signal mask, preventing
   SIGCHLD from interrupting poll() in the Riot scheduler. *)
let () = ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigchld ])

let initial_query () =
  match Array.to_list Sys.argv with
  | _ :: args when args <> [] -> Some (String.concat " " args)
  | _ -> None

let app =
  Minttea.app
    ~init:(fun _ -> Minttea.Command.Hide_cursor)
    ~update:Update.update
    ~view:View.render
    ()

let () =
  let query = initial_query () in
  let initial_model = Model.make ?initial_query:query () in
  let initial_model =
    match query with
    | Some q when q <> "" ->
        let Update.SearchResults results = Update.perform_search q in
        Update.handle_search_results initial_model results
    | _ -> initial_model
  in
  Minttea.start app ~initial_model
