open Spices

let title_style = default |> bold true |> fg (color "#7C3AED")
let help_style = default |> faint true
let selected_style = default |> bold true |> fg (color "#10B981")
let normal_style = default
let error_style = default |> fg (color "#EF4444")
let loading_style = default |> faint true |> italic true
let notification_style = default |> bold true |> fg (color "#10B981") |> bg (color "#1F2937")
let seeders_style = default |> fg (color "#10B981")
let leechers_style = default |> fg (color "#F59E0B")
let size_style = default |> faint true

let render_result ~selected (result : Model.search_result) =
  let prefix = if selected then "► " else "  " in
  let style = if selected then selected_style else normal_style in
  let name =
    let max_len = 50 in
    if String.length result.name > max_len then String.sub result.name 0 (max_len - 3) ^ "..."
    else result.name
  in
  let size_str = Model.format_size result.size in
  let seeders = build seeders_style "S:%d" result.seeders in
  let leechers = build leechers_style "L:%d" result.leechers in
  let size = build size_style "%8s" size_str in
  Printf.sprintf "%s%s  %s  %s %s" prefix (build style "%-50s" name) size seeders leechers

let render (model : Model.model) =
  let title = build title_style "pg - Torrent Search" in
  let help =
    match model.mode with
    | Searching -> build help_style "Enter: search, Esc: clear, q: quit"
    | Browsing -> build help_style "j/k: navigate, Enter: copy magnet, /: search, q: quit"
    | Notification _ -> build help_style "Press any key to continue"
  in
  let input_prefix =
    match model.mode with Searching -> "> " | _ -> "  "
  in
  let cursor = match model.mode with Searching -> "_" | _ -> "" in
  let input_line = Printf.sprintf "%s%s%s" input_prefix model.query cursor in
  let notification_line =
    match model.mode with
    | Notification msg -> "\n" ^ build notification_style " %s " msg ^ "\n"
    | _ -> ""
  in
  let error_line =
    match model.error with
    | Some err -> "\n" ^ build error_style "Error: %s" err
    | None -> ""
  in
  let loading_line =
    if model.loading then "\n" ^ build loading_style "Searching..."
    else ""
  in
  let results_section =
    if model.loading then ""
    else if Model.is_no_results model.results then "\nNo results found."
    else if model.results = [] then ""
    else
      let total = List.length model.results in
      let visible_results =
        model.results
        |> List.filteri (fun i _ -> i >= model.scroll_offset && i < model.scroll_offset + Model.page_size)
      in
      let lines =
        visible_results
        |> List.mapi (fun i r ->
            let actual_index = model.scroll_offset + i in
            render_result ~selected:(actual_index = model.selected_index) r)
      in
      let pagination_info =
        let start_idx = model.scroll_offset + 1 in
        let end_idx = min (model.scroll_offset + Model.page_size) total in
        build help_style "\n[%d-%d of %d]" start_idx end_idx total
      in
      "\n\nResults:\n" ^ String.concat "\n" lines ^ pagination_info
  in
  [ title; help; ""; input_line; notification_line; error_line; loading_line; results_section ]
  |> List.filter (fun s -> s <> "")
  |> String.concat "\n"
