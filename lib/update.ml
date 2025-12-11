open Minttea

type search_msg = SearchResults of (Model.search_result list, string) result

let handle_search_results model results =
  match results with
  | Ok rs ->
      let mode = if rs = [] || Model.is_no_results rs then Model.Searching else Model.Browsing in
      let sorted_rs = List.sort (fun a b -> compare b.Model.seeders a.Model.seeders) rs in
      { model with Model.results = sorted_rs; loading = false; error = None; mode; selected_index = 0; scroll_offset = 0 }
  | Error err -> { model with Model.loading = false; error = Some err }

let perform_search query =
  match Lwt_main.run (Api.search query) with
  | Ok results -> SearchResults (Ok results)
  | Error err -> SearchResults (Error err)

let copy_selected model =
  if model.Model.results = [] || Model.is_no_results model.results then (model, Command.Noop)
  else
    let selected = List.nth model.results model.selected_index in
    let magnet = Magnet.make ~info_hash:selected.info_hash ~name:selected.name in
    match Clipboard.copy_to_clipboard magnet with
    | Ok () ->
        let truncated_name =
          if String.length selected.name > 40 then String.sub selected.name 0 37 ^ "..."
          else selected.name
        in
        ( { model with mode = Notification (Printf.sprintf "Copied: %s" truncated_name) },
          Command.Noop )
    | Error _ -> ({ model with error = Some "Failed to copy to clipboard" }, Command.Noop)

let update event (model : Model.model) =
  match event with
  | Event.KeyDown (Key "q") when model.mode <> Searching ->
      (model, Command.Quit)
  | Event.KeyDown Escape when model.mode <> Searching ->
      (model, Command.Quit)
  | Event.KeyDown (Key "q") when model.mode = Searching && model.query = "" ->
      (model, Command.Quit)
  | Event.KeyDown _ when match model.mode with Notification _ -> true | _ -> false ->
      ({ model with mode = Browsing }, Command.Noop)
  | Event.KeyDown Enter when model.mode = Searching ->
      if model.query = "" then (model, Command.Noop)
      else
        let model = { model with loading = true; error = None } in
        let SearchResults results = perform_search model.query in
        (handle_search_results model results, Command.Noop)
  | Event.KeyDown Enter when model.mode = Browsing -> copy_selected model
  | Event.KeyDown (Key "j") when model.mode = Browsing ->
      let max_idx = max 0 (List.length model.results - 1) in
      let new_idx = min (model.selected_index + 1) max_idx in
      let new_scroll =
        if new_idx >= model.scroll_offset + Model.page_size then model.scroll_offset + 1
        else model.scroll_offset
      in
      ({ model with selected_index = new_idx; scroll_offset = new_scroll }, Command.Noop)
  | Event.KeyDown Down when model.mode = Browsing ->
      let max_idx = max 0 (List.length model.results - 1) in
      let new_idx = min (model.selected_index + 1) max_idx in
      let new_scroll =
        if new_idx >= model.scroll_offset + Model.page_size then model.scroll_offset + 1
        else model.scroll_offset
      in
      ({ model with selected_index = new_idx; scroll_offset = new_scroll }, Command.Noop)
  | Event.KeyDown (Key "k") when model.mode = Browsing ->
      let new_idx = max 0 (model.selected_index - 1) in
      let new_scroll =
        if new_idx < model.scroll_offset then model.scroll_offset - 1
        else model.scroll_offset
      in
      ({ model with selected_index = new_idx; scroll_offset = new_scroll }, Command.Noop)
  | Event.KeyDown Up when model.mode = Browsing ->
      let new_idx = max 0 (model.selected_index - 1) in
      let new_scroll =
        if new_idx < model.scroll_offset then model.scroll_offset - 1
        else model.scroll_offset
      in
      ({ model with selected_index = new_idx; scroll_offset = new_scroll }, Command.Noop)
  | Event.KeyDown (Key "/") when model.mode = Browsing ->
      ({ model with mode = Searching }, Command.Noop)
  | Event.KeyDown (Key "i") when model.mode = Browsing ->
      ({ model with mode = Searching }, Command.Noop)
  | Event.KeyDown Escape when model.mode = Searching && model.query <> "" ->
      ({ model with query = "" }, Command.Noop)
  | Event.KeyDown Escape when model.mode = Searching && model.results <> [] ->
      ({ model with mode = Browsing }, Command.Noop)
  | Event.KeyDown Backspace when model.mode = Searching ->
      let query =
        if String.length model.query > 0 then
          String.sub model.query 0 (String.length model.query - 1)
        else ""
      in
      ({ model with query }, Command.Noop)
  | Event.KeyDown Space when model.mode = Searching ->
      ({ model with query = model.query ^ " " }, Command.Noop)
  | Event.KeyDown (Key c) when model.mode = Searching && String.length c = 1 ->
      ({ model with query = model.query ^ c }, Command.Noop)
  | _ -> (model, Command.Noop)
