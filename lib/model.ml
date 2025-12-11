type search_result = {
  id : string;
  name : string;
  info_hash : string;
  leechers : int;
  seeders : int;
  num_files : int;
  size : int64;
  username : string;
  added : int;
  status : string;
  category : string;
  imdb : string option;
}

type mode =
  | Searching
  | Browsing
  | Notification of string

type model = {
  query : string;
  results : search_result list;
  selected_index : int;
  scroll_offset : int;
  mode : mode;
  loading : bool;
  error : string option;
}

let page_size = 15

let make ?initial_query () =
  let query = Option.value ~default:"" initial_query in
  let mode = if query = "" then Searching else Searching in
  {
    query;
    results = [];
    selected_index = 0;
    scroll_offset = 0;
    mode;
    loading = query <> "";
    error = None;
  }

let is_no_results results =
  match results with
  | [ r ] -> r.id = "0"
  | _ -> false

let format_size bytes =
  let kb = 1024L in
  let mb = Int64.mul kb 1024L in
  let gb = Int64.mul mb 1024L in
  let tb = Int64.mul gb 1024L in
  if bytes >= tb then
    Printf.sprintf "%.1f TB" (Int64.to_float bytes /. Int64.to_float tb)
  else if bytes >= gb then
    Printf.sprintf "%.1f GB" (Int64.to_float bytes /. Int64.to_float gb)
  else if bytes >= mb then
    Printf.sprintf "%.1f MB" (Int64.to_float bytes /. Int64.to_float mb)
  else if bytes >= kb then
    Printf.sprintf "%.1f KB" (Int64.to_float bytes /. Int64.to_float kb)
  else Printf.sprintf "%Ld B" bytes
