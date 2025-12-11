open Lwt.Syntax

let base_url = "https://apibay.org/q.php"

let parse_result json =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> to_string in
  let name = json |> member "name" |> to_string in
  let info_hash = json |> member "info_hash" |> to_string in
  let leechers = json |> member "leechers" |> to_string |> int_of_string in
  let seeders = json |> member "seeders" |> to_string |> int_of_string in
  let num_files = json |> member "num_files" |> to_string |> int_of_string in
  let size = json |> member "size" |> to_string |> Int64.of_string in
  let username = json |> member "username" |> to_string in
  let added = json |> member "added" |> to_string |> int_of_string in
  let status = json |> member "status" |> to_string in
  let category = json |> member "category" |> to_string in
  let imdb =
    let s = json |> member "imdb" |> to_string in
    if s = "" then None else Some s
  in
  Model.
    {
      id;
      name;
      info_hash;
      leechers;
      seeders;
      num_files;
      size;
      username;
      added;
      status;
      category;
      imdb;
    }

let search query =
  let encoded_query = Uri.pct_encode query in
  let url = Uri.of_string (Printf.sprintf "%s?q=%s" base_url encoded_query) in
  let* resp, body = Cohttp_lwt_unix.Client.get url in
  let status = Cohttp.Response.status resp in
  if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
    let* body_str = Cohttp_lwt.Body.to_string body in
    try
      let json = Yojson.Safe.from_string body_str in
      let results =
        match json with
        | `List items -> List.map parse_result items
        | _ -> []
      in
      Lwt.return_ok results
    with exn -> Lwt.return_error (Printexc.to_string exn)
  else Lwt.return_error (Printf.sprintf "HTTP error: %s" (Cohttp.Code.string_of_status status))
