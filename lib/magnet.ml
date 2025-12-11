let trackers =
  [
    "udp://tracker.opentrackr.org:1337/announce";
    "udp://open.stealth.si:80/announce";
    "udp://tracker.torrent.eu.org:451/announce";
    "udp://tracker.bittor.pw:1337/announce";
    "udp://public.popcorn-tracker.org:6969/announce";
    "udp://tracker.dler.org:6969/announce";
    "udp://exodus.desync.com:6969";
    "udp://open.demonii.com:1337/announce";
  ]

let make ~info_hash ~name =
  let encoded_name = Uri.pct_encode name in
  let tracker_params =
    trackers |> List.map (fun t -> "&tr=" ^ Uri.pct_encode t) |> String.concat ""
  in
  Printf.sprintf "magnet:?xt=urn:btih:%s&dn=%s%s" info_hash encoded_name
    tracker_params
