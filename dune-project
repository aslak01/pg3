(lang dune 3.16)

(name pg)

(generate_opam_files true)

(source
 (github aslak01/pg))

(authors "aslak01")

(maintainers "aslak01")

(license MIT)

(package
 (name pg)
 (synopsis "Terminal torrent search")
 (description "A TUI for searching torrents with vim-style navigation")
 (depends
  (ocaml (>= 5.1))
  dune
  minttea
  spices
  leaves
  cohttp-lwt-unix
  tls-lwt
  lwt
  yojson
  uri
  spawn))
