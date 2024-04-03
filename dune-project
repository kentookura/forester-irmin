(lang dune 3.7)

(using menhir 2.0)

(name irmin-forester)

(authors "Kento Okura")

(maintainers "Kento Okura")

(version 3.1.0)

(generate_opam_files true)

(license GPL-3.0-or-later)

(package
 (name irmin-forester)
 (synopsis "")
 (depends
  (menhir
   (>= 20211230))
  (ocaml
   (>= 5.0.0))
  dune
  (forester (>= 3.1.0))
  irmin-cli
  irmin-watcher 
  asai
  lwt))
