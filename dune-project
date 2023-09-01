(lang dune 3.7)
  (name cps_parser)
  (generate_opam_files true)
  (authors "Author Name")
  (maintainers "Maintainer Name")
  (package
    (name cps_parser)
    (synopsis "A short synopsis")
    (description "A longer description")
    (depends ocaml dune (alcotest :with-test))
    (tags
      (topics "to describe" your project)
    )
  )
