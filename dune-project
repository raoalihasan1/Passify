(lang dune 3.19)

(name passify)

(generate_opam_files true)

(source
 (github raoalihasan1/passify))

(authors "Ali Hasan <alirao2880@gmail.com>")

(maintainers "Ali Hasan <alirao2880@gmail.com>")

(package
 (name passify)
 (synopsis "A CLI application for determining the strength of a password")
 (description "Determine the strength of your password based on a set of defined rules")
 (depends ocaml dune)
 (tags
  (password strength security)))
