open! Core

type t [@@deriving sexp, to_string]

val of_rules_not_satisfied : Rules.t -> t
