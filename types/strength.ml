open! Core

type t = Weak | Moderate | Strong [@@deriving sexp, to_string]

let of_rules_not_satisfied rules =
  let percentage_satisfied =
    Float.(
      (1. - (of_int (Rules.no_not_satisfied rules) / of_int Rules.count)) * 100.)
  in
  match percentage_satisfied with
  | x when Float.(x >= 75.) -> Strong
  | x when Float.(x >= 50.) -> Moderate
  | (_ : float) -> Weak
