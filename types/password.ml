open! Core
open! Async

type t = {
  password : string;
  rules_not_satisfied : Rules.t;
  strength : Strength.t;
}
[@@deriving fields ~getters ~iterators:create, sexp]

let create password =
  let not_satisfied = Rules.not_satisfied password in
  Deferred.map not_satisfied ~f:(fun rules_not_satisfied ->
      Fields.create ~password ~rules_not_satisfied
        ~strength:(Strength.of_rules_not_satisfied rules_not_satisfied))

let strength_as_string t = t |> strength |> Strength.to_string

let rules_not_satisfied_as_string t =
  t |> rules_not_satisfied |> Rules.not_satisfied_to_string
