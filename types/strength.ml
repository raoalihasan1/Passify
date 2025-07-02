open! Core
open! Async

type t = Weak | Moderate | Strong [@@deriving sexp, to_string]

let of_rules_not_satisfied rules =
  let percentage_satisfied =
    Float.(
      (1. - (of_int (Rules.no_not_satisfied rules) / of_int Rules.count)) * 100.)
  in
  (* Password is strong if it satisfies >=75% of the rules, moderate
     if it satisfies >=50% of the rules, and weak otherwise *)
  match percentage_satisfied with
  | x when Float.(x >= 75.) -> Strong
  | x when Float.(x >= 50.) -> Moderate
  | (_ : float) -> Weak

let%expect_test "Password is weak in strength" =
  Deferred.map (Rules.not_satisfied "Kitten1") ~f:(fun rules_not_satisfied ->
      let strength = of_rules_not_satisfied rules_not_satisfied in
      print_s [%message (strength : t)];
      [%expect {| (strength Weak) |}])

let%expect_test "Password is moderate in strength" =
  Deferred.map (Rules.not_satisfied "Kitten1!") ~f:(fun rules_not_satisfied ->
      let strength = of_rules_not_satisfied rules_not_satisfied in
      print_s [%message (strength : t)];
      [%expect {| (strength Moderate) |}])

let%expect_test "Password is strong in strength" =
  Deferred.map (Rules.not_satisfied "Word_doesnt_exist!")
    ~f:(fun rules_not_satisfied ->
      let strength = of_rules_not_satisfied rules_not_satisfied in
      print_s [%message (strength : t)];
      [%expect {| (strength Strong) |}])
