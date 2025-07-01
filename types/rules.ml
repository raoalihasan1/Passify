open! Core
open! Async

module Conditions = struct
  type t =
    [ `At_least_12_chars
    | `Uppercase_and_lowercase
    | `Numbers_and_symbols
    | `Not_in_dict ]
  [@@deriving enumerate, sexp]

  let query_dictionary password =
    let clean_password =
      password |> String.filter ~f:Char.is_alpha |> String.lowercase
    in
    let uri =
      Uri.of_string
        [%string
          "https://api.dictionaryapi.dev/api/v2/entries/en/%{clean_password}"]
    in
    let client_res = Cohttp_async.Client.get uri in
    Deferred.map client_res ~f:(fun (response, (_ : Cohttp_async.Body.t)) ->
        response |> Cohttp.Response.status |> Cohttp.Code.code_of_status
        |> Cohttp.Code.is_success)

  let checker t password =
    match t with
    | `At_least_12_chars -> Deferred.return (String.length password >= 12)
    | `Uppercase_and_lowercase ->
        let uppercase_exists = String.find password ~f:Char.is_uppercase
        and lowercase_exists = String.find password ~f:Char.is_lowercase in
        Deferred.return
          (Option.is_some uppercase_exists && Option.is_some lowercase_exists)
    | `Numbers_and_symbols ->
        let digit_exists = String.find password ~f:Char.is_digit
        and symbol_exists =
          String.find password ~f:(fun char -> not (Char.is_alphanum char))
        in
        Deferred.return
          (Option.is_some digit_exists && Option.is_some symbol_exists)
    | `Not_in_dict -> Deferred.map ~f:not (query_dictionary password)

  let to_string = function
    | `At_least_12_chars -> "At Least 12 Characters Length"
    | `Uppercase_and_lowercase -> "Use Upper & Lowercase Letters"
    | `Numbers_and_symbols -> "Use Numbers & Symbols"
    | `Not_in_dict -> "Use Words Not In The Dictionary"
end

type t = Conditions.t list [@@deriving sexp]

let count = List.length Conditions.all

let not_satisfied password =
  Deferred.List.filter Conditions.all ~how:`Sequential ~f:(fun condition ->
      Deferred.map (Conditions.checker condition password) ~f:not)

let not_satisfied_to_string t =
  t |> List.map ~f:Conditions.to_string |> String.concat ~sep:"\n"

let no_not_satisfied = List.length
