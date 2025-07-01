open! Core
open! Async
module Password = Passify_types.Password

let run =
  Command.async ~summary:"Examine the strength of your inputted password"
    [%map.Command
      let password =
        Command.Param.(
          flag "password" (required string) ~doc:"The password to examine")
      and as_sexp =
        Command.Param.(
          flag "sexp" (optional_with_default false bool) ~doc:"Output as sexp")
      in
      fun () ->
        let format_to_table =
          let cols =
            [
              ("Password", fun (_ : Password.t) -> password);
              ("Strength", Password.strength_as_string);
              ("Potential Improvements", Password.rules_not_satisfied_as_string);
            ]
            |> List.map ~f:(fun (name, f) ->
                   Ascii_table.Column.create ~show:`If_not_empty name f)
          in
          Ascii_table.output cols ~oc:Out_channel.stdout
        in
        Deferred.map (Password.create password) ~f:(fun password ->
            if as_sexp then
              print_endline (Sexp.to_string_hum (Password.sexp_of_t password))
            else format_to_table [ password ])]
