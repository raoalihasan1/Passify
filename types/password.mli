open! Core
open! Async

type t [@@deriving sexp]

val create : string -> t Deferred.t
val strength_as_string : t -> string
val rules_not_satisfied_as_string : t -> string
