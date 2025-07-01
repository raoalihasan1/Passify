open! Core
open! Async

type t [@@deriving sexp]

val count : int
val not_satisfied : string -> t Deferred.t
val not_satisfied_to_string : t -> string
val no_not_satisfied : t -> int
