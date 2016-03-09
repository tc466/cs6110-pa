
open Ast

(* A state is a finite map from variables to integers *)
type state
(* Values are ints,  bools, and closures *)
type value = Number of int | Boolean of bool | Closure of exp * state | Error

(* Make a new empty state *)
val make : unit -> state

(* Look up a variable by name, return value *)
(* Raises Not_found if no binding *)
val lookup : state -> id -> value

(* Rebind var to value in state *)
val update : state -> id -> value -> state

(* Recursive update - used in let rec *)
val rec_update : value -> value * state

(* Produce bindings as an ossociation list *)
val bindings : state -> (id * value) list

val to_string : value -> string
