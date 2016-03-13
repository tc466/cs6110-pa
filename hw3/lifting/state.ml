
open Ast

(* A state is a finite map from variables to integers *)
(* implementation uses association lists *)
type state = (id * value) list
(* Values are ints,  bools, and closures *)
and value = Number of int | Boolean of bool | Closure of exp * state | Error

(* Make a new empty state *)
let make () = []

(* Look up a variable by name, return value *)
let lookup (s : state) (var : id) : value =
  try List.assoc var s
  with Not_found -> failwith ("Uninitialized variable " ^ var)

(* Rebind var to value in state *)
let update (s : state) (var : id) (value : value) : state =
  (var, value) :: s
  
(* Recursive update - used in let rec *)
let rec_update (v : value) : value * state =
  match v with
    | Closure (g, (id, Error) :: t) ->
      let rec u = (id, Closure (g, u)) :: t in (lookup u id, u)
    | _ -> failwith "Improper value for let rec"

(* Produce bindings as an ossociation list *)
let bindings (s : state) : (id * value) list = s

let to_string (v : value) : string =
  match v with
    | Number n -> string_of_int n
    | Boolean b -> string_of_bool b
    | Closure (e, _) -> Ast.to_string e
    | Error -> failwith "System error - error value encountered"