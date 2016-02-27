
open Ast
open State
    
(* Evaluation according to big-step semantics *)

(* evaluate an arithmetic expression in a state *)
let rec eval_a (a : aexp) (s : state) : int =
  failwith "Implement me!"

(* evaluate a boolean expression in a state *)
let rec eval_b (b : bexp) (s : state) : bool =
  failwith "Implement me!"

(* evaluate a command in a state *)
let rec eval_c (c : com) (s : state) : state =
  failwith "Implement me!"
