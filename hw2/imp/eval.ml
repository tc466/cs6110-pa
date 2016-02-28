
open Ast
open State
    
(* Evaluation according to big-step semantics *)

(* evaluate an arithmetic expression in a state *)
let rec eval_a (a : aexp) (s : state) : int =
  match a with
  | Var x -> lookup s x
  | Number n -> n
  | Plus (a1, a2) -> eval_a a1 s + eval_a a2 s
  | Minus (a1, a2) -> eval_a a1 s - eval_a a2 s
  | Times (a1, a2) -> eval_a a1 s * eval_a a2 s
  | Div (a1, a2) -> eval_a a1 s / eval_a a2 s
  | Mod (a1, a2) -> eval_a a1 s mod eval_a a2 s
  | Input -> print_string "? "; int_of_string (read_line())

(* evaluate a boolean expression in a state *)
let rec eval_b (b : bexp) (s : state) : bool =
  match b with
  | Eq (a1, a2) -> eval_a a1 s = eval_a a2 s
  | Leq (a1, a2) -> eval_a a1 s <= eval_a a2 s
  | Lt (a1, a2) -> eval_a a1 s < eval_a a2 s
  | Not b -> not (eval_b b s)
  | And (b1, b2) -> (eval_b b1 s) && (eval_b b2 s)
  | Or (b1, b2) -> (eval_b b1 s) || (eval_b b2 s)
  | True -> true
  | False -> false

(* evaluate a command in a state *)
let rec eval_c (c : com) (s : state) : state =
  match c with
  | While (b, c1) -> if eval_b b s then
                       let s1 = eval_c c1 s in
                         eval_c c s1
                     else s
  | For (a, c1) -> let n = eval_a a s in
                     if n > 0 then
                       let s1 = eval_c c1 s in
                         eval_c (For( Number(n-1), c1 )) s1
                     else
                       s
  | Cond (b, c1, c2) -> if eval_b b s then
                          eval_c c1 s
                        else
                          eval_c c2 s
  | Comp (c1, c2) -> let s1 = eval_c c1 s in
                       eval_c c2 s1
  | Assg (var, a) -> let n = eval_a a s in
                       rebind s var n
  | Print a -> let n = eval_a a s in
               let str = string_of_int n in
                 print_endline str; s
  | Skip -> s