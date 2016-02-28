
open Ast
open State
    
(* Evaluation according to big-step semantics *)

(* evaluate an arithmetic expression in a state *)
let rec eval_a (a : aexp) (s : state) : int =
  match a with
    | Var var -> lookup s var
  	| Number n -> n
  	| Plus (a1, a2) ->
  	  let n1 = eval_a a1 s in
        let n2 = eval_a a2 s in
        	n1 + n2
  	| Minus (a1, a2) ->
  	  let n1 = eval_a a1 s in
        let n2 = eval_a a2 s in
        	n1 - n2
  	| Times (a1, a2) ->
  	  let n1 = eval_a a1 s in
        let n2 = eval_a a2 s in
        	n1 * n2
  	| Div (a1, a2) ->
  	  let n1 = eval_a a1 s in
        let n2 = eval_a a2 s in
        	n1 / n2
  	| Mod (a1, a2) ->
  	  let n1 = eval_a a1 s in
        let n2 = eval_a a2 s in
        	n1 mod n2
  	| Input ->
  	  print_string "? ";
      let input = read_line() in
  	    int_of_string input	

(* evaluate a boolean expression in a state *)
let rec eval_b (b : bexp) (s : state) : bool =
  match b with
  	| Eq (a1, a2) ->
  	  let n1 = eval_a a1 s in
        let n2 = eval_a a2 s in
        	n1 = n2
  	| Leq (a1, a2) ->
  	  let n1 = eval_a a1 s in
        let n2 = eval_a a2 s in
        	n1 <= n2
  	| Lt (a1, a2) ->
  	  let n1 = eval_a a1 s in
        let n2 = eval_a a2 s in
        	n1 < n2  	
  	| Not (be) ->
  		let b = eval_b be s in
  			not b
  	| And (be1, be2) ->
  		let b1 = eval_b be1 s in
  		  let b2 = eval_b be2 s in
  		  	b1 && b2
  	| Or (be1, be2) ->
  		let b1 = eval_b be1 s in
  		  let b2 = eval_b be2 s in
  		  	b1 || b2
  	| True -> true
  	| False -> false

(* evaluate a command in a state *)
let rec eval_c (c : com) (s : state) : state =
  match c with
	  | While (be, c1) ->
	  	let b = eval_b be s in
	  		if b then
	  			let s1 = eval_c c1 s in
	  				eval_c c s1
	  		else
	  		  s
	  | For (a, c1) ->
	  	let n = eval_a a s in
	  		if n > 0 then
	  			let s1 = eval_c c1 s in
	  				let a = Number (n - 1) in 
	  					eval_c (For (a, c1)) s1
	  		else
	  			s
	  | Cond (be, c1, c2) ->
	  	let b = eval_b be s in
	  		if b then eval_c c1 s else eval_c c2 s
	  | Comp (c1, c2) ->
	    let s1 = eval_c c1 s in 
	      eval_c c2 s1
	  | Assg (var, a) ->
	    let value = eval_a a s in
	      rebind s var value
	  | Print (a) ->
	  	let n = eval_a a s in
	  	  let str = string_of_int n in
	  	    print_endline str; s
	  | Skip -> s

