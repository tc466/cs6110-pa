
open Util
open Ast
open State

let rec is_target_exp (e : exp) : bool =
  match e with 
    | Var _ -> true
    | Fun  _ -> false 
    | Let _ -> false
    | Letrec _ -> false
    | App (e1, e2) -> is_target_exp e1 && is_target_exp e2
    | Cond (b, e1, e2) -> is_target_exp b && is_target_exp e1 && is_target_exp e2
    | Num _ -> true
    | Plus (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Minus (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Times (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Div (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Mod (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | And (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Or (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Not b -> is_target_exp b
    | Eq (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Leq (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Lt (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Bool _ -> true

let rec is_target_fun (e : exp) : bool = 
  match e with
    | Fun(_,(Fun _ as e1)) -> is_target_fun e1
    | Fun(_,e1) -> is_target_exp e1
    | _ -> false

let rec is_target_prog (e : exp) : bool = 
  match e with 
    | Let(_,e1,p2) -> is_target_fun e1 && is_target_prog p2
    | Letrec(_,e1,p2) -> is_target_fun e1 && is_target_prog p2
    | _ -> is_target_exp e

(*
TODO:
 - don't convert if already target program/expression
*)
let convert (e : exp) (s : state) : exp * state =
  let fresh = Fresh.make(allv e) in
  let rec create_app (e : exp) (v : id list) : exp = (
    match v with
    | [] -> e
    | h :: t -> create_app (App(e, Var(h))) t ) in
  let rec head_of_app (e : exp) : id = (
    match e with
    | Var(x) -> x
    | App(e1, e2) -> head_of_app e1
    | _ -> failwith "Cannot find head of app" ) in
  let rec arg_slice (args : id list) (n : int) : id list = (
    if n = 0 then args else
    match args with
    | [] -> failwith "args list too short"
    | h :: t -> arg_slice t (n-1) ) in
  let rec convert_aux (e : exp) (s : state): exp * state =
    match e with
    | Let ([f], e1, e2) ->
      let (e2c, s1) = convert_aux e2 s in
      let (e1c, s2) = convert_aux e1 s1 in
      if HashSet.mem (fv e2c) f then ( subst e1c f e2c, s2 ) else
      let fn = Fresh.next fresh in
      let e2cfv = HashSet.values (fv e2c) in
      let s3 = update s2 fn (Closure(Fun(e2cfv, e2c), s2)) in
      let ff = create_app (Var fn) e2cfv in
      ( subst e1c f ff, s3 )
    | Let (f :: t, e1, e2) ->
      let ee = Let([f], Fun(t, e1), e2) in
      convert_aux ee s
    | Letrec ([f], e1, e2) ->
      let (e2c, s1) = convert_aux e2 s in
      let (e1c, s2) = convert_aux e1 s1 in
      (* print_endline (Ast.to_string e1c); *)
      (* Name of recursive function in lifted version *)
      let fn = Fresh.next fresh in
      (* Get the name of the lifted recursive function body *)
      let recfn = head_of_app e1c in
      let e1cfvnorec = recfn :: (HashSet.values (fv (Fun([recfn], (Fun([f],e1c)))))) in
      (* Get the args of the orignal recursive function *)
      let recbody = lookup s2 recfn in
      let (recargs, recargs_extra) =
        match recbody with
        | Closure (g, _) -> (
          match g with
          | Fun(args, _) -> (args, arg_slice args (List.length e1cfvnorec))
          | _ -> failwith "Incorrect recursive body" )
        | _ -> failwith "Recursive body not found" in
      let fnargs = List.concat [e1cfvnorec; recargs_extra] in
      let (e1cfvnorec1, e1cfvnorec2) = (
        match e1cfvnorec with
        | [] -> failwith "Recursive function name not found"
        | h :: t -> (h, t) ) in
      let fnbody1 = create_app (App(Var(fn), Var(e1cfvnorec1))) e1cfvnorec2 in
      let fnbody = subst fnbody1 f (create_app (Var(e1cfvnorec1)) recargs) in
      (* print_endline (Ast.to_string fnbody1); *)
      (* print_endline (Ast.to_string fnbody); *)
      let s3 = update s2 fn (Closure(Fun(fnargs, fnbody), s2)) in
      let fn2 = Fresh.next fresh in
      let e2cfv = HashSet.values (fv e2c) in
      let s4 = update s3 fn2 (Closure(Fun(e2cfv, e2c), s3)) in
      let ff = create_app (Var fn2) e2cfv in
      (* print_endline (Ast.to_string e1c); *)
      ( subst fnbody1 f ff, s4 )
    | Letrec (f :: t, e1, e2) ->
      let ee = Letrec([f], Fun(t, e1), e2) in
      convert_aux ee s
    | Fun (x, e) ->
      let (e1, s1) = convert_aux e s in
      let e1fv = HashSet.values (fv (Fun(x,e1))) in
      let fn = Fresh.next fresh in
      let fnbody = Fun(List.concat [e1fv; x], e1) in
      (* print_endline (Ast.to_string fnbody); *)
      let e2 = create_app (Var fn) e1fv in
      let s2 = update s1 fn (Closure(fnbody, s1)) in
      ( e2, s2 )
    | App(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( App(e1c, e2c), s2 )
    | Cond(cond, e1, e2) ->
      let (condc, s1) = convert_aux cond s in
      let (e1c,   s2) = convert_aux e1 s1 in
      let (e2c,   s3) = convert_aux e2 s2 in
      ( Cond(condc, e1c, e2c), s3 )
    | Plus(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Plus(e1c, e2c), s2 )
    | Minus(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Minus(e1c, e2c), s2 )
    | Times(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Times(e1c, e2c), s2 )
    | Div(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Div(e1c, e2c), s2 )
    | Mod(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Mod(e1c, e2c), s2 )
    | And(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( And(e1c, e2c), s2 )
    | Or(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Or(e1c, e2c), s2 )
    | Eq(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Eq(e1c, e2c), s2 )
    | Leq(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Leq(e1c, e2c), s2 )
    | Lt(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s in
      let (e2c, s2) = convert_aux e2 s1 in
      ( Lt(e1c, e2c), s2 )
    | Not(e1) ->
      let (e1c, s1) = convert_aux e1 s in
      ( Not(e1c), s1 )
    | _ -> (e, s) in
  convert_aux e s

let lift (e : exp) : exp =
  let s = make() in
  let (e1, s1) = convert e s in
  let rec lift_aux = function
    | [] -> e1
    | (f, e2) :: t ->
      match e2 with
      | Closure (g, _) ->
        if HashSet.mem (fv g) f then
          Letrec([f], g, lift_aux t)
        else
          Let([f], g, lift_aux t)
      | _ -> failwith "Improper value in state" in
  lift_aux (List.rev (bindings s1))

