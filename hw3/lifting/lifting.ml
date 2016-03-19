
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

let convert (e : exp) (s : state) : exp * state =
  let fresh = Fresh.make(HashSet.make()) in
  let rec create_app (e : exp) (v : id list) : exp = (
    match v with
    | [] -> e
    | h :: t -> create_app (App(e, Var(h))) t ) in
  let rec convert_aux (e : exp) (s : state) (r : id list): exp * state =
    match e with
    | Let ([f], e1, e2) ->
      let (e2c, s1) = convert_aux e2 s r in
      let (e1c, s2) = convert_aux e1 s1 r in
      if HashSet.mem (fv e2c) f then ( subst e1c f e2c, s2 ) else
      let fn = Fresh.next fresh in
      let e2cfv = HashSet.values (fv e2c) in
      let s3 = update s2 fn (Closure(Fun(e2cfv, e2c), s2)) in
      let ff = create_app (Var fn) e2cfv in
      ( subst e1c f ff, s3 )
    | Let (f :: t, e1, e2) ->
      let ee = Let([f], Fun(t, e1), e2) in
      convert_aux ee s r
    | Letrec ([f], e1, e2) ->
      let (e2c, s1) = convert_aux e2 s r in
      let (e1c, s2) = convert_aux e1 s1 r in
      if HashSet.mem (fv e2c) f then ( subst e1c f e2c, s2 ) else
      let fn = Fresh.next fresh in
      let e2cfv = HashSet.values (fv e2c) in
      let s3 = update s2 fn (Closure(Fun(e2cfv, e2c), s2)) in
      let ff = create_app (Var fn) e2cfv in
      ( subst e1c f ff, s3 )
    | Letrec (f :: t, e1, e2) ->
      let ee = Letrec([f], Fun(t, e1), e2) in
      convert_aux ee s r
    | Fun (x, e) ->
      let (e1, s1) = convert_aux e s r in
      let e1fv = HashSet.values (fv (Fun(x,e1))) in
      let fn = Fresh.next fresh in
      ( create_app (Var fn) e1fv, update s1 fn (Closure(Fun(List.concat [e1fv; x], e1), s1)) )
    | App(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( App(e1c, e2c), s2 )
    | Cond(cond, e1, e2) ->
      let (condc, s1) = convert_aux cond s r in
      let (e1c,   s2) = convert_aux e1 s1 r in
      let (e2c,   s3) = convert_aux e2 s2 r in
      ( Cond(condc, e1c, e2c), s3 )
    | Plus(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Plus(e1c, e2c), s2 )
    | Minus(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Minus(e1c, e2c), s2 )
    | Times(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Times(e1c, e2c), s2 )
    | Div(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Div(e1c, e2c), s2 )
    | Mod(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Mod(e1c, e2c), s2 )
    | And(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( And(e1c, e2c), s2 )
    | Or(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Or(e1c, e2c), s2 )
    | Eq(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Eq(e1c, e2c), s2 )
    | Leq(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Leq(e1c, e2c), s2 )
    | Lt(e1, e2) ->
      let (e1c, s1) = convert_aux e1 s r in
      let (e2c, s2) = convert_aux e2 s1 r in
      ( Lt(e1c, e2c), s2 )
    | Not(e1) ->
      let (e1c, s1) = convert_aux e1 s r in
      ( Not(e1c), s1 )
    | _ -> (e, s) in
  convert_aux e s []

let lift (e : exp) : exp =
  let s = make() in
  let (e1, s1) = convert e s in
  let rec lift_aux = function
    | [] -> e1
    | (f, e2) :: t ->
      match e2 with
      | Closure (g, _) -> Let([f], g, lift_aux t)
      | _ -> failwith "Improper value in state" in
  lift_aux (List.rev (bindings s1))

