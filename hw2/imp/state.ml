
(* A state is a finite map from variables to integers *)
type state = (string, int) Hashtbl.t

(* Make a new empty state *)
let make () = Hashtbl.create 11

(* Look up a variable by name, return value *)
(* Raises Not_found if no binding *)
let lookup (s : state) (var : string) : int =
  try Hashtbl.find s var
  with Not_found -> failwith ("Uninitialized variable " ^ var)

(* Rebind var to value in state *)
let rebind (s : state) (var : string) (value: int) : state =
  Hashtbl.remove s var; Hashtbl.add s var value; s
