(* some utilities *)

(* option monad *)
let option_map (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with
  | Some a -> Some (f a)
  | None -> None
  
(*****************************************************
 * HashSet -- like in Java
 *****************************************************)
module type HashSet = sig
  type 'a t
  val make : unit -> 'a t
  val add : 'a t -> 'a -> unit
  val remove : 'a t -> 'a -> unit
  val mem : 'a t -> 'a -> bool
  val size : 'a t -> int
end

module HashSet : HashSet = struct
  type 'a t = ('a, 'a) Hashtbl.t
  let make() : 'a t = Hashtbl.create 16
  let add (h : 'a t) (x : 'a) = Hashtbl.add h x x
  let remove (h : 'a t) (x : 'a) =
    while Hashtbl.mem h x do
      Hashtbl.remove h x
    done
  let mem (h : 'a t) (x : 'a) = Hashtbl.mem h x
  let size (h : 'a t) : int = Hashtbl.length h
end

(*****************************************************
 * Stream of strings in length-lexicographic order --
 * use to create new variable names
 *****************************************************)
module type LexStream = sig
  type t
  val make : unit -> t
  val next : t -> string
end

module LexStream : LexStream = struct
  type t = int list ref 
  
  let rec inc (s : int list) : int list =
    match s with
      | [] -> [Char.code 'a']
      | x :: t ->
          if x < Char.code 'z' then (x + 1) :: t
          else Char.code 'a' :: inc t
        
  let make() : t = ref [Char.code 'a']

  let next (h : t) : string =
    let l = !h in
    h := inc l;
    String.concat "" (List.map (String.make 1) (List.map Char.chr (List.rev l)))
end
  
