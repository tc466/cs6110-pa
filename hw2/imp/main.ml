(* read-eval-print loop *)

open Ast
open Eval

exception Quit

(* current program file and parsed program *)
let file : string option ref = ref None
let program : com option ref = ref None

let open_in (file : string) : in_channel =
  try open_in file
  with Sys_error s -> failwith ("Cannot open file: " ^ s)
  
(* Command handlers *)
let load (filename : string) : unit =
  let ch = open_in filename in
  file := Some filename;
  let parse : com =
    try Parser.main Lexer.token (Lexing.from_channel ch)
    with _ -> close_in ch; failwith "Cannot parse program" in
  program := Some parse;
  close_in ch
  
let list() : unit =
  let rec copy_lines (ch : in_channel) : unit =
    print_endline (input_line ch);
    copy_lines ch in
  match !file with
    | None -> failwith "No program loaded"
    | Some f ->
        let ch = open_in f in
        try copy_lines ch
        with End_of_file -> close_in ch        

let run() : unit =
  match !program with
    | Some p -> ignore (eval_c p (State.make()))
    | None -> failwith "No program loaded"  

let help() : unit =
  print_endline "Available commands are:";
  print_endline "load <file>, list, run, help, quit"

let quit() : unit =
  print_endline "bye";
  raise Quit

(* main read-eval-print loop *)
let rec repl() : unit =
  print_string ">> ";
  (try
    let input = Str.split (Str.regexp "[ \t]+") (read_line()) in
    match input with
      | [] -> ()
      | cmd :: args ->
        match (cmd, args) with
          | ("load", [filename]) -> load filename
          | ("list", []) -> list()
          | ("run", []) -> run()
          | ("quit", []) -> quit()
          | _ -> help()
  with Failure s -> print_endline s);
  repl()
  
let _ =
  print_endline "IMP interactive interpreter version 2016.0";
  try repl()
  with Quit -> ()
