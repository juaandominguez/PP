
open Parsing;;
open Lexing;;

open Context;;
open Lib;;
open Arith;;
open Command;;
open Parser;;
open Lexer;;

let rec loop ctx =
  print_string ">> ";
  try
    let cmd = s token (from_string (read_line ())) in
    loop (run ctx cmd)
  with 
      Lexical_error ->
        print_endline "Lexical error";
        loop ctx

    | Parse_error ->
        print_endline "Syntax error";
        loop ctx

    | No_binding s ->
        print_endline ("Variable " ^ s ^ " not defined");
        loop ctx

    | Function_not_defined s ->
        print_endline ("Function " ^ s ^ " not defined");
        loop ctx

    | End_of_program
    | End_of_file ->
        ()

;;


let _ = print_endline "Floating point calculator..." in

let _ = loop empty_context in

print_endline "... bye!!!";;