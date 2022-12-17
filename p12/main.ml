
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

  ...

  let cmd = s token (from_string (read_line ())) in

  ...

;;

let _ = print_endline "Floating point calculator..." in
let _ = loop empty_context in
print_endline "... bye!!!";;

