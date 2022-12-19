type token =
  | EOF
  | QUIT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | REMAINDER
  | POWER
  | EQUAL
  | LPAREN
  | RPAREN
  | FLOAT of (float)
  | ID of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Command.command
