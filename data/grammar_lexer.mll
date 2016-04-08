{

  open Grammar_parser

  exception SyntaxError of string
  exception Eof

}

let blank = [' ' '\t' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha (alpha | digit | '_')*

rule token = parse
    blank               { token lexbuf } (* skip blanks, also newlines *)
  | digit+ as lxm       { INT(int_of_string lxm) }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '/'                 { DIV }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '['                 { LBRACKET }
  | ']'                 { RBRACKET }
  | "true"              { BOOL(true) }
  | "false"             { BOOL(false) }
  | "if"                { IF }
  | "do"                { DO }
  | "else"              { ELSE }
  | "end"               { END }
  | "while"             { WHILE }
  | "for"               { FOR }
  | "cond"              { COND }
  | "match"             { MATCH }
  | "break"             { BREAK }
  | "continue"          { CONTINUE }
  | "let"               { LET }
  | "in"                { IN }
  | "and"               { AND }
  | "or"                { OR }
  | '='                 { EQ }
  | '<'                 { LT }
  | '>'                 { GT }
  | "->"                { ARROW }
  | ';'                 { SEMICOLON }
  | ','                 { COMMA }
  | id as lxm           { VAR(lxm) }
  | eof                 { EOF }
  | 'q' | 'e'           { raise Eof }
  | _                   { raise (SyntaxError("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
