{

  open Grammar_parser

  exception SyntaxError of string
  exception Eof

}

rule token = parse
    [' ' '\t' '\n']     { token lexbuf } (* skip blanks, also newlines *)
  | ['0'-'9']+ as lxm   { INT(int_of_string lxm) }
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
  | "let"               { LET }
  | "in"                { IN }
  | "and"               { AND }
  | "or"                { OR }
  | '='                 { EQ }
  | '<'                 { LT }
  | '>'                 { GT }
  | ';'                 { SEMICOLON }
  | ','                 { COMMA }
  | ['a'-'z']+ as lxm   { VAR(lxm) }
  | eof                 { EOF }
  | 'q' | 'e'           { raise Eof }
  | _                   { raise (SyntaxError("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
