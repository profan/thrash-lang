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
  | "if"                { IF }
  | "do"                { DO }
  | "else"              { ELSE }
  | "end"               { END }
  | "while"             { WHILE }
  | "for"               { FOR }
  | "let"               { LET }
  | "in"                { IN }
  | "="                 { EQUAL }
  | ";"                 { SEMICOLON }
  | ['a'-'z']* as lxm   { VAR(lxm) }
  | _                   { raise (SyntaxError("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof                 { EOF }
  | 'q' | 'e'           { raise Eof }
