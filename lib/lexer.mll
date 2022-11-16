{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char
}

let spaces = [' ' '\t']+
let digit = ['0'-'9']
let integer = digit+
let letter = ['a'-'z' 'A'-'Z']
let underline = '_'
let identifier = letter (letter | digit | underline)*

rule token = parse
 | spaces            { token lexbuf }
 | '\n'              { L.new_line lexbuf; token lexbuf }
 | '#' [^ '\n']*     { token lexbuf }
 | '('               { LPAREN }
 | ')'               { RPAREN }
 | '='               { EQ }
 | ','               { COMMA }
 | ';'               { SEMICOLON }
 | ":="              { ASSIGN }
 | '+'               { PLUS }
 | '-'               { MINUS }
 | '*'               { TIMES }
 | '/'               { DIV }
 | '%'               { REST }
 | "=="              { EQUAL }
 | "<>"              { NEQUAL }
 | '<'               { LT }
 | "<="              { LE }
 | '>'               { GT }
 | ">="              { GE }
 | "and"             { AND }
 | "or"              { OR }
 | "if"              { IF }
 | "then"            { THEN }
 | "else"            { ELSE }
 | "while"           { WHILE }
 | "do"              { DO }
 | "let"             { LET }
 | "in"              { IN }
 | "int"             { INT }
 | "bool"            { BOOL }
 | "unit"            { UNIT }
 | "false"           { LITBOOL false }
 | "true"            { LITBOOL true }
 | integer as lxm    { LITINT (int_of_string lxm) }
 | identifier as lxm { ID (Symbol.symbol lxm) }
 | eof               { EOF }
 | _                 { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
