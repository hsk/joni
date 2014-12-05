{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']

rule token = parse
| space+ { token lexbuf }
| "/*" { comment lexbuf }
| "//" _* ['\n' '\r'] { token lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACK }
| ']' { RBRACK }
| "return" { RETURN }
| "::" { CAST }
| '@' { AT }
| "new" { NEW }
| "package" { PACKAGE(import lexbuf) }
| "import" { IMPORT(import lexbuf) }
| "static" { STATIC }
| "public" { PUBLIC }
| "protected" { PROTECTED }
| "private" { PRIVATE }
| "final" { FINAL }
| "this" { THIS }
| "class" { CLASS }
| ":>" { EXTENDS }
| "<:" { REXTENDS }
| "match" { MATCH }
| '|' { OR }
| "=>" { ARROW }
| "trait" { TRAIT }
| "if" { IF }
| "else" { ELSE }
| digit+ { INT(int_of_string (Lexing.lexeme lexbuf)) }

| '^' { HAT }
| "♡" { HART }
| '.' { DOT }
| ',' { COMMA }
| ';' { SEMICOLON }
| ':' { COLON }
| '=' { ASSIGN }

| '<' { LT }
| '>' { GT }
| "<=" { LE }
| ">=" { GE }
| "==" { EQ }
| "!=" { NE }

| '+' { ADD }
| '-' { SUB }
| '*' { MUL }
| '/' { DIV }
| '#' { SHARP }

| '"' [^ '"']* '"' { STRING(Lexing.lexeme lexbuf) }
| ['a'-'z' 'A'-'Z' '_' '$' ]['a'-'z' 'A'-'Z' '_' '$' '0'-'9']*
    { ID(Lexing.lexeme lexbuf) }
| eof { EOF }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }

and import = parse
| space+ { import lexbuf }
| ['a'-'z' 'A'-'Z' '*' '.']* { Lexing.lexeme lexbuf }
| eof { Lexing.lexeme lexbuf }
| _
    { failwith
      (Printf.sprintf "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)) }

and comment = parse
| "*/" { token lexbuf }
| _ { comment lexbuf }
