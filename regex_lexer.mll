{
open Regex_parser
open Lexing

exception SyntaxError of string
}

let digit = ['1'-'9']
let syntaxcharacter = ['^' '$' '\\' '.' '*' '+' '?' '(' ')' '[' ']' '{' '}' '|']
let patterncharacter = _#syntaxcharacter

rule token = parse
| '|' { ALT }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRAC }
| '}' { RBRAC }
| ',' { COMMA }
| '*' { STAR }
| '+' { PLUS }
| '?' { QMARK }
| '^' { HAT }
| '$' { DOLLAR }
| '\\' { BACKSL }
| '.' { DOT }
| ':' { COLONS }
| '<' { LESS }
| '>' { MORE }
| '=' { EQUAL }
| '-' { MINUS }
| '!' { EXCL }
| 'b' { LOWB }
| 'B' { UPB }
| 'd'  { LOWD }
| 'D'  { UPD }
| 's' { LOWS }
| 'S' { UPS }
| 'w' { LOWW }
| 'W' { UPW }
| 'f' { LOWF }
| 'n' { LOWN }
| 'r' { LOWR }
| 't' { LOWT }
| 'v' { LOWV }
| 'k' { LOWK }
| 'x' { LOWX }
| 'u' { LOWU }
| 'p' { LOWP }
| 'P' { UPP }
| '0' { ZERO }
| digit as d { NZDIGIT d }
| patterncharacter as c { CHAR c }
| eof { EOF }