(* File lexer.mll *)
rule token = parse
  | [' ' '\t'] { token lexbuf }
  | '(' {Parser.LPAREN}
  | ')' {Parser.RPAREN}
  | '\\' {Parser.LAMBDA}
  | '.' {Parser.PERIOD}
  | ':' {Parser.COLON}
  | "->" {Parser.ARROW}
  | "succ" {Parser.SUCC}
  | "pred" {Parser.PRED}
  | "let" {Parser.LET}
  | "=" {Parser.EQUALS}
  | "as" {Parser.AS}
  | "in" {Parser.IN}
  | "0" {Parser.ZERO}
  | ['a'-'z''A'-'Z']+ as lxm { Parser.VAR(lxm) }
  | eof {Parser.EOF}
