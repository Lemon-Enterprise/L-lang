type t =
  | VAR
  | LET
  | FUNCTION
  | FNC
  | IF
  | ELSE
  | ELSEIF
  | WHILE
  | DO
  | CLASS
  | ENUM
  | IMPL
  | PUB
  | PRIV
  | PROT
  | SWITCH
  | CASE
  | BREAK
  | CONTINUE
  | DEFAULT
  | TRY
  | CATCH
  | FROM
  | FOR
  | IN
  | AS
  | NULL
  | INT of int
  | STRING of string
  | FLOAT of float
  | CHAR of char
  | BOOL of bool
  | IDENT of string
  | ATSIGN_DIRECTIVE of string
  | ASSIGN
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | GREATERTHAN
  | LESSTHAN
  | GREATERTHAN_OR_EQUAL_TO
  | LESSTHAN_OR_EQUAL_TO
  | ARROW
  | ILLEGAL
  | EOF
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET

type token_info = { type_ : t; line : int; column : int }

let to_string = function
  | VAR -> "VAR"
  | LET -> "LET"
  | FUNCTION -> "FUNCTION"
  | FNC -> "FNC"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | ELSEIF -> "ELSEIF"
  | WHILE -> "WHILE"
  | DO -> "DO"
  | CLASS -> "CLASS"
  | ENUM -> "ENUM"
  | IMPL -> "IMPL"
  | PUB -> "PUB"
  | PRIV -> "PRIV"
  | PROT -> "PROT"
  | SWITCH -> "SWITCH"
  | CASE -> "CASE"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | DEFAULT -> "DEFAULT"
  | TRY -> "TRY"
  | CATCH -> "CATCH"
  | FROM -> "FROM"
  | FOR -> "FOR"
  | IN -> "IN"
  | AS -> "AS"
  | NULL -> "NULL"
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | STRING s -> "STRING(\"" ^ s ^ "\")"
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | CHAR c -> "CHAR('" ^ String.make 1 c ^ "')"
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | ATSIGN_DIRECTIVE s -> "ATSIGN_DIRECTIVE(@" ^ s ^ ")"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULTIPLY -> "MULTIPLY"
  | DIVIDE -> "DIVIDE"
  | GREATERTHAN -> "GREATERTHAN"
  | LESSTHAN -> "LESSTHAN"
  | GREATERTHAN_OR_EQUAL_TO -> "GREATERTHAN_OR_EQUAL_TO"
  | LESSTHAN_OR_EQUAL_TO -> "LESSTHAN_OR_EQUAL_TO"
  | ARROW -> "ARROW"
  | ILLEGAL -> "ILLEGAL"
  | EOF -> "EOF"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"

let lookup_keyword = function
  | "var" -> VAR
  | "let" -> LET
  | "function" -> FUNCTION
  | "fnc" -> FNC
  | "if" -> IF
  | "else" -> ELSE
  | "elseif" -> ELSEIF
  | "while" -> WHILE
  | "do" -> DO
  | "class" -> CLASS
  | "enum" -> ENUM
  | "impl" -> IMPL
  | "pub" -> PUB
  | "priv" -> PRIV
  | "prot" -> PROT
  | "switch" -> SWITCH
  | "case" -> CASE
  | "break" -> BREAK
  | "continue" -> CONTINUE
  | "default" -> DEFAULT
  | "try" -> TRY
  | "catch" -> CATCH
  | "from" -> FROM
  | "for" -> FOR
  | "in" -> IN
  | "as" -> AS
  | "null" -> NULL
  | "true" -> BOOL true
  | "false" -> BOOL false
  | identifier -> IDENT identifier

let lookup_atsign = function
  | "macro" -> ATSIGN_DIRECTIVE "macro"
  | "nostd" -> ATSIGN_DIRECTIVE "nostd"
  | "nomain" -> ATSIGN_DIRECTIVE "nomain"
  | "import" -> ATSIGN_DIRECTIVE "import"
  | "export" -> ATSIGN_DIRECTIVE "export"
  | unknown -> ATSIGN_DIRECTIVE unknown
