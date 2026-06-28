type t =

  | VAR | LET | FUNCTION | FNC | IF | ELSE | ELSEIF | WHILE | DO | CLASS
  | ENUM | IMPL | PUB | PRIV | PROT | SWITCH | CASE | BREAK | CONTINUE
  | DEFAULT | TRY | CATCH | FROM | FOR | IN | AS | NULL
  | INT of int | STRING of string | FLOAT of float | CHAR of char | BOOL of bool
  | IDENT of string | ATSIGN_DIRECTIVE of string
  | ASSIGN | PLUS | MINUS | MULTIPLY | DIVIDE | GREATERTHAN | LESSTHAN
  | GREATERTHAN_OR_EQUAL_TO | LESSTHAN_OR_EQUAL_TO | ARROW | ILLEGAL | EOF
  | SEMICOLON | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET

type token_info = {
  type_ : t;
  line : int;
  column : int;
}

val to_string : t -> string
val lookup_keyword : string -> t
val lookup_atsign : string -> t
