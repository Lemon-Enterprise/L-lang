type literal =
  | Int_l of int
  | Float_l of float
  | String_l of string
  | Char_l of char
  | Bool_l of bool
  | Null_l

type bin_op =

  | Add | Sub | Mul | Div | Equal | Greater | Less | GreaterEq | LessEq

type expr =
  | Literal of literal
  | Identifier of string
  | BinaryOp of bin_op * expr * expr
  | Call of string * expr list
  | Lambda of string list * stmt list
  | Match of expr * (expr * stmt list) list

and stmt =
  | VarDecl of { name : string; value : expr }
  | LetDecl of { name : string; value : expr }
  | FuncDecl of { name : string; params : string list; body : stmt list }
  | FncDecl of { name : string; params : string list; body : stmt list }
  | IfStmt of { condition : expr; then_branch : stmt list; elseif_branches : (expr * stmt list) list; else_branch : stmt list option }
  | WhileStmt of { condition : expr; body : stmt list }
  | DoWhileStmt of { body : stmt list; condition : expr }
  | ForInStmt of { var_name : string; iter_expr : expr; body : stmt list }
  | ClassDecl of { name : string; members : stmt list }
  | EnumDecl of { name : string; cases : string list }
  | ImplDecl of { namespace : string; body : stmt list }
  | Directive of { name : string; args : string list }
  | ExprStmt of expr
  | Break
  | Continue

type program = stmt list

val string_of_program : program -> string
