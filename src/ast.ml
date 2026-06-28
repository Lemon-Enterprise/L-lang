type literal =
  | Int_l of int
  | Float_l of float
  | String_l of string
  | Char_l of char
  | Bool_l of bool
  | Null_l

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Equal
  | Greater
  | Less
  | GreaterEq
  | LessEq

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
  | IfStmt of {
      condition : expr;
      then_branch : stmt list;
      elseif_branches : (expr * stmt list) list;
      else_branch : stmt list option;
    }
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

let string_of_literal = function
  | Int_l i -> string_of_int i
  | Float_l f -> string_of_float f
  | String_l s -> "\"" ^ s ^ "\""
  | Char_l c -> "'" ^ String.make 1 c ^ "'"
  | Bool_l b -> string_of_bool b
  | Null_l -> "null"

let string_of_bin_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Greater -> ">"
  | Less -> "<"
  | GreaterEq -> ">="
  | LessEq -> "<="

let rec string_of_expr = function
  | Literal l -> string_of_literal l
  | Identifier s -> s
  | BinaryOp (op, l, r) ->
      "(" ^ string_of_expr l ^ " " ^ string_of_bin_op op ^ " "
      ^ string_of_expr r ^ ")"
  | Call (f, args) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Lambda (params, body) ->
      "fnc(" ^ String.concat ", " params ^ ") { " ^ string_of_program body
      ^ " }"
  | Match (e, _) -> "switch (" ^ string_of_expr e ^ ") { ... }"

and string_of_stmt = function
  | VarDecl { name; value } ->
      "var " ^ name ^ " = " ^ string_of_expr value ^ ";"
  | LetDecl { name; value } ->
      "let " ^ name ^ " = " ^ string_of_expr value ^ ";"
  | FuncDecl { name; params; body } ->
      "function " ^ name ^ "(" ^ String.concat ", " params ^ ") { "
      ^ string_of_program body ^ " }"
  | FncDecl { name; params; body } ->
      "fnc " ^ name ^ "(" ^ String.concat ", " params ^ ") { "
      ^ string_of_program body ^ " }"
  | IfStmt { condition; then_branch; elseif_branches; else_branch } ->
      let cond_s = string_of_expr condition in
      let then_s = string_of_program then_branch in
      let elseifs_s =
        List.map
          (fun (c, b) ->
            "elseif (" ^ string_of_expr c ^ ") { " ^ string_of_program b ^ " }")
          elseif_branches
      in
      let else_s =
        match else_branch with
        | Some b -> " else { " ^ string_of_program b ^ " }"
        | None -> ""
      in
      "if (" ^ cond_s ^ ") { " ^ then_s ^ " }" ^ String.concat "" elseifs_s
      ^ else_s
  | WhileStmt _ -> "while (...) { ... }"
  | DoWhileStmt _ -> "do { ... } while (...);"
  | ForInStmt { var_name; iter_expr; _ } ->
      "for " ^ var_name ^ " in " ^ string_of_expr iter_expr ^ " { ... }"
  | ClassDecl { name; _ } -> "class " ^ name ^ " { ... }"
  | EnumDecl { name; _ } -> "enum " ^ name ^ " { ... }"
  | ImplDecl { namespace; _ } -> "impl " ^ namespace ^ " { ... }"
  | Directive { name; _ } -> "@" ^ name ^ " (...)"
  | ExprStmt e -> string_of_expr e ^ ";"
  | Break -> "break;"
  | Continue -> "continue;"

and string_of_program p = String.concat " " (List.map string_of_stmt p)
