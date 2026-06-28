open Token

type t = {
  lexer : Lexer.t;
  mutable cur_token : token_info;
  mutable peek_token : token_info;
}

type precedence = LOWEST | EQUALS | LESSGREATER | SUM | PRODUCT

let get_precedence = function
  | ASSIGN -> EQUALS
  | GREATERTHAN | LESSTHAN | GREATERTHAN_OR_EQUAL_TO | LESSTHAN_OR_EQUAL_TO ->
      LESSGREATER
  | PLUS | MINUS -> SUM
  | MULTIPLY | DIVIDE -> PRODUCT
  | _ -> LOWEST

let token_precedence p = get_precedence p.cur_token.type_
let peek_precedence p = get_precedence p.peek_token.type_

let next_token p =
  p.cur_token <- p.peek_token;
  p.peek_token <- Lexer.next_token p.lexer

let init l =
  let p =
    {
      lexer = l;
      cur_token = { type_ = EOF; line = 0; column = 0 };
      peek_token = { type_ = EOF; line = 0; column = 0 };
    }
  in
  next_token p;
  next_token p;
  p

let cur_token_is p t_type = p.cur_token.type_ = t_type
let peek_token_is p t_type = p.peek_token.type_ = t_type

let expect_peek p t_type =
  if peek_token_is p t_type then begin
    next_token p;
    true
  end
  else false

let get_bin_op = function
  | PLUS -> Ast.Add
  | MINUS -> Ast.Sub
  | MULTIPLY -> Ast.Mul
  | DIVIDE -> Ast.Div
  | ASSIGN -> Ast.Equal
  | GREATERTHAN -> Ast.Greater
  | LESSTHAN -> Ast.Less
  | GREATERTHAN_OR_EQUAL_TO -> Ast.GreaterEq
  | LESSTHAN_OR_EQUAL_TO -> Ast.LessEq
  | _ -> failwith "Not a binary operator"

let rec parse_expr p precedence =
  let initial =
    match p.cur_token.type_ with
    | IDENT s -> Some (Ast.Identifier s)
    | INT i -> Some (Ast.Literal (Ast.Int_l i))
    | _ -> None
  in
  match initial with
  | None -> None
  | Some left_expr -> Some (parse_infix p left_expr precedence)

and parse_infix p left precedence =
  let left_acc = ref left in
  while (not (peek_token_is p SEMICOLON)) && precedence < peek_precedence p do
    match p.peek_token.type_ with
    | PLUS | MINUS | MULTIPLY | DIVIDE | ASSIGN | GREATERTHAN | LESSTHAN
    | GREATERTHAN_OR_EQUAL_TO | LESSTHAN_OR_EQUAL_TO ->
        next_token p;
        let op = get_bin_op p.cur_token.type_ in
        let cur_prec = token_precedence p in
        next_token p;
        begin match parse_expr p cur_prec with
        | Some right -> left_acc := Ast.BinaryOp (op, !left_acc, right)
        | None -> ()
        end
    | _ -> ()
  done;
  !left_acc

and parse_block_statement p =
  let rec block_loop acc =
    if cur_token_is p RBRACE || cur_token_is p EOF then List.rev acc
    else
      match parse_statement p with
      | Some stmt ->
          next_token p;
          block_loop (stmt :: acc)
      | None ->
          next_token p;
          block_loop acc
  in
  block_loop []

and parse_declaration p is_let =
  match p.peek_token.type_ with
  | IDENT name ->
      next_token p;
      if expect_peek p ASSIGN then begin
        next_token p;
        match parse_expr p LOWEST with
        | Some value ->
            if peek_token_is p SEMICOLON then next_token p;
            if is_let then Some (Ast.LetDecl { name; value })
            else Some (Ast.VarDecl { name; value })
        | None -> None
      end
      else None
  | _ -> None

and parse_if_statement p =
  if not (expect_peek p LPAREN) then None
  else begin
    next_token p;
    match parse_expr p LOWEST with
    | None -> None
    | Some condition ->
        if (not (expect_peek p RPAREN)) || not (expect_peek p LBRACE) then None
        else begin
          next_token p;
          let then_branch = parse_block_statement p in
          if cur_token_is p RBRACE then next_token p;
          let rec parse_elseifs acc =
            if cur_token_is p ELSEIF then
              begin if not (expect_peek p LPAREN) then acc
              else begin
                next_token p;
                match parse_expr p LOWEST with
                | Some cond ->
                    if
                      (not (expect_peek p RPAREN)) || not (expect_peek p LBRACE)
                    then acc
                    else begin
                      next_token p;
                      let block = parse_block_statement p in
                      if cur_token_is p RBRACE then next_token p;
                      parse_elseifs ((cond, block) :: acc)
                    end
                | None -> acc
              end
              end
            else acc
          in
          let elseif_branches = List.rev (parse_elseifs []) in
          let else_branch =
            if cur_token_is p ELSE then
              begin if expect_peek p LBRACE then begin
                next_token p;
                let block = parse_block_statement p in
                if cur_token_is p RBRACE then next_token p;
                Some block
              end
              else None
              end
            else None
          in
          Some
            (Ast.IfStmt { condition; then_branch; elseif_branches; else_branch })
        end
  end

and parse_statement p =
  match p.cur_token.type_ with
  | LET -> parse_declaration p true
  | VAR -> parse_declaration p false
  | IF -> parse_if_statement p
  | _ -> None

let parse_program p =
  let rec program_loop acc =
    if cur_token_is p EOF then List.rev acc
    else
      match parse_statement p with
      | Some stmt ->
          next_token p;
          program_loop (stmt :: acc)
      | None ->
          next_token p;
          program_loop acc
  in
  program_loop []
