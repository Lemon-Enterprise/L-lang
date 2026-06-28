type t = {
  input : string;
  mutable position : int;
  mutable read_position : int;
  mutable ch : char;
  mutable line : int;
  mutable column : int;
}

let advance_init l =
  if l.read_position >= String.length l.input then l.ch <- '\x00'
  else l.ch <- String.get l.input l.read_position;
  l.position <- l.read_position;
  l.read_position <- l.read_position + 1;
  l.column <- l.column + 1

let init (input : string) : t =
  let l =
    {
      input;
      position = 0;
      read_position = 0;
      ch = '\x00';
      line = 1;
      column = 0;
    }
  in
  advance_init l;
  l

let advance (l : t) : unit =
  if l.ch = '\n' then begin
    l.line <- l.line + 1;
    l.column <- 0
  end;
  if l.read_position >= String.length l.input then l.ch <- '\x00'
  else l.ch <- String.get l.input l.read_position;
  l.position <- l.read_position;
  l.read_position <- l.read_position + 1;
  l.column <- l.column + 1

let peek_char (l : t) : char =
  if l.read_position >= String.length l.input then '\x00'
  else String.get l.input l.read_position

let rec skip_whitespace (l : t) : unit =
  match l.ch with
  | ' ' | '\t' | '\n' | '\r' ->
      advance l;
      skip_whitespace l
  | _ -> ()

let is_letter (ch : char) : bool =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch = '_'

let is_digit (ch : char) : bool = ch >= '0' && ch <= '9'

let read_identifier (l : t) : string =
  let start_pos = l.position in
  while is_letter l.ch || is_digit l.ch do
    advance l
  done;
  String.sub l.input start_pos (l.position - start_pos)

let read_number (l : t) : Token.t =
  let start_pos = l.position in
  let is_float = ref false in
  while is_digit l.ch || l.ch = '.' do
    if l.ch = '.' then is_float := true;
    advance l
  done;
  let num_str = String.sub l.input start_pos (l.position - start_pos) in
  if !is_float then Token.FLOAT (float_of_string num_str)
  else Token.INT (int_of_string num_str)

let read_string (l : t) : string =
  advance l;
  let start_pos = l.position in
  while l.ch <> '"' && l.ch <> '\x00' do
    advance l
  done;
  let str = String.sub l.input start_pos (l.position - start_pos) in
  advance l;
  str

let next_token (l : t) : Token.token_info =
  skip_whitespace l;
  let tok_line = l.line in
  let tok_col = l.column in
  let t_type =
    match l.ch with
    | ';' ->
        advance l;
        Token.SEMICOLON
    | '(' ->
        advance l;
        Token.LPAREN
    | ')' ->
        advance l;
        Token.RPAREN
    | '{' ->
        advance l;
        Token.LBRACE
    | '}' ->
        advance l;
        Token.RBRACE
    | '[' ->
        advance l;
        Token.LBRACKET
    | ']' ->
        advance l;
        Token.RBRACKET
    | '=' ->
        advance l;
        Token.ASSIGN
    | '+' ->
        advance l;
        Token.PLUS
    | '*' ->
        advance l;
        Token.MULTIPLY
    | '/' ->
        advance l;
        Token.DIVIDE
    | '-' ->
        if peek_char l = '>' then begin
          advance l;
          advance l;
          Token.ARROW
        end
        else begin
          advance l;
          Token.MINUS
        end
    | '<' ->
        if peek_char l = '=' then begin
          advance l;
          advance l;
          Token.LESSTHAN_OR_EQUAL_TO
        end
        else begin
          advance l;
          Token.LESSTHAN
        end
    | '>' ->
        if peek_char l = '=' then begin
          advance l;
          advance l;
          Token.GREATERTHAN_OR_EQUAL_TO
        end
        else begin
          advance l;
          Token.GREATERTHAN
        end
    | '@' ->
        advance l;
        if is_letter l.ch then
          let dir_name = read_identifier l in
          Token.lookup_atsign dir_name
        else Token.ILLEGAL
    | '"' -> Token.STRING (read_string l)
    | '\x00' -> Token.EOF
    | ch ->
        if is_letter ch then
          let ident = read_identifier l in
          Token.lookup_keyword ident
        else if is_digit ch then read_number l
        else begin
          advance l;
          Token.ILLEGAL
        end
  in
  { Token.type_ = t_type; line = tok_line; column = tok_col }
