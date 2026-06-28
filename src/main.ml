let () =
  let input =
    "var x = 10; let y = 5; if (y > x) { let result = y; } else { let result = \
     x; }"
  in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let program = Parser.parse_program parser in
  let ast_string = Ast.string_of_program program in
  print_endline ast_string
