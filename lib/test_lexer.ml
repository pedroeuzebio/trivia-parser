(* inline expect tests for the scanner *)

let scan_string s =
  let lexbuf = Lexing.from_string s in
  let rec loop () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n%!"
      Location.pp_location
      (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> loop ()
  in
  try loop ()
  with
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s\n" Location.pp_location loc msg

let%expect_test "spaces" =
  scan_string " \n\t   \n  ";
  [%expect{| :3.2-3.2 Parser.EOF |}]

let%expect_test "comments" =
  scan_string "# this is a comment";
  [%expect{| :1.19-1.19 Parser.EOF |}]

let%expect_test "boolean literals" =
  scan_string "true false";
  [%expect{|
    :1.0-1.4 (Parser.LITBOOL true)
    :1.5-1.10 (Parser.LITBOOL false)
    :1.10-1.10 Parser.EOF |}]

let%expect_test "integer literals" =
  scan_string "27348";
  [%expect{|
    :1.0-1.5 (Parser.LITINT 27348)
    :1.5-1.5 Parser.EOF |}];

  (* integer literal has no signal *)
  scan_string "-27348";
  [%expect{|
    :1.0-1.1 Parser.MINUS
    :1.1-1.6 (Parser.LITINT 27348)
    :1.6-1.6 Parser.EOF |}];

  scan_string "+27348";
  [%expect{|
    :1.0-1.1 Parser.PLUS
    :1.1-1.6 (Parser.LITINT 27348)
    :1.6-1.6 Parser.EOF |}]

let%expect_test "types" =
  scan_string "int";
  [%expect{|
    :1.0-1.3 Parser.INT
    :1.3-1.3 Parser.EOF |}];
  scan_string "bool";
  [%expect{|
    :1.0-1.4 Parser.BOOL
    :1.4-1.4 Parser.EOF |}];
  scan_string "unit";
  [%expect{|
    :1.0-1.4 Parser.UNIT
    :1.4-1.4 Parser.EOF |}]

let%expect_test "let expressions" =
  scan_string "let in";
  [%expect{|
    :1.0-1.3 Parser.LET
    :1.4-1.6 Parser.IN
    :1.6-1.6 Parser.EOF |}]

let%expect_test "if expressions" =
  scan_string "if then else";
  [%expect{|
    :1.0-1.2 Parser.IF
    :1.3-1.7 Parser.THEN
    :1.8-1.12 Parser.ELSE
    :1.12-1.12 Parser.EOF |}]

let%expect_test "while expressions" =
  scan_string "while do";
  [%expect{|
    :1.0-1.5 Parser.WHILE
    :1.6-1.8 Parser.DO
    :1.8-1.8 Parser.EOF |}]

let%expect_test "identifiers" =
  scan_string "Idade alfa15 beta_2";
  [%expect{|
    :1.0-1.5 (Parser.ID "Idade")
    :1.6-1.12 (Parser.ID "alfa15")
    :1.13-1.19 (Parser.ID "beta_2")
    :1.19-1.19 Parser.EOF |}];

  (* invalid identifier *)
  scan_string "_altura";
  [%expect{| :1.0-1.1 error: illegal character '_' |}];

  scan_string "5x";
  [%expect{|
    :1.0-1.1 (Parser.LITINT 5)
    :1.1-1.2 (Parser.ID "x")
    :1.2-1.2 Parser.EOF |}]

let%expect_test "binary operators" =
  scan_string ":= + - * / % == <> < <= > >= and or";
  [%expect{|
    :1.0-1.2 Parser.ASSIGN
    :1.3-1.4 Parser.PLUS
    :1.5-1.6 Parser.MINUS
    :1.7-1.8 Parser.TIMES
    :1.9-1.10 Parser.DIV
    :1.11-1.12 Parser.REST
    :1.13-1.15 Parser.EQUAL
    :1.16-1.18 Parser.NEQUAL
    :1.19-1.20 Parser.LT
    :1.21-1.23 Parser.LE
    :1.24-1.25 Parser.GT
    :1.26-1.28 Parser.GE
    :1.29-1.32 Parser.AND
    :1.33-1.35 Parser.OR
    :1.35-1.35 Parser.EOF |}]

let%expect_test "punctuations" =
  scan_string "( ) , ; =";
  [%expect{|
    :1.0-1.1 Parser.LPAREN
    :1.2-1.3 Parser.RPAREN
    :1.4-1.5 Parser.COMMA
    :1.6-1.7 Parser.SEMICOLON
    :1.8-1.9 Parser.EQ
    :1.9-1.9 Parser.EOF |}]

let%expect_test "program" =
  scan_string {|
               int sum(int a, int b) =
                 if a < b then
                   a + sum(a + 1, b)
                 else
                   0
               unit main() =
                 let x = readint() in
                 print(let y = x + 1 in sum(0, y))
               |};
  [%expect{|
    :2.15-2.18 Parser.INT
    :2.19-2.22 (Parser.ID "sum")
    :2.22-2.23 Parser.LPAREN
    :2.23-2.26 Parser.INT
    :2.27-2.28 (Parser.ID "a")
    :2.28-2.29 Parser.COMMA
    :2.30-2.33 Parser.INT
    :2.34-2.35 (Parser.ID "b")
    :2.35-2.36 Parser.RPAREN
    :2.37-2.38 Parser.EQ
    :3.17-3.19 Parser.IF
    :3.20-3.21 (Parser.ID "a")
    :3.22-3.23 Parser.LT
    :3.24-3.25 (Parser.ID "b")
    :3.26-3.30 Parser.THEN
    :4.19-4.20 (Parser.ID "a")
    :4.21-4.22 Parser.PLUS
    :4.23-4.26 (Parser.ID "sum")
    :4.26-4.27 Parser.LPAREN
    :4.27-4.28 (Parser.ID "a")
    :4.29-4.30 Parser.PLUS
    :4.31-4.32 (Parser.LITINT 1)
    :4.32-4.33 Parser.COMMA
    :4.34-4.35 (Parser.ID "b")
    :4.35-4.36 Parser.RPAREN
    :5.17-5.21 Parser.ELSE
    :6.19-6.20 (Parser.LITINT 0)
    :7.15-7.19 Parser.UNIT
    :7.20-7.24 (Parser.ID "main")
    :7.24-7.25 Parser.LPAREN
    :7.25-7.26 Parser.RPAREN
    :7.27-7.28 Parser.EQ
    :8.17-8.20 Parser.LET
    :8.21-8.22 (Parser.ID "x")
    :8.23-8.24 Parser.EQ
    :8.25-8.32 (Parser.ID "readint")
    :8.32-8.33 Parser.LPAREN
    :8.33-8.34 Parser.RPAREN
    :8.35-8.37 Parser.IN
    :9.17-9.22 (Parser.ID "print")
    :9.22-9.23 Parser.LPAREN
    :9.23-9.26 Parser.LET
    :9.27-9.28 (Parser.ID "y")
    :9.29-9.30 Parser.EQ
    :9.31-9.32 (Parser.ID "x")
    :9.33-9.34 Parser.PLUS
    :9.35-9.36 (Parser.LITINT 1)
    :9.37-9.39 Parser.IN
    :9.40-9.43 (Parser.ID "sum")
    :9.43-9.44 Parser.LPAREN
    :9.44-9.45 (Parser.LITINT 0)
    :9.45-9.46 Parser.COMMA
    :9.47-9.48 (Parser.ID "y")
    :9.48-9.49 Parser.RPAREN
    :9.49-9.50 Parser.RPAREN
    :10.15-10.15 Parser.EOF |}]
