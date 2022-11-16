(* Test syntax analyser *)

module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    let tree = Asttree.tree_of_lprogram ast in
    let box = PrintBox_text.to_string tree in
    Format.printf "%s\n\n%!" box;
  with
  | Parser.Error ->
     Format.printf "%a error: syntax\n%!" Location.pp_position lexbuf.L.lex_curr_p
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s%!" Location.pp_location loc msg

let%expect_test "function declaration and constant expression" =
  check "int f() = 100";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─IntExp 100 |}];

  check "bool f(int x) = false";
  [%expect{|
    Program
    └─Fun
      ├─f:bool
      ├─Formals
      │ └─x:int
      └─BoolExp false |}];

  check "bool f(int x, bool y) = true";
  [%expect{|
    Program
    └─Fun
      ├─f:bool
      ├─Formals
      │ ├─x:int
      │ └─y:bool
      └─BoolExp true |}];

  check "int f(int x, int y, bool z) = 101";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      │ ├─x:int
      │ ├─y:int
      │ └─z:bool
      └─IntExp 101 |}];

  check {|
         int f() = 99
         int g(int y) = 100
         bool h(int a, int b, int c, int d) = true
         |};
  [%expect{|
    Program
    ├─Fun
    │ ├─f:int
    │ ├─Formals
    │ └─IntExp 99
    ├─Fun
    │ ├─g:int
    │ ├─Formals
    │ │ └─y:int
    │ └─IntExp 100
    └─Fun
      ├─h:bool
      ├─Formals
      │ ├─a:int
      │ ├─b:int
      │ ├─c:int
      │ └─d:int
      └─BoolExp true |}];

  check "foo f(int x) = 100";
  [%expect{| :1.3 error: syntax |}];

  check "unit f(numero x) = 100";
  [%expect{| :1.13 error: syntax |}];

  check "int f() 100";
  [%expect{| :1.11 error: syntax |}];

  check "bool f(int x y) = 0";
  [%expect{| :1.14 error: syntax |}]

let%expect_test "variables" =
  check "int f(int p) = p";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      │ └─p:int
      └─VarExp p |}]

let%expect_test "binary operators" =
  check "bool f() = 2 + 3 - 4 * 5 < 5 * 6 / 7 % 8 - 9";
  [%expect{|
    Program
    └─Fun
      ├─f:bool
      ├─Formals
      └─OpExp <
        ├─OpExp -
        │ ├─OpExp +
        │ │ ├─IntExp 2
        │ │ └─IntExp 3
        │ └─OpExp *
        │   ├─IntExp 4
        │   └─IntExp 5
        └─OpExp -
          ├─OpExp %
          │ ├─OpExp /
          │ │ ├─OpExp *
          │ │ │ ├─IntExp 5
          │ │ │ └─IntExp 6
          │ │ └─IntExp 7
          │ └─IntExp 8
          └─IntExp 9 |}];

  check "bool f() = 2 > 3 <= 4";
  [%expect{| :1.19 error: syntax |}];

  check "bool g() = 6 < 7 + 1";
  [%expect{|
    Program
    └─Fun
      ├─g:bool
      ├─Formals
      └─OpExp <
        ├─IntExp 6
        └─OpExp +
          ├─IntExp 7
          └─IntExp 1 |}];

  check "bool g() = 1 == 2 or 3 <> 4 and 5 >= 6 and true or false";
  [%expect{|
    Program
    └─Fun
      ├─g:bool
      ├─Formals
      └─OpExp or
        ├─OpExp or
        │ ├─OpExp ==
        │ │ ├─IntExp 1
        │ │ └─IntExp 2
        │ └─OpExp and
        │   ├─OpExp and
        │   │ ├─OpExp <>
        │   │ │ ├─IntExp 3
        │   │ │ └─IntExp 4
        │   │ └─OpExp >=
        │   │   ├─IntExp 5
        │   │   └─IntExp 6
        │   └─BoolExp true
        └─BoolExp false |}]

let%expect_test "assignment" =
  check "unit f(int x) = x := 2 + 3 * 4 > 5 % 6 or  x == 0";
  [%expect{|
    Program
    └─Fun
      ├─f:unit
      ├─Formals
      │ └─x:int
      └─AssignExp x
        └─OpExp or
          ├─OpExp >
          │ ├─OpExp +
          │ │ ├─IntExp 2
          │ │ └─OpExp *
          │ │   ├─IntExp 3
          │ │   └─IntExp 4
          │ └─OpExp %
          │   ├─IntExp 5
          │   └─IntExp 6
          └─OpExp ==
            ├─VarExp x
            └─IntExp 0 |}];

  check "int f(int x) = x := y := 2 - 3";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      │ └─x:int
      └─AssignExp x
        └─AssignExp y
          └─OpExp -
            ├─IntExp 2
            └─IntExp 3 |}];

  check "int f(int x) = x := 2 + y := 3";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      │ └─x:int
      └─AssignExp x
        └─OpExp +
          ├─IntExp 2
          └─AssignExp y
            └─IntExp 3 |}]

let%expect_test "conditional expression" =
  check "int f() = if 5 < 6 then 10";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─IfExp
        ├─OpExp <
        │ ├─IntExp 5
        │ └─IntExp 6
        └─IntExp 10 |}];

  check "int f() = if true then 10 else 20";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─IfExp
        ├─BoolExp true
        ├─IntExp 10
        └─IntExp 20 |}];

  check "int f() = if true then 10 else 20 + 30";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─IfExp
        ├─BoolExp true
        ├─IntExp 10
        └─OpExp +
          ├─IntExp 20
          └─IntExp 30 |}];

  check {|
         int f() =
           if false then
             if true then
               10
             else
               20
           else
             if 12 < 0 then
               40
             else
               50
         |};
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─IfExp
        ├─BoolExp false
        ├─IfExp
        │ ├─BoolExp true
        │ ├─IntExp 10
        │ └─IntExp 20
        └─IfExp
          ├─OpExp <
          │ ├─IntExp 12
          │ └─IntExp 0
          ├─IntExp 40
          └─IntExp 50 |}];

  check {|
         int f() =
           if 2 < 3 then
             if true then
               10
             else
               20
         |};
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─IfExp
        ├─OpExp <
        │ ├─IntExp 2
        │ └─IntExp 3
        └─IfExp
          ├─BoolExp true
          ├─IntExp 10
          └─IntExp 20 |}]

let%expect_test "repetition" =
  check "unit f(int x) = while x > 0 do x := x - 1";
  [%expect{|
    Program
    └─Fun
      ├─f:unit
      ├─Formals
      │ └─x:int
      └─WhileExp
        ├─OpExp >
        │ ├─VarExp x
        │ └─IntExp 0
        └─AssignExp x
          └─OpExp -
            ├─VarExp x
            └─IntExp 1 |}]

let%expect_test "let expression" =
  check "bool f() = let x = 2 + 3 in 4 * x < 9";
  [%expect{|
    Program
    └─Fun
      ├─f:bool
      ├─Formals
      └─LetExp x
        ├─OpExp +
        │ ├─IntExp 2
        │ └─IntExp 3
        └─OpExp <
          ├─OpExp *
          │ ├─IntExp 4
          │ └─VarExp x
          └─IntExp 9 |}];

  check "int f() = let x = 2 + 3 in if x > 10 then x := x/10 else 200";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─LetExp x
        ├─OpExp +
        │ ├─IntExp 2
        │ └─IntExp 3
        └─IfExp
          ├─OpExp >
          │ ├─VarExp x
          │ └─IntExp 10
          ├─AssignExp x
          │ └─OpExp /
          │   ├─VarExp x
          │   └─IntExp 10
          └─IntExp 200 |}];

  check "bool f() = if 5 < 10 then false else let y = 5 in 8 < y";
  [%expect{|
    Program
    └─Fun
      ├─f:bool
      ├─Formals
      └─IfExp
        ├─OpExp <
        │ ├─IntExp 5
        │ └─IntExp 10
        ├─BoolExp false
        └─LetExp y
          ├─IntExp 5
          └─OpExp <
            ├─IntExp 8
            └─VarExp y |}];

  check "int f() = if true then let y = 5 in 8 + y else 99";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─IfExp
        ├─BoolExp true
        ├─LetExp y
        │ ├─IntExp 5
        │ └─OpExp +
        │   ├─IntExp 8
        │   └─VarExp y
        └─IntExp 99 |}]

let%expect_test "function call" =
  check "int f(int x) = not(x < read_int())";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      │ └─x:int
      └─CallExp not
        └─OpExp <
          ├─VarExp x
          └─CallExp read_int |}];

  check "int f(int x) = g(x<3, x+1, 2*x+5) + 9";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      │ └─x:int
      └─OpExp +
        ├─CallExp g
        │ ├─OpExp <
        │ │ ├─VarExp x
        │ │ └─IntExp 3
        │ ├─OpExp +
        │ │ ├─VarExp x
        │ │ └─IntExp 1
        │ └─OpExp +
        │   ├─OpExp *
        │   │ ├─IntExp 2
        │   │ └─VarExp x
        │   └─IntExp 5
        └─IntExp 9 |}];

  check "int f() = g(2 3 4)";
  [%expect{|
    :1.15 error: syntax |}]

let%expect_test "sequence" =
  check "int f() = (2; true; 17)";
  [%expect{|
    Program
    └─Fun
      ├─f:int
      ├─Formals
      └─SeqExp
        ├─IntExp 2
        ├─BoolExp true
        └─IntExp 17 |}];

  check "unit f() = ()";
  [%expect{|
    Program
    └─Fun
      ├─f:unit
      ├─Formals
      └─SeqExp |}];

  check "bool f() = 2; true";
  [%expect{| :1.13 error: syntax |}]
