// parser.mly

%token                 EOF
%token                 LPAREN
%token                 RPAREN
%token                 EQ
%token                 COMMA
%token                 SEMICOLON
%token                 ASSIGN
%token                 PLUS
%token                 MINUS
%token                 TIMES
%token                 DIV
%token                 REST
%token                 EQUAL
%token                 NEQUAL
%token                 LT
%token                 LE
%token                 GT
%token                 GE
%token                 AND
%token                 OR
%token                 IF
%token                 THEN
%token                 ELSE
%token                 WHILE
%token                 DO
%token                 LET
%token                 IN
%token                 INT
%token                 BOOL
%token                 UNIT
%token <bool>          LITBOOL
%token <int>           LITINT
%token <Symbol.symbol> ID

(* add precedence rules if needed *)

%start <Ast.lprogram> program


%nonassoc IFP
%nonassoc W ELSE LEP AS

%left OR
%left AND 
%nonassoc  EQUAL NEQUAL LT  LE GE GT 
%left PLUS MINUS 
%left TIMES DIV  REST



%%

(* write the missing production rules *)
program:
    | seq=nonempty_list(fundec) EOF { ($loc , Ast.Program (seq)) }

fundec:
    | t=typeid LPAREN l=typeidlist RPAREN EQ e=exp { $loc , (t, l, e) }

typeid:
    | INT var=ID   { (Ast.Int, ($loc, var)) }
    | BOOL var= ID { (Ast.Bool, ($loc, var)) }
    | UNIT var= ID { (Ast.Unit, ($loc, var)) }

typeidlist:
    | l=separated_list(COMMA, typeid) { l }

exp:
    | a=LITINT { $loc , Ast.IntExp a }
    | a=LITBOOL { $loc , Ast.BoolExp a }
    | a=ID { $loc , Ast.VarExp a }
    | a= ID ASSIGN b=exp %prec AS{$loc, Ast.AssignExp(a,b)}
    | ex1=exp op=operator ex2=exp { $loc , Ast.OpExp (op, ex1, ex2) }
    | IF ex1=exp THEN ex2=exp ELSE ex3=exp { $loc , Ast.IfExp (ex1, ex2, Some  ex3) }
    | IF ex1=exp THEN ex2=exp %prec IFP { $loc , Ast.IfExp (ex1, ex2, None)}
    | WHILE ex1=exp DO ex2=exp  %prec W{ $loc , Ast.WhileExp (ex1, ex2)}
    | a=ID LPAREN l=args RPAREN { $loc , Ast.CallExp (a, l) }
    | LET a=ID EQ ex1=exp IN ex2=exp %prec LEP { $loc , Ast.LetExp (a, ex1, ex2) }
    |LPAREN ex1 = explist RPAREN {$loc, Ast.SeqExp (ex1)}

args: 
    | l = separated_list(COMMA,exp) {l }

explist:
    | l=separated_list(SEMICOLON, exp) { l }

%inline operator:
    | PLUS { Ast.Plus }
    | MINUS { Ast.Minus }
    | TIMES {Ast.Times}
    | DIV {Ast.Div}
    | LT   { Ast.LT }
    | LE   { Ast.LE }
    | GT   { Ast.GT }
    | REST {Ast.Rest}
    | EQUAL {Ast.EQ}
    | NEQUAL {Ast.NE}
    | GE   { Ast.GE }

    | AND {Ast.And}
    | OR {Ast.Or}