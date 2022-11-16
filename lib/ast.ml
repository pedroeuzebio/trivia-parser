(* ast.ml *)

type symbol = Symbol.symbol
  [@@deriving show]

type 'a loc = 'a Location.loc
  [@@deriving show]


type operator =
  | Plus
  | Minus
  | Times
  | Div
  | Rest
  | EQ
  | NE
  | LT
  | LE
  | GT
  | GE
  | And
  | Or
  [@@deriving show]

type exp =
  | IntExp of int
  | BoolExp of bool
  | VarExp of symbol
  | AssignExp of symbol * lexp
  | OpExp of operator * lexp * lexp
  | IfExp of lexp * lexp * lexp option
  | WhileExp of lexp * lexp
  | CallExp of symbol * lexp list
  | LetExp of symbol * lexp * lexp
  | SeqExp of lexp list
  [@@deriving show]

(* this type should be uncommented *)

and program =
  | Program of lfundec list
  [@@deriving show]
  
  (* this type should be commented *)
(*
and program =
| Program of int
[@@deriving show]
*)

and fundec = typeid * typeid list * lexp
  [@@deriving show]

and type_ =
  | Int
  | Bool
  | Unit
  [@@deriving show]

and typeid = type_ * lsymbol
  [@@deriving show]

and lsymbol = symbol loc
  [@@deriving show]

and lexp = exp loc
  [@@deriving show]

and lfundec = fundec loc
  [@@deriving show]

and lprogram = program loc
  [@@deriving show]
