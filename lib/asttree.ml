(* asttree.ml *)
(* Convert abstract syntax trees to multiway trees of string *)

open Ast

(* Helper functions *)

let name = Symbol.name
let map = List.map
let sprintf = Format.sprintf

let mkt root children =
  PrintBox.tree (PrintBox.text root) children

(* Convert a symbol to a general tree *)
let tree_of_symbol s = mkt (name s) []

(* Convert a binary operator to a string *)
let string_of_operator op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Rest -> "%"
  | EQ -> "=="
  | NE -> "<>"
  | LT -> "<"
  | LE -> "<="
  | GT -> ">"
  | GE -> ">="
  | And -> "and"
  | Or -> "or"

(* Convert a type to a string *)
let string_of_type_ t =
  match t with
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"

(* Convert an expression to a generic tree *)
let rec tree_of_exp exp =
  match exp with
  | IntExp x -> mkt (sprintf "IntExp %i" x) []
  | BoolExp x -> mkt (sprintf "BoolExp %b" x) []
  | VarExp x -> mkt (sprintf "VarExp %s" (name x)) []
  | AssignExp (x, e) -> mkt (sprintf "AssignExp %s" (name x)) [tree_of_lexp e]
  | OpExp (op, l, r) -> mkt (sprintf "OpExp %s" (string_of_operator op)) [tree_of_lexp l; tree_of_lexp r]
  | IfExp (t, x, None) -> mkt "IfExp" [tree_of_lexp t; tree_of_lexp x]
  | IfExp (t, x, Some y) -> mkt "IfExp" [tree_of_lexp t; tree_of_lexp x; tree_of_lexp y]
  | WhileExp (t, x) -> mkt "WhileExp" [tree_of_lexp t; tree_of_lexp x]
  | CallExp (f, a) -> mkt (sprintf "CallExp %s" (name f)) (map tree_of_lexp a)
  | LetExp (x, i, e) -> mkt (sprintf "LetExp %s" (name x)) [tree_of_lexp i; tree_of_lexp e]
  | SeqExp l -> mkt (sprintf "SeqExp") (map tree_of_lexp l)

and tree_of_fundec (typeid, params, body) =
  mkt
    "Fun"
    [ tree_of_typeid typeid;
      mkt "Formals" (map tree_of_typeid params);
      tree_of_lexp body
    ]

(* this function should be umcommentted *)
and tree_of_program (Program fundecs) =
mkt "Program" (map tree_of_lfundec fundecs)

(* this function should be commented *)
(*
and tree_of_program (Program ct) =
mkt "Program" [mkt (string_of_int ct) []]
*)

and tree_of_typeid (type_, (_loc, id)) =
  mkt (sprintf "%s:%s" (name id) (string_of_type_ type_)) []

(* Convert an anotated ast to a generic tree *)
and mkt_of_lsymbol (_loc, x) = tree_of_symbol x

and tree_of_lexp (_loc, x) = tree_of_exp x

and tree_of_lfundec (_loc, x) = tree_of_fundec x

and tree_of_lprogram (_loc, x) = tree_of_program x
