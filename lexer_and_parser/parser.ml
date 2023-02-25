open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  let (t, expr) = parse_Expr toks in 
  if t <> [] then failwith "error" else t, expr

(* Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr*)

and parse_Expr toks = 
  match lookahead toks with 
    | Some Tok_Let -> parse_LetExpr toks
    | Some Tok_If -> parse_IfExpr toks
    | Some Tok_Fun -> parse_FunctionExpr toks
    | _ -> parse_OrExpr toks

and parse_LetExpr toks = 
  let t = match_token toks Tok_Let in 
  let boolean = (match lookahead t with 
    | Some Tok_Rec -> true
    | _ -> false)
  in 
  let rec_tok = if boolean then match_token t Tok_Rec else t in
  let tok_ID = (match lookahead rec_tok with 
    | Some Tok_ID (x) -> x 
    | _ -> raise (InvalidInputException "parse_FunctionExpr") )
  in
  let tok_id = match_token rec_tok (Tok_ID(tok_ID)) in 
  let equal_tok = match_token tok_id Tok_Equal in 
    (*check for parse_expr again *)
  let (t', e') = parse_Expr equal_tok in 
  let in_tok = match_token t' Tok_In in 
  let (t'', e'') = parse_Expr in_tok in 
  (t'', Let (tok_ID, boolean, e', e''))

and parse_FunctionExpr toks = 
  let t = match_token toks Tok_Fun in 
  let tok_ID = match lookahead t with 
    | Some Tok_ID(x) -> x
    | _ -> raise (InvalidInputException "parse_FunctionExpr") 
    in
  let tok_id = match_token t (Tok_ID(tok_ID)) in 
  let tok_arrow = match_token tok_id Tok_Arrow in
  let (t', e') = parse_Expr tok_arrow in (t', Fun (tok_ID, e'))

and parse_IfExpr toks = 
  let t = match_token toks Tok_If in 
  let (t', e') = parse_Expr t in 
  let then_tok = match_token t' Tok_Then in 
  let (then_tok', then_e') = parse_Expr then_tok in 
  let else_tok = match_token then_tok' Tok_Else in 
  let (else_tok', else_e') = parse_Expr else_tok in 
  (*Not removing the last false *)
  (else_tok', If (e', then_e', else_e'))


and parse_OrExpr toks = 
  let (t, a) = parse_AndExpr toks in 
  match lookahead t with 
  | Some Tok_Or -> let t' = match_token t Tok_Or in 
  let (t'', o) = parse_OrExpr t' in 
  (t'', Binop(Or, a, o))
  | _ -> (t, a)

and parse_AndExpr toks = 
  let (t, e) = parse_EqualityExpr toks in 
  match lookahead t with 
  | Some Tok_And -> let t' = match_token t Tok_And in 
  let (t'', a) = parse_AndExpr t' in 
  (t'', Binop (And, e, a))
  | _ -> (t,e)

and parse_EqualityExpr toks = 
  let (t, r) = parse_RelationalExpr toks in
  match lookahead t with 
  | Some Tok_Equal -> let t' = match_token t Tok_Equal in 
  let (t'', e) = parse_EqualityExpr t' in 
  (t'', Binop(Equal, r, e))
  | Some Tok_NotEqual -> let t' = match_token t Tok_NotEqual in 
  let (t'', e) = parse_EqualityExpr t' in 
  (t'', Binop(NotEqual, r, e))
  | _ -> (t, r)

and parse_RelationalExpr toks = 
  let (t, a) = parse_AdditiveExpr toks in 
  match lookahead t with 
  | Some Tok_Less -> let t' = match_token t Tok_Less in
  let (t'', r) = parse_RelationalExpr  t' in 
  (t'', Binop(Less, a, r))
  | Some Tok_Greater -> let t' = match_token t Tok_Greater in
  let (t'', r) = parse_RelationalExpr  t' in 
  (t'', Binop(Greater, a, r))
  | Some Tok_LessEqual -> let t' = match_token t Tok_LessEqual in
  let (t'', r) = parse_RelationalExpr  t' in 
  (t'', Binop(LessEqual, a, r))
  | Some Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in
  let (t'', r) = parse_RelationalExpr  t' in 
  (t'', Binop(GreaterEqual, a, r))
  | _ -> (t, a)

and parse_AdditiveExpr toks = 
  let (t, m) = parse_MultiplicativeExpr toks in 
  match lookahead t with 
    | Some Tok_Add -> let t' = match_token t Tok_Add in
    let (t'', a) = parse_AdditiveExpr t' in
    (t'', Binop(Add, m, a))
    | Some Tok_Sub -> let t' = match_token t Tok_Sub in
    let (t'', a) = parse_AdditiveExpr t' in
    (t'', Binop(Sub, m, a))
    | _ -> (t, m)

and parse_MultiplicativeExpr toks = 
  let (t, c) = parse_ConcatExpr toks in 
  match lookahead t with 
    | Some Tok_Mult -> let t' = match_token t Tok_Mult in
    let (t'', m) = parse_MultiplicativeExpr t' in 
    (t'', Binop(Mult, c, m))
    | Some Tok_Div -> let t' = match_token t Tok_Div in
    let (t'', m) = parse_MultiplicativeExpr t' in 
    (t'', Binop(Div, c, m))
    | _ -> (t, c)

and parse_ConcatExpr toks = 
  let (t, u) = parse_UnaryExpr toks in 
  match lookahead t with 
  | Some Tok_Concat -> let t' = match_token t Tok_Concat in
  let (t'', h) = parse_ConcatExpr t' in (t'', Binop(Concat, u, h))
  | _ -> (t, u)

and parse_UnaryExpr toks = 
  match lookahead toks with 
    | Some Tok_Not -> let t = match_token toks (Tok_Not) in 
    let (t', u') = parse_UnaryExpr t in (t', (Not(u')))
    | _ -> parse_FunctionCallExpr toks

and parse_FunctionCallExpr toks = 
  let (t, p) = parse_PrimaryExpr toks in 

  match lookahead t with 
  | Some Tok_Int x -> let (t', p') = parse_PrimaryExpr t in (t', FunctionCall(p, p'))
  | Some Tok_Bool x -> let (t', p') = parse_PrimaryExpr t in (t', FunctionCall(p, p'))
  | Some Tok_String x -> let (t', p') = parse_PrimaryExpr t in (t', FunctionCall(p, p'))
  | Some Tok_ID x -> let (t', p') = parse_PrimaryExpr t in (t', FunctionCall(p, p'))
  | Some Tok_LParen -> let (t', p') = parse_PrimaryExpr t in (t', FunctionCall(p, p'))
  | _ -> (t,p)

(*Gotta fix this part*)
and parse_PrimaryExpr toks = 
  match lookahead toks with 
    | Some Tok_Int x -> (match_token toks (Tok_Int(x))), Value(Int(x))
    | Some Tok_Bool x -> (match_token toks (Tok_Bool(x))), Value(Bool(x))
    | Some Tok_String x -> (match_token toks (Tok_String(x))), Value(String(x))
    | Some Tok_ID x -> (match_token toks (Tok_ID(x))), ID(x)
    | Some Tok_LParen -> let t = match_token toks Tok_LParen in
      let (t', s) = parse_Expr t in 
      let t'' = match_token t' Tok_RParen in (t'', s)
    | _ -> (toks, Value(String("i don't know")))

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with 
  (*may not need to put doublesemi first*)
  | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi), NoOp
  | Some Tok_Def -> parse_DefMutop toks
  | _ -> parse_ExprMutop toks

and parse_DefMutop toks = 
  let t = match_token toks Tok_Def in 
  let tok_ID = match lookahead t with 
    | Some Tok_ID(x) -> x 
    | _ -> raise (InvalidInputException "parse_DefMutop") 
  in
  let tok_id = match_token t (Tok_ID(tok_ID)) in 
  let equal_tok = match_token tok_id Tok_Equal in 
  let (t', e') = parse_Expr equal_tok in 
  let double_semi_tok = match_token t' Tok_DoubleSemi in 
  (double_semi_tok, Def (tok_ID, e'))

and parse_ExprMutop toks = 
  let (t, e) = parse_Expr toks in 
  let double_semi_tok = match_token t Tok_DoubleSemi in 
  (double_semi_tok, Expr(e))