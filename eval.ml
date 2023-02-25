open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  | Value(x) -> x
  | ID(x) -> lookup env x
  | Not (x) -> let something = eval_expr env x in 
              (match something with
              | Bool(y) -> Bool (not y)
              | _ -> failwith "error")
  | Binop (a, b, c) -> 
    (match a with 
    | Add -> let n1 = (eval_expr env b) in 
             let n2 = (eval_expr env c) in
             (match (n1, n2) with 
             | Int(x), Int(y) -> Int (x + y)
             | _ -> raise (TypeError "Expected Int"))
    | Sub -> let n1 = (eval_expr env b) in 
             let n2 = (eval_expr env c) in
             (match (n1, n2) with 
             | Int(x), Int(y) -> Int (x - y)
             | _ -> raise (TypeError "Expected Int"))
    | Mult -> let n1 = (eval_expr env b) in 
             let n2 = (eval_expr env c) in
             (match (n1, n2) with 
             | Int(x), Int(y) -> Int (x * y)
             | _ -> raise (TypeError "Expected Int"))
    (* Need to check if it is divisible by 0 *)
    | Div -> let n1 = (eval_expr env b) in 
             let n2 = (eval_expr env c) in
             (match (n1, n2) with 
             | Int(x), Int(y) -> if y != 0 then Int (x / y) else raise DivByZeroError
             | _ -> raise (TypeError "Expected Int"))

    | Greater -> let n1 = (eval_expr env b) in 
             let n2 = (eval_expr env c) in
             (match (n1, n2) with 
             | Int(x), Int(y) -> Bool (x > y)
             | _ -> raise (TypeError "Expected Int"))
    | Less -> let n1 = (eval_expr env b) in 
             let n2 = (eval_expr env c) in
             (match (n1, n2) with 
             | Int(x), Int(y) -> Bool (x < y)
             | _ -> raise (TypeError "Expected Int"))
    | GreaterEqual -> let n1 = (eval_expr env b) in 
             let n2 = (eval_expr env c) in
             (match (n1, n2) with 
             | Int(x), Int(y) -> Bool (x >= y)
             | _ -> raise (TypeError "Expected Int"))
    | LessEqual -> let n1 = (eval_expr env b) in 
             let n2 = (eval_expr env c) in
             (match (n1, n2) with 
             | Int(x), Int(y) -> Bool (x <= y)
             | _ -> raise (TypeError "Expected Int"))
    (*need to check if both argument evaluate to a string *)
    | Concat -> let n1 = (eval_expr env b) in 
                let n2 = (eval_expr env c) in
                (match (n1, n2) with 
             | String(x), String(y) -> String (x ^ y)
             | _ -> raise (TypeError "Expected String"))

    (*Type error needs to be thrown if the two types don't match or if the types are Closure *)
    | Equal -> let n1 = (eval_expr env b) in 
              let n2 = (eval_expr env c) in
              (match (n1, n2) with 
             | Bool(x), Bool(y) -> Bool (x == y)
             | Int(x), Int(y) -> Bool (x == y)
             | String(x), String(y) -> Bool (x == y)
             | _ -> raise (TypeError "Expected Bool/String/Int"))
    | NotEqual -> let n1 = (eval_expr env b) in 
                let n2 = (eval_expr env c) in
                (match (n1, n2) with 
             | Bool(x), Bool(y) -> Bool (x != y)
             | Int(x), Int(y) -> Bool (x != y)
             | String(x), String(y) -> Bool (x != y)
             | _ -> raise (TypeError "Expected Bool/String/Int"))
    (*Throw type error if non bool *)
    | Or -> let n1 = (eval_expr env b) in 
                let n2 = (eval_expr env c) in
                (match (n1, n2) with 
             | Bool(x), Bool(y) -> Bool (x || y)
             | _ -> raise (TypeError "Expected Bool"))
    | And -> let n1 = (eval_expr env b) in 
                let n2 = (eval_expr env c) in
                (match (n1, n2) with 
             | Bool(x), Bool(y) -> Bool (x && y)
             | _ -> raise (TypeError "Expected Bool")))
  | If (e1, e2, e3) -> let if_bool = eval_expr env e1 in
                      (match if_bool with
                      | Bool(x) -> if x then eval_expr env e2 else eval_expr env e3
                      | _ -> raise (TypeError "Expected Bool")
                      )
  | Fun (v, e) -> Closure (env, v, e)
  | FunctionCall (e1, e2) -> let v1 = eval_expr env e1 in 
                             let v2 = eval_expr env e2 in 
                             (match v1 with 
                             | Closure (capital_a, x, b) -> let menv = extend capital_a x v2 in 
                                                    eval_expr menv b
                             | _ -> raise (TypeError "Not a function"))
  | Let (x, bool_var, e1, e2) -> if bool_var == false then  
                                   let v1 = eval_expr env e1 in
                                   let menv = extend env x v1 in 
                                   let v2 = eval_expr menv e2 in 
                                   v2
                                (*recursive part *)
                                 else 
                                   let temp_env = extend_tmp env x in 
                                   let v1 = eval_expr temp_env e1 in 
                                   update temp_env x v1;
                                   let v2 = eval_expr temp_env e2 in 
                                   v2

                                 



(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
 let eval_mutop env m = 
 (* failwith "error" *)
   match m with 
      | Def (v, e) -> let menv = extend_tmp env v in 
                     let v1 = eval_expr menv e in 
                     update menv v v1;
                     (menv, Some v1)
      | Expr (e) -> let v1 = eval_expr env e in 
                     (env, Some v1)
      | NoOp -> (env,None)