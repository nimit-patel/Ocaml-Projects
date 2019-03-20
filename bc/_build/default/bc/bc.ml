open Core.Std ;;
open Caml ;;
module Scope = Caml.Map.Make(String) ;;

type sExpr = 
    | Atom of string
    | List of sExpr list
    ;;

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string* expr list
    ;;

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr* statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 
    ;;

type block = statement list ;;

type env = float Scope.t ;;

type envQueue = env Stack.t;;

let globalStack = Stack.create () ;;
let localStack = Stack.create () ;;

(* Gets value from the global scope *)    
let rec getGlobalValue (var: string) : float =
    let globalScope = Stack.top globalStack in
    let value = Scope.find_opt var globalScope in
    match value with
    | Some(flt)     -> flt
    | None          -> 0.0
    ;;

let varEval (var: string) (scopes :envQueue): float  = 
    let topScope = Stack.top scopes in
    let value = Scope.find_opt var topScope in

    match value with
    | Some(flt)     -> flt
    | None          -> getGlobalValue var
    ;;


let assignVar (var: string) (value : float) (scopes :envQueue): unit = 
    let localScope = Stack.pop scopes in
    let globalScope = Stack.pop globalStack in

    if(Scope.mem var localScope) then
        let localScope = Scope.add var value localScope in
        Stack.push localScope scopes
    else if(Scope.mem var globalScope) then
        let globalScope =  Scope.add var value globalScope in
        Stack.push globalScope globalStack
    else begin
        let localScope = Scope.add var value localScope in
        Stack.push localScope scopes
    end
    ;;

let evalOp (op: string)  (left: float) (right: float) : float =
    match op with
    | "*" -> left *. right
    | "/" -> left /. right
    | "+" -> left +. right
    | "-" -> left -. right
    | "^" -> left ** right
    | _   -> 0.0
    ;;

let rec evalExpr (exp : expr) (scopes :envQueue) :float  =
    match exp with
    | Num(num)              -> num
    | Var(variable)         -> varEval variable scopes
    | Op2(op, e1, e2)       -> let left = evalExpr e1 scopes in
                               let right = evalExpr e2 scopes in
                               evalOp op left right
    
    | _                     -> 0.0
    ;;



let rec evalStatement (s: statement) (scopes :envQueue): envQueue =
    match s with 
        | Assign(var, expr)  ->  let value = evalExpr expr scopes in
                                assignVar var value scopes;
                                scopes
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e scopes in
                if(cond > 0.0) then 
                    evalCode codeT scopes 
                else
                    evalCode codeF scopes
            ;
            scopes
        | Expr(expr)          -> let result = evalExpr expr scopes in
                                 print_float result;
                                 scopes
        | _ -> scopes (*ignore *)

and evalCode (stat_list: block) (scopes :envQueue): unit = 
    (* crate new environment *)
    (* user fold_left  *)
    (* pop the local environment *)
    match stat_list with
    | hd::tl        -> let s = evalStatement hd scopes in
                       evalCode tl scopes
    | _             -> ()
    ;;

(* Test for expression
let%expect_test "evalNum" = 
    let t1 = Op2("^", (Num 20.0), (Num 20.0)) in
    evalExpr t1 [] |>
    printf "%F";
    [%expect {| 40. |}]
    ;;
     *)

(* Test for variable *)
let%expect_test "evalVar" = 
    let var = Var("i") in
    let scope = Scope.empty in
    let global = Scope.empty in

    let scope = Scope.add "i" 24.0 scope in
    let global = Scope.add "r" 23.0 global in
    
    Stack.push scope localStack;
    Stack.push global globalStack;

    evalExpr var localStack |>
    printf "%F";
    [%expect {| 24. |}]

(* 
    v = 10; 
    v // display v
 *)
let p1: block = [
        Assign("v", Num(4.0));
        Expr(Var("v")) 
];;


let%expect_test "p1" =
    Stack.push Scope.empty localStack;
    Stack.push Scope.empty globalStack;
    evalCode p1 localStack; 
    [%expect {| 4. |}]
    ;;

(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
];;

(*
let%expect_test "p1" =
    evalCode p2 []; 
    [%expect {| 3628800. |}]
    ;;

    *)

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }
    f(3)
    f(5)
 *)
let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<=", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]
    ;;

(*
let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]
    ;;
    *)

(* ADD run. Internal func can change *)
(* Read no needed *)
(*
// var assignment --> 
// use curr scope if var exist there
// use global scope if var exist there
// use local scope create new var 
*)
