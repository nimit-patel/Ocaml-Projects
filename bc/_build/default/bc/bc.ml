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

(*
let varScopes = Stack.create ;;
let globalScope = Stack.create ;;
*)

(* Gets value from the global scope *)
let rec getGlobalValue (var: string) (scopes :envQueue): float =
    
    if Stack.length scopes == 1 then 
        let globalScope = Stack.top scopes in
        let value = Scope.find_opt var globalScope in
        match value with
        | Some(flt)     -> flt
        | None          -> 0.0
    else
        let scope = Stack.pop scopes in
        getGlobalValue var scopes
    ;;

let varEval (var: string) (scopes :envQueue): float  = 
    let topScope = Stack.top scopes in
    let value = Scope.find_opt var topScope in

    match value with
    | Some(flt)     -> flt
    | None          -> getGlobalValue var (Stack.copy scopes)
    ;;

let evalOp (op: string) (left: float) (right: float) : float =
    match op with
    | "*" -> left *. right
    | "/" -> left /. right
    | "+" -> left +. right
    | "-" -> left -. right
    | "^" -> left ** right
    | _   -> 0.0
    ;;

let rec evalExpr (_e: expr) (_q:envQueue) :float  =
    match _e with
    | Num(num)              -> num
    | Var(variable)         -> varEval variable _q
    | Op2(op, e1, e2)       -> let left = evalExpr e1 _q in
                               let right = evalExpr e2 _q in
                               evalOp op left right
    
    | _                     -> 0.0
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
    let var = Var("r") in
    let scope = Scope.empty in
    let global = Scope.empty in
    let testScopes = Stack.create () in

    let scope = Scope.add "i" 24.0 scope in
    let global = Scope.add "r" 23.0 global in
    
    Stack.push global testScopes;
    Stack.push scope testScopes;

    evalExpr var testScopes |>
    printf "%F";
    [%expect {| 1. |}]

let evalCode (_code: block) (_q:envQueue): unit = 
    (* crate new environment *)
    (* user fold_left  *)
    (* pop the local environment *)
    print_endline "Not implemented"

let evalStatement (s: statement) (q:envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> (* eval e and store in v *) q
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e q in
                if(cond > 0.0) then 
                    evalCode codeT q 
                else
                    evalCode codeF q
            ;
            q
        | _ -> q (*ignore *)
    ;;


(* 
    v = 10; 
    v // display v
 *)
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
];;

(*
let%expect_test "p1" =
    evalCode p1 []; 
    [%expect {| 1. |}]
    ;;
    *)

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