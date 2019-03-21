open Core.Std ;;
open Caml ;;
module Scope = Caml.Map.Make(String) ;;

(* Exceptions to handle return, break, and continue *)
exception Ret of float  ;;
exception Brk of unit   ;;
exception Cont of unit  ;;
exception Error of string ;;

(* Stacks for variables *)
let globalStack = Stack.create () ;;
let localStack = Stack.create () ;;
Stack.push Scope.empty localStack ;;
Stack.push Scope.empty globalStack ;;

type sExpr = 
    | Atom of string
    | List of sExpr list
    ;;

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string*expr list
    ;;

type statement = 
    | Break
    | Continue
    | Return of expr
    | Assign of string*expr
    | Expr of expr
    | If of expr* statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 
    ;;

type block = statement list ;;

type env = float Scope.t ;;

type envQueue = env Stack.t;;

let funcMap : statement list Scope.t ref = ref Scope.empty;;
let paramMap : string list Scope.t ref = ref Scope.empty;;

(* Puts value into the appropriate scope *)
let assignVar (var: string) (value : float) (scopes :envQueue): unit = 
    let localScope = Stack.pop scopes in
    let globalScope = Stack.pop globalStack in

    if(Scope.mem var localScope) then
        let localScope = Scope.add var value localScope in
        Stack.push localScope scopes;
        Stack.push globalScope globalStack;
    else if(Scope.mem var globalScope) then
        let globalScope =  Scope.add var value globalScope in
        Stack.push localScope scopes;
        Stack.push globalScope globalStack;
    else begin
        let localScope = Scope.add var value localScope in
        Stack.push localScope scopes;
        Stack.push globalScope globalStack;
    end
    ;;

(* Gets value from the global scope *)    
let rec getGlobalValue (var: string) (scopes :envQueue) : float =
    let globalScope = Stack.top globalStack in
    let value = Scope.find_opt var globalScope in
    match value with
    | Some(flt)     -> flt
    | None          -> assignVar var 0.0 scopes; (* puts default value 0 for this new variable *)
                       0.0
    ;;

(* Gets value from the local scope *)
let varEval (var: string) (scopes :envQueue): float  = 
    let topScope = Stack.top scopes in
    let value = Scope.find_opt var topScope in
    match value with
    | Some(flt)     -> flt
    | None          -> getGlobalValue var scopes
    ;;

let evalPre (addVal : float) (exp : expr) (scopes :envQueue) : float =
    match exp with
        | Var(var)  -> let value = varEval var scopes in 
                       let value = value +. addVal in
                       assignVar var value scopes;
                       value
        | _         -> raise (Error("Invalid operation on expression!"));
    ;;

let evalRel (op: string)  (left: float) (right: float) : float =
    let diff = left -. right in
    match op with
    | ">"    -> if diff > 0.  then 1.0 else 0.0
    | "<"    -> if diff < 0.  then 1.0 else 0.0
    | ">="   -> if diff >= 0. then 1.0 else 0.0
    | "<="   -> if diff <= 0. then 1.0 else 0.0
    | "=="   -> if diff = 0. then 1.0 else 0.0
    | "!="   -> if diff <> 0. then 1.0 else 0.0
    | _      -> raise (Error("Invalid relational operator " ^ op));
    ;;

let evalLogical (op: string)  (left: float) (right: float) : float =
    match op with
    | "&&" -> if (left != 0.0) && (right != 0.0) then 1.0 else 0.0
    | "||" -> if (left != 0.0) || (right != 0.0) then 1.0 else 0.0
    |  _   -> raise (Error("Invalid logical operator " ^ op));
    ;;

let evalOp (op: string)  (left: float) (right: float) : float =
    match op with
    | "*" -> left *. right
    | "/" -> left /. right
    | "+" -> left +. right
    | "-" -> left -. right
    | "^" -> left ** right
    | ">" | "<" | ">=" | "<=" | "==" | "!=" -> evalRel op left right
    | "&&" | "||" -> evalLogical op left right
    | _   -> raise (Error("Invalid binary operator " ^ op));
    ;;

let rec evalExpr (exp : expr) (scopes :envQueue) :float  =
    match exp with
    | Num(num)              -> num
    | Var(variable)         -> varEval variable scopes
    | Op1(op, e1)           -> evalUnary op e1 scopes
    | Op2(op, e1, e2)       -> let left = evalExpr e1 scopes in
                               let right = evalExpr e2 scopes in
                               evalOp op left right
    | Fct(name, expr_list)  -> evalFunc name expr_list scopes
    | _                     -> raise (Error("Invalid expression!"));
    ;

and evalUnary (op : string) (exp : expr) (scopes :envQueue) : float = 
match op with
    | "++"  -> evalPre 1.0 exp scopes
    | "--"  -> evalPre (0.0 -. 1.0) exp scopes
    | "!" -> let value = evalExpr exp scopes in
             if value == 0.0 then 1.0 else 0.0
    | "-" -> let value = evalExpr exp scopes in
             value *. -1.0
    | _   -> raise (Error("Invalid unary operator " ^ op));
    ;

and evalStatement (s: statement) (scopes :envQueue): envQueue =
    match s with 
        | Break             ->  raise (Brk ())
        | Continue          ->  raise (Cont ())
        | Return(expr)      ->  raise (Ret(evalExpr expr scopes));
                            
        | Assign(var, expr) ->  let value = evalExpr expr scopes in
                                assignVar var value scopes;
                                scopes

        
        | Expr(expr)        ->  let result = evalExpr expr scopes in
                                result |> printf "%F\n";
                                scopes

        | If(exp, codeT, codeF)       -> evalIfElse exp codeT codeF scopes;          
                                         scopes

        | While(cond, stat_list)      -> evalWhileLoop cond stat_list scopes;
                                         scopes

        | For(init, cond, update, stat_list) -> let tmp = evalStatement init scopes in
                                                evalForLoop cond update stat_list scopes;
                                                scopes

        | FctDef (name, params, stat_list)   -> putFuncDef name params stat_list;
                                                scopes
    
        | _                                  ->  raise (Error("Invalid statement!"));

        ;
and evalCode (stat_list: block) (scopes :envQueue): unit = 
    match stat_list with
    | hd::tl        -> let s = evalStatement hd scopes in
                       evalCode tl scopes
    | []            -> ()
    ;

and evalIfElse (exp : expr) (codeT : statement list) (codeF : statement list) (scopes : envQueue): unit =
    if(evalExpr exp scopes > 0.0) then 
        evalCode codeT scopes 
    else
        evalCode codeF scopes
    ;
and evalWhileLoop (cond : expr) (stat_list : statement list) (scopes: envQueue): unit =
    if (evalExpr cond scopes) <> 1.0 then
        ()
    else begin
            try
                evalCode stat_list scopes;
                evalWhileLoop cond stat_list scopes
            with
              Brk  () -> ()
            | Cont () -> evalWhileLoop cond stat_list scopes
        end
    ;

and evalForLoop (cond : expr) (update: statement) (stat_list: statement list) (scopes: envQueue): unit = 
    if (evalExpr cond scopes) <> 1.0 then
       ()
    else begin
            try
                evalCode stat_list scopes;
                let tmp = evalStatement update scopes in
                evalForLoop cond update stat_list scopes
            with
              Brk  () -> ()
            | Cont () -> let tmp = evalStatement update scopes in
                         evalForLoop cond update stat_list scopes
         end
    ;
    
and putFuncDef (name : string) (params : string list) (stat_list : statement list) : unit = 
    let key = string_of_int (List.length params) ^ name in
    funcMap := Scope.add key stat_list !funcMap;
    paramMap := Scope.add key params !paramMap;
    ;

and evalFunc (name : string) (args : expr list) (scopes : envQueue) : float =
    (* determine if the function exist *)
    let paramCount = (List.length args) in
    let key =  string_of_int paramCount ^ name in
    
    if (Scope.mem key !funcMap) then 
        let impl = Scope.find key !funcMap in
        let params = Scope.find key !paramMap in
        let funcScope: float Scope.t ref =  ref Scope.empty in  

        for i = 0 to (paramCount - 1) do
            let var = List.nth params i in
            let value = evalExpr (List.nth args i) scopes in 
            funcScope := Scope.add var value !funcScope;
        done;
        
        Stack.push !funcScope scopes;

        try 
            evalCode impl scopes;
            let tmp = Stack.pop scopes in (* remove function scope *)
            0.0
        with
            Ret(flt) -> let tmp = Stack.pop scopes in (* remove function scope *)
                        flt
        
    else
        0.0
    ;;

let runCode (code: block) : unit =
    try
        evalCode code localStack; 
    with
        Error(msg) -> print_endline msg;
    ;;

let testExpr : block = [
    Expr(Op2("+", Op2("*", (Num 20.0), (Num 20.0)), (Num 4.0)))
];;

(* Test for expression *)
let%expect_test "testExpr" = 
    runCode testExpr;  
    [%expect {| 404. |}]
    ;;

let testVar : block = [
    Expr(Var("i"))
];;

(* Test for variable *)
let%expect_test "testVar" = 
    runCode testVar;
    [%expect {| 0. |}]
    ;;

(* 
    v = 4; 
    v //  4
    ++v // 5
    v = (v + 4) / 2.25
    v == 2.25
    v != 2.25 
 *)

let testAssign: block = [
        Assign("v", Num(4.0));
        Expr(Var("v"));
        Expr(Op1("++", Var("v")));
        Assign("v", Op2("/", Op2("+", Var("v"), Num(4.0)), Num(4.0)));
        Expr(Var("v"));
        Expr(Op2("==", Var("v"), Num(2.25)));
        Expr(Op2("!=", Var("v"), Num(2.25)));
];;

let%expect_test "p1" =
    runCode testAssign; 
    [%expect {| 
                4.
                5. 
                2.25
                1.
                0.
            |}]
    ;;

(* 
    If else test
    v = 0

    if (v - 4) < 0.0 then
    ++v
    else
    --v
*)
let ifelse: block = [
    Assign("v", Num(0.0));
    If(
        Op2("<", Op2("-",  Var("v"), Num(4.0)), Num(0.0)), 
        [Expr(Op1("++", Var("v")))], 
        [Expr(Op1("--", Var("v")))]
    );
];;

let%expect_test "ifelse" =
    runCode ifelse;
    [%expect {| 
              1.
              |}]
    ;;


(* 
    while(k < 10){
        if(k == 4){
            break;
        }
        ++k;
    }
*)
let while_break_test: block = [
    While(
        Op2("<", Var("k"), Num(10.0)),
        [   
            If(
                Op2("==", Var("k"), Num(4.0)),
                [Break],
                []
            );

            Expr(Op1("++", Var("k")));
        ]
    );
];;

let%expect_test "while_break_test" =
    runCode while_break_test; 
    [%expect {| 
                1.
                2.
                3.
                4.
            |}]
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
let for_test: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Assign("i",(Op2("+", Var("i"), Num(1.0)))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )];
    );
    Expr(Var("v"))
];;


let%expect_test "for_test" =
    runCode for_test; 
    [%expect {| 362880. |}]
    ;;


(* 
Function TEST 1
    f(x){
        ++x
    }
    x 
*)

let foo: block = 
    [
        FctDef("foo", ["x"], [
            Expr(Op1("++", Var("x")))
        ]);
        Expr(Fct("foo", [Num(3.0)]));
        Expr(Var("x"));
    ]
    ;;

let%expect_test "foo" = 
    runCode foo;
    [%expect {| 
    4.
    0.
    0.
    |}]
    ;;


(* 
Function TEST 2
    square(x){
        return x * x
    }
*)

let square: block = 
    [
        FctDef("square", ["x"], [
            Return(Op2("*", Var("x"), Var("x")))
        ]);
        Expr(Fct("square", [Num(2.0)]));
    ]
    ;;

let%expect_test "square" = 
    runCode square;
    [%expect {| 
                4.
            |}]
    ;;




(*  
    Fibbonaci sequence
    0, 1, 1, 2, 3, 5, 8
    define f(x) {
        if (x <= 1.0) then
            return x
        else
            return (f(x-1)+f(x-2))
    }
    f(3)
    f(5)
 *)
let fib: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<=", Var("x"), Num(1.0)),
                [Return(Var("x"))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]
    ;;


let%expect_test "fib" =
    runCode fib; 
    [%expect {| 
        2.
        5.
    |}]
    ;;