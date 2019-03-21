# BC
Basic calculator evaluator using Ocaml.

## Ocaml version
4.05.0

## Variable scoping rules
Global scope is used expect for functions.

#### Variable scoping rules for a function 
The arguments of a function and any variable in the function body that does not exist in the global scope are considered to be in the local scope.

## How to test the code?
Write a block (statement list) of code and use the runCode (code : block) method. Finally, to run the test use the following command:

```
dune runtest
```  

### Example test
```
(*
Function TEST
fact(x){
    if(x <= 0){
        return 1
    }
    return n * fact(n - 1);
}
*)

let fact: block = 
    [
        FctDef("fact", ["x"], [
            If(
                Op2("<=", Var("x"), Num(0.0)),
                [Return(Num(1.0))],
                [Return(
                    Op2("*", 
                    Var("x"), 
                    Fct("fact", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("fact", [Num(4.0)]));
        Expr(Fct("fact", [Num(10.0)]));
    ]
;;


let%expect_test "fact" =
    runCode fact; 
    [%expect {| 
                24.
                3628800.
            |}]
;;
```


