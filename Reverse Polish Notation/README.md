# RPN

Evaluates reverse polish notation. An expression is entered one per line where each line 
contains either floating point number, an operator (*, /, +, -, ^ ), or a space.
For each line, tokens are seperated by a space. If RPN is valid, result is printed on a 
new line. For bad input or invalid expression, program
will output an error message.

### How to run the code?

Version: The OCaml toplevel, version 4.01.0+ocp1

```
ocamlc str.cma -o rpn rpn.ml 
ocamlrun rpn
```  

To stop the program

```
ctrl + z
```

### Example input/ouput

```
1 2 +
3.
```

