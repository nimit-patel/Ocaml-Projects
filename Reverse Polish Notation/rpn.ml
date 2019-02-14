(* Sum type *)
type result = 
  | Num of float 
  | Error of string 
  ;;

let calc stack op =
    let second = Stack.pop stack in
    let first  = Stack.pop stack in
    (op first second)
  ;;

  (*  with
    | Stack.Empty -> print_string "Invalid RPN Expression!"
    | Division_by_zero -> print_string "Division by zero!"
    | _ -> print_string "Something went wrong!" *)

let parse token stack =
  match token with
  | "*" -> Stack.push ( calc stack  ( *. ) ) stack
  | "/" -> Stack.push ( calc stack  ( /. ) ) stack
  | "+" -> Stack.push ( calc stack  ( +. ) ) stack
  | "-" -> Stack.push ( calc stack  ( -. ) ) stack
  | "^" -> Stack.push ( calc stack  ( ** ) ) stack
  |  _  -> Stack.push ( float_of_string(token) ) stack
  ;;

  // token to evaluate --> result
  // tokens

  
let rpn (expression) = 

  (* Get a list of tokens for an expression *)
  let regex = Str.regexp " +" in
  let tokens = Str.split regex expression in
  (* Create a new stack *)
  let stack = Stack.create () in
  (* Visit each token and perform necessary operation *)
  List.iter (fun token -> parse token stack) tokens;
  Stack.iter (fun top -> print_float top; print_newline ();) stack
  ;; 

(* 2. Interprets each line as a RPN with " "(space) as a separator

3. Evaluates each RPN and either produces an error 
(if the RPN is incorrect) or prints a result if it is correct. 
The result or the error are printed on a separate line (in order) 
for each RPN in the input (one line in, one line out)

You only need to implement floating point numbers 
(no distinction between integer and float), implement the 
following binary operators: +,-,*,/,^(power, i.e x^3 is x cubed).
 No need to implement any unary operator.*)

(* read lines of input *)
let rec read_lines () =
    try let line = read_line () in
        rpn (line); 
        read_lines ();
    with
      End_of_file -> () 
    ;;

let () =
    read_lines ();