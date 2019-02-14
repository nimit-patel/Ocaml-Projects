(* Sum type *)
type result = 
  | Num of float 
  | Error of string 
  ;;

let calc stack op : result =
  try
    let second = Stack.pop stack in
    let first  = Stack.pop stack in
    Stack.push (op first second) stack;
    Num (op first second)
  with
    | Stack.Empty      -> Error "Invalid RPN Expression, Stack is empty!"
    | _                -> Error "Something went wrong!"
  ;;

let get_float_value token stack : result = 
  let float_value = float_of_string(token) in
  Stack.push float_value stack;
  Num float_value
  ;;

let parse_token token stack : result =
  match token with
  | "*" -> calc stack ( *. )
  | "/" -> calc stack ( /. )
  | "+" -> calc stack ( +. )
  | "-" -> calc stack ( -. )
  | "^" -> calc stack ( ** )
  |  _  -> get_float_value token stack
  ;;

let get_result result : string =
  match result with
  | Num num -> string_of_float num
  | Error error -> error
  ;;

let is_float value : bool= 
  match value with
  | Num num -> true
  | Error error -> false
  ;;

let rec eval tokens stack : result=
  match tokens with 
  | []       -> if (Stack.length stack) = 1 then Num (Stack.pop stack) 
                else Error "Invalid RPN expression!"
  | hd :: tl -> let value = (parse_token hd stack) in
                let is_type_float = is_float value in
                if is_type_float then eval tl stack 
                else value
  ;;


let rpn (expression) = 
  (* Get a list of tokens for an expression *)
  let regex = Str.regexp " +" in
  let tokens = Str.split regex expression in
  (* Create a new stack *)
  let stack = Stack.create () in
  let result = (eval tokens stack) in
  match result with
  | Num num -> print_float num; print_newline ();
  | Error error -> print_string error; print_newline ();
  ;;
  
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