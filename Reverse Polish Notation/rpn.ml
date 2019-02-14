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
    | Stack.Empty      -> Error "Invalid RPN Expression: not enough operands!" 
    | _                -> Error "Something went wrong!"
  ;;

let get_float_value token stack : result = 
  try 
    let float_value = float_of_string(token) in
    Stack.push float_value stack;
    Num float_value
  with
    | _                 -> Error ("Error: " ^ token ^ " cannot be converted to a float")
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
                else Error "Invalid RPN expression: not enough operators!"
  | hd :: tl -> let result = (parse_token hd stack) in
                let is_type_float = is_float result in
                if is_type_float then eval tl stack 
                else result
  ;;


let rpn (expression) = 
  let regex = Str.regexp " +" in               (* regex space separator *)
  let tokens = Str.split regex expression in   (* Get a list of tokens for an expression *)
  let stack = Stack.create () in               (* Creates a new stack *)
  let result = (eval tokens stack) in          (* result of rpn *)

  match result with
  | Num num -> print_float (num); print_newline ();
  | Error error -> print_endline (error); 
  ;;
  

let rec read_lines () =                         (* read lines of input *)
    try let line = read_line () in
        rpn (line);
        read_lines ();
    with
      End_of_file -> () 
    ;;

let () =
    read_lines ();