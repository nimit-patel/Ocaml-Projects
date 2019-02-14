type result =                                   (* Sum type either float or string *)
  | Num of float 
  | Error of string 
  ;;

let calc stack op : result =
  try
    Num (op (Stack.pop stack) (Stack.pop stack)) (* Evaluate expression by popping two values from stack*)
  with
    | Stack.Empty      -> Error "Invalid RPN Expression: not enough operands!" 
    | _                -> Error "Something went wrong!"
  ;;

let get_float_value token stack : result = 
  try 
    Num (float_of_string(token))
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

let is_float value : bool= 
  match value with
  | Num num -> true
  | Error error -> false
  ;;

let rec eval tokens stack : result=
  match tokens with 
  | []         -> if (Stack.length stack) = 1       (* Exactly one value should be on stack after evaluating rpn *)
                  then Num    (Stack.pop stack) 
                  else Error "Invalid RPN expression: not enough operators!"
  | curr::rest -> let result = (parse_token curr stack) in
                  match result with
                  | Num num -> Stack.push num stack; eval rest stack
                  | _       -> result
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