type result =                                   (* Sum type either float or string *)
  | Num of float 
  | Error of string 
  ;;

let calc stack op : result =
  if Stack.length stack > 1
  then Num   (op (Stack.pop stack) (Stack.pop stack)) (* Evaluate expression by popping two values from stack.
                                                      The second pop operation is done first so, evaluation 
                                                      is first <op> second *)
  else Error ("Invalid RPN Expression: not enough operands!") 
  ;;

let parse_token token stack : result =
  match token with
  | "*" -> calc stack ( *. )
  | "/" -> calc stack ( /. )
  | "+" -> calc stack ( +. )
  | "-" -> calc stack ( -. )
  | "^" -> calc stack ( ** )
  |  _  -> let float_val = float_of_string_opt(token) in
           match float_val with 
           | Some(flt)    -> Num   (flt)
           | None         -> Error ("Error: \'" ^ token ^ "\' cannot be converted to a float")
  ;;

let rec eval tokens stack : result=
  match tokens with 
  | []         -> if          (Stack.length stack) = 1    (* Exactly one value should be on stack after evaluating rpn *)
                  then Num    (Stack.pop stack) 
                  else Error  ("Invalid RPN expression: not enough operators!")
  | curr::rest -> let result = (parse_token curr stack) in
                  match result with
                  | Num num -> Stack.push num stack; eval rest stack
                  | _       -> result
  ;;

let rpn (expression) = 
  let regex = Str.regexp " +" in               (* Regex space separator *)
  let tokens = Str.split regex expression in   (* Get a list of tokens for an expression *)
  let stack = Stack.create () in               (* Creates a new stack *)
  let result = (eval tokens stack) in          (* Result of rpn *)

  match result with
  | Num num     -> print_float (num); print_newline ();
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