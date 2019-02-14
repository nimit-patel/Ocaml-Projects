(* Sum type *)
type result = 
  | Num of float 
  | Error of string 
  ;;

let calc stack op : result =
  try
    let second = Stack.pop stack in
    let first  = Stack.pop stack in
    Num (op first second)
  with
    | Stack.Empty      -> Error "Invalid RPN Expression!"
    | Division_by_zero -> Error "Division by zero!"
    | _                -> Error "Something went wrong!"
  ;;


let parse_token token stack : result =
  match token with
  | "*" -> calc stack ( *. )
  | "/" -> calc stack ( /. )
  | "+" -> calc stack ( +. )
  | "-" -> calc stack ( -. )
  | "^" -> calc stack ( ** )
  |  _  -> Num ( float_of_string(token) )
  ;;

let get_result result : string =
  match result with
  | Num num -> string_of_float num
  | Error error -> error
  ;;

let rec eval tokens stack =
  match tokens with 
  | []       -> print_string (get_result (Stack.pop stack;))
  | hd :: tl -> let val = parse_token hd in
                Stack.push val stack;
                match val with
                | Num num -> eval tl stack
  ;;


let rpn (expression) = 
  (* Get a list of tokens for an expression *)
  let regex = Str.regexp " +" in
  let tokens = Str.split regex expression in
  (* Create a new stack *)
  let stack = Stack.create () in
  eval tokens stack;
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