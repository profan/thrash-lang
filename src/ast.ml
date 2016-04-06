let reduce f lst =
    match lst with
    | head::tail -> List.fold_left f head tail
    | [] -> failwith "Empty List?"

type _ value' =
    | Bool : bool -> bool value'
    | Var : string -> string value'
    | Int : int -> int value'

type value =
    | Bool of bool
    | Int of int

type _ expr' =
    | Var : string -> 'a expr'
    | Value : value -> 'a expr'
    | List : 'a expr' list -> 'a expr'
    | If : 'a expr' * 'a expr' * 'a expr' -> 'a expr'
    | While : 'a expr' * 'a expr' -> 'a expr'
    | For : 'a expr' * 'a expr' * 'a expr' -> 'a expr'
    | BinOp : char * 'a expr' * 'a expr' -> 'a expr'
    | UnaryOp : char * 'a expr' -> 'a expr'
    | Let : string * 'a expr' -> 'a expr'

let rec print_ast' : type a. a expr' -> string = function
    | Var (var) -> var
    | Value (Bool b) -> (string_of_bool b)
    | Value (Int i) -> (string_of_int i)
    | List (things) ->
        List.map (fun (e) -> (print_ast' e)) things
        |> reduce (fun acc e -> acc ^ " " ^ e)
        |> Printf.sprintf "(List %s)"
    | If (b, l, r) ->
        Printf.sprintf "(If (%s) (%s) (%s)" (print_ast' b) (print_ast' l) (print_ast' r)
    | While (cond, body) ->
        Printf.sprintf "(While (%s) (%s))" (print_ast' cond) (print_ast' body)
    | For (var, cond, body) ->
        Printf.sprintf "(For %s (%s) (%s))" (print_ast' var) (print_ast' cond) (print_ast' body)
    | BinOp (op, a, b) ->
        Printf.sprintf "(BinOp %c %s %s)" op (print_ast' a) (print_ast' b)
    | UnaryOp (op, a) ->
        Printf.sprintf "(UnaryOp %c %s)" op (print_ast' a)
    | Let (var, e) ->
        Printf.sprintf "(Let %s %s)" var (print_ast' e)

let rec eval' : type a. a expr' -> string = function
    | Var (var) -> var
    | Value (Bool b) -> (string_of_bool b)
    | Value (Int i) -> (string_of_int i)
    | List (things) ->
        List.map (fun (e) -> (eval' e)) things
        |> reduce (fun acc e -> acc ^ " " ^ e)
        |> Printf.sprintf "%s;"
    | If (b, l, r) ->
        Printf.sprintf "if %s then \n %s \nelse\n %s \nfi" (eval' b) (eval' l) (eval' r)
    | While (cond, body) ->
        Printf.sprintf "while %s; do \n%s \ndone" (eval' cond) (eval' body)
    | For (var, cond, body) ->
        Printf.sprintf "for %s in %s \ndo\n %s \ndone" (eval' var) (eval' cond) (eval' body)
    | BinOp (op, a, b) ->
        Printf.sprintf "%s %c %s" (eval' a) op (eval' b)
    | UnaryOp (op, a) ->
        Printf.sprintf "%c %s" op (eval' a)
    | Let (var, e) ->
        Printf.sprintf "local %s = %s" var (eval' e)
