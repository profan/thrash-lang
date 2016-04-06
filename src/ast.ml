let reduce f lst =
    match lst with
    | head::tail -> List.fold_left f head tail
    | [] -> failwith "Empty List?"

type _ value' =
    | Bool : bool -> bool value'
    | Var : string -> string value'
    | Int : int -> int value'

type _ expr' =
    | Value : 'a value' -> 'a expr'
    | List : 'a expr' list -> 'a expr'
    | If : 'a expr' * 'a expr' * 'a expr' -> 'a expr'
    | While : 'a expr' * 'a expr' -> 'a expr'
    | For : 'a expr' * 'a expr' * 'a expr' -> 'a expr'
    | BinOp : string * 'a expr' * 'a expr' -> 'a expr'
    | UnaryOp : string * 'a expr' -> 'a expr'
    | Let : string value' * 'a expr' -> 'a expr'

let rec eval' : type a. a expr' -> string = function
    | Value (Bool b) -> (string_of_bool b)
    | Value (Int i) -> (string_of_int i)
    | Value (Var v) -> v
    | List (things) ->
        List.map (fun (e) -> (eval' e)) things
        |> reduce (fun acc e -> acc ^ " " ^ e)
        |> Printf.sprintf "( %s )"
    | If (b, l, r) ->
        Printf.sprintf "if %s then \n %s else %s \n fi \n" (eval' b) (eval' l) (eval' r)
    | While (cond, body) ->
        Printf.sprintf "while %s; do \n %s \n done" (eval' cond) (eval' body)
    | For (var, cond, body) ->
        Printf.sprintf "for %s in %s \n do \n %s \n done" (eval' var) (eval' cond) (eval' body)
    | BinOp(op, a, b) ->
        Printf.sprintf "%s %s %s" (eval' a) op (eval' b)
    | UnaryOp(op, a) ->
        Printf.sprintf "%s %s" op (eval' a)
    | Let(Var(var), e) ->
        Printf.sprintf "local %s = %s" var (eval' e)
