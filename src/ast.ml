type value =
    | Bool of bool
    | Int of int

(* we probably don't need this as a GADT right now, but nonetheless. *)
type _ expr' =
    | Var : string -> 'a expr'
    | Value : value -> 'a expr'
    | List : 'a expr' list -> 'a expr'
    | If : 'a expr' * 'a expr' * 'a expr' -> 'a expr'
    | While : 'a expr' * 'a expr' -> 'a expr'
    | For : 'a expr' * 'a expr' * 'a expr' -> 'a expr'
    | Cond : ('a expr' * 'a expr') list -> 'a expr'
    | BinOp : string * 'a expr' * 'a expr' -> 'a expr'
    | UnaryOp : string * 'a expr' -> 'a expr'
    | LetFn : string * string list * 'a expr' -> 'a expr'
    | Call : string * 'a expr' list -> 'a expr'
    | Let : string * 'a expr' -> 'a expr'

let print_params params = Util.reduce (fun acc e -> acc ^ " " ^ e) params

let rec print_ast' : type a. a expr' -> string = function
    | Var (var) -> var
    | Value (Bool b) -> (string_of_bool b)
    | Value (Int i) -> (string_of_int i)
    | List (things) ->
        List.map (fun (e) -> (print_ast' e)) things
        |> Util.reduce (fun acc e -> acc ^ " " ^ e)
        |> Printf.sprintf "(List %s)"
    | If (b, l, r) ->
        Printf.sprintf "(If %s %s %s)" (print_ast' b) (print_ast' l) (print_ast' r)
    | While (cond, body) ->
        Printf.sprintf "(While %s %s)" (print_ast' cond) (print_ast' body)
    | For (var, cond, body) ->
        Printf.sprintf "(For %s %s %s)" (print_ast' var) (print_ast' cond) (print_ast' body)
    | Cond (clauses) ->
        List.map (fun (cnd, thn) -> ((print_ast' cnd), (print_ast' thn))) clauses
        |> List.fold_left (fun acc (cnd, thn) -> acc ^ " (" ^ cnd ^ " " ^ thn ^ ")") ""
        |> Printf.sprintf "(Cond%s)"
    | BinOp (op, a, b) ->
        Printf.sprintf "(BinOp %s %s %s)" op (print_ast' a) (print_ast' b)
    | UnaryOp (op, a) ->
        Printf.sprintf "(UnaryOp %s %s)" op (print_ast' a)
    | LetFn (fn, params, e) ->
        Printf.sprintf "(LetFn %s %s %s)" fn (print_params params) (print_ast' e)
    | Call (fn, args) ->
        Printf.sprintf "(Call %s%s)" fn (List.fold_left (fun acc e -> acc ^ " " ^ (print_ast' e)) "" args)
    | Let (var, e) ->
        Printf.sprintf "(Let %s %s)" var (print_ast' e)

let rec eval' : type a. a expr' -> string = function
    | Var (var) -> "$" ^ var
    | Value (Bool b) -> (string_of_bool b)
    | Value (Int i) -> (string_of_int i)
    | List (things) ->
        List.map (fun (e) -> (eval' e)) things
        |> Util.reduce (fun acc e -> acc ^ " " ^ e)
        |> Printf.sprintf "%s;"
    | If (b, l, r) ->
        Printf.sprintf "if [ %s ]; then \n %s \nelse\n %s \nfi" (eval' b) (eval' l) (eval' r)
    | While (cond, body) ->
        Printf.sprintf "while [ %s ]; do \n%s \ndone" (eval' cond) (eval' body)
    | For (var, cond, body) ->
        Printf.sprintf "for %s in %s \ndo\n %s \ndone" (eval' var) (eval' cond) (eval' body)
    | Cond (clauses) ->
        List.map (fun (cnd, thn) -> ((eval' cnd), (eval' thn))) clauses
        |> List.fold_left (fun acc (cnd, thn) -> acc ^ cnd ^ thn) ""
        |> Printf.sprintf "%s;"
    | BinOp (("and" | "or") as op, a, b) ->
        let new_op = match op with
        | "and" -> "-a"
        | "or" -> "-o"
        | _ -> failwith "impossible case"
        in
            Printf.sprintf "%s %s %s" (eval' a) new_op (eval' b)
    | BinOp (("+" | "-" | "/" | "*") as op, a, b) ->
        Printf.sprintf "$(%s %s %s)" (eval' a) op (eval' b)
    | BinOp (op, a, Value(Int(b))) ->
        let new_op = match op with
        | "=" -> "-eq"
        | ">" -> "-gt"
        | "<" -> "-lt"
        | _ -> op
        in
            Printf.sprintf "%s %s %d" (eval' a) new_op b
    | BinOp ("=", a, b) ->
        Printf.sprintf "%s == %s" (eval' a) (eval' b)
    | BinOp (op, a, b) ->
        Printf.sprintf "%s %s %s" (eval' a) op (eval' b)
    | UnaryOp (op, a) ->
        Printf.sprintf "%s%s" op (eval' a)
    | LetFn (fn, params, e) ->
        let (_, decls) = List.fold_left (fun (c, v) e ->
            (c + 1, v ^ Printf.sprintf "local $%s=$%d \n" e c))
            (0, "") params
        in
            Printf.sprintf "%s() {\n%s%s\n}" fn decls (eval' e)
    | Call (fn, args) ->
        Printf.sprintf "%s%s" fn (List.fold_left (fun acc e -> acc ^ " " ^ (print_ast' e)) "" args)
    | Let (var, If(b, l, r)) ->
        Printf.sprintf "%s=\"\"" var
    | Let (var, e) ->
        Printf.sprintf "%s=%s" var (eval' e)
