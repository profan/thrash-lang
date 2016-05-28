type value =
    | Bool of bool
    | Int of int

[@@deriving show]

(* we probably don't need this as a GADT right now, but nonetheless. *)
type expr =
    | Var of string
    | Value of value
    | List of expr list
    | If of expr * expr * expr
    | While of expr * expr
    | For of expr * expr * expr
    | Cond of (expr * expr) list
    | BinOp of string * expr * expr
    | UnaryOp of string * expr
    | LetFn of string * string list * expr
    | Call of string * expr list
    | Let of string * expr

[@@deriving show]

let rec eval : expr -> string = function
    | Var (var) -> "$" ^ var
    | Value (Bool b) -> (string_of_bool b)
    | Value (Int i) -> (string_of_int i)
    | List (things) ->
        List.map (fun (e) -> (eval e)) things
        |> Util.reduce (fun acc e -> acc ^ " " ^ e)
        |> Printf.sprintf "%s;"
    | If (b, l, r) ->
        Printf.sprintf "if [ %s ]; then \n %s \nelse\n %s \nfi" (eval b) (eval l) (eval r)
    | While (cond, body) ->
        Printf.sprintf "while [ %s ]; do \n%s \ndone" (eval cond) (eval body)
    | For (var, cond, body) ->
        Printf.sprintf "for %s in %s \ndo\n %s \ndone" (eval var) (eval cond) (eval body)
    | Cond (clauses) ->
        List.map (fun (cnd, thn) -> ((eval cnd), (eval thn))) clauses
        |> List.fold_left (fun acc (cnd, thn) -> acc ^ cnd ^ thn) ""
        |> Printf.sprintf "%s;"
    | BinOp (("and" | "or") as op, a, b) ->
        let new_op = match op with
        | "and" -> "-a"
        | "or" -> "-o"
        | _ -> failwith "impossible case"
        in
            Printf.sprintf "%s %s %s" (eval a) new_op (eval b)
    | BinOp (("+" | "-" | "/" | "*") as op, a, b) ->
        Printf.sprintf "$(%s %s %s)" (eval a) op (eval b)
    | BinOp (op, a, Value(Int(b))) ->
        let new_op = match op with
        | "=" -> "-eq"
        | ">" -> "-gt"
        | "<" -> "-lt"
        | _ -> op
        in
            Printf.sprintf "%s %s %d" (eval a) new_op b
    | BinOp ("=", a, b) ->
        Printf.sprintf "%s == %s" (eval a) (eval b)
    | BinOp (op, a, b) ->
        Printf.sprintf "%s %s %s" (eval a) op (eval b)
    | UnaryOp (op, a) ->
        Printf.sprintf "%s%s" op (eval a)
    | LetFn (fn, params, e) ->
        let (_, decls) = List.fold_left (fun (c, v) e ->
            (c + 1, v ^ Printf.sprintf "local $%s=$%d \n" e c))
            (0, "") params
        in
            Printf.sprintf "%s() {\n%s%s\n}" fn decls (eval e)
    | Call (fn, args) ->
        Printf.sprintf "%s%s" fn (List.fold_left (fun acc e -> acc ^ " " ^ (eval e)) "" args)
    | Let (var, If(b, l, r)) ->
        Printf.sprintf "%s=\"\"" var
    | Let (var, e) ->
        Printf.sprintf "%s=%s" var (eval e)
