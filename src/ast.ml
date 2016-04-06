type _ value' =
    | Bool : bool -> bool value'
    | Var : string -> string value'
    | Int : int -> int value'

type _ expr' =
    | Value : 'a value' -> 'a expr'
    | If : bool expr' * 'a expr' * 'a expr' -> 'a expr'
    | BinOp : char * 'a expr' * 'a expr' -> 'a expr'
    | UnaryOp : char * 'a expr' -> 'a expr'

let rec eval' : type a. a expr' -> a = function
    | Value (Bool b) -> b
    | Value (Int i) -> i
    | Value (Var v) -> v
    | If (b, l, r) -> if eval' b then eval' l else eval' r
    | BinOp(op, a, b) -> eval' a
    | UnaryOp(op, a) -> eval' a ;;
