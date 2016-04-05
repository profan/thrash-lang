type _ value' =
    | Bool : bool -> bool value'
    | Var : string -> string value'
    | Int : int -> int value'

type _ expr' =
    | Value : 'a value' -> 'a expr'
    | If : bool expr' * 'a expr' * 'a expr' -> 'a expr'
    | Eq : 'a expr' * 'a expr' -> bool expr'
    | Lt : int expr' * int expr' -> bool expr'
    | BinOp : char * 'a expr' * 'a expr' -> 'a expr'
    | Neg : 'a expr' -> 'a expr'

let rec eval' : type a. a expr' -> a = function
    | Value (Bool b) -> b
    | Value (Int i) -> i
    | Value (Var v) -> v
    | If (b, l, r) -> if eval' b then eval' l else eval' r
    | Eq (a, b) -> (eval' a) = (eval' b)
    | Lt (a, b) -> a < b
    | BinOp(op, a, b) -> eval' a
    | Neg (a) -> eval' a;;
