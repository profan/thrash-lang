type _ value_g =
    | TBool : bool -> bool value_g
    | TInt : int -> int value_g

type _ expr_g =
    | TValue : 'a value_g -> 'a expr_g
    | TIf : bool expr_g * 'a expr_g * 'a expr_g -> 'a expr_g
    | TEq : 'a expr_g * 'a expr_g -> bool expr_g
    | TLt : int expr_g * int expr_g -> bool expr_g
