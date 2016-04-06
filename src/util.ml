let gensym_counter = ref 0
let gensym : unit -> string = fun () -> 
    let n = !gensym_counter in
    let() = incr gensym_counter in
        if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
        else "t" ^ string_of_int n

let reduce f lst =
    match lst with
    | head::tail -> List.fold_left f head tail
    | [] -> failwith "Empty List?"
