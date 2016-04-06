(* entry point for ocaml compiler, will do argument parsing and generally make the parts collaborate. *)

let call_with_channel (ch_open, ch_close) fn file =
    let channel = ch_open file in
    let result = fn channel in
        ch_close channel;
        result

let call_with_output_channel fn file = call_with_channel (open_out, close_out) fn file
let call_with_input_channel fn file = call_with_channel (open_in, close_in) fn file

let main ch =
    try
        Lexing.from_channel ch
        |> Grammar_parser.main Grammar_lexer.token
    with 
        | Grammar_lexer.Eof -> print_string "EOF"
        (* | Dyp.Syntax_error ->
            let (startpos, endpos) = Dyp.dyplex_lexbuf_position in *) 

let filename = "tests/first.calc";;
Printf.printf "executing: %s \n" filename;;
let ast = call_with_input_channel (fun ch -> main ch) filename;;

let compiled = eval' ast;;
Printf.printf "produced code: %s \n" compiled;;
