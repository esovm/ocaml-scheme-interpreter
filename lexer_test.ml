let string_of_token token =
    match token with
    | Parser.LPAREN -> "LPAREN"
    | Parser.RPAREN -> "RPAREN"
    | Parser.DOT -> "DOT"
    | Parser.STRING s -> "String: " ^ s
    | Parser.INT i -> "Int: " ^ (string_of_int i)
    | Parser.ID x -> "Id: " ^ x
    | Parser.BOOL true -> "Bool: #t"
    | Parser.BOOL false -> "Bool: #f"
    | Parser.EOF -> "EOF"
    | _ -> failwith ""

let lexer_test infile =
    let lexbuf = Lexing.from_channel infile in
    let rec loop () =
        let token = Lexer.lex lexbuf in
        print_endline (string_of_token token);
        match token with
        | Parser.EOF -> ()
        | _ -> loop ()
    in
        loop ()

let () =
    if Array.length Sys.argv <> 2 then
        prerr_endline ("usage: " ^ Sys.executable_name ^ " [filename]")
    else
        let infile = open_in Sys.argv.(1) in
        lexer_test infile;
        close_in infile
