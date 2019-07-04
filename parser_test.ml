let parser_test infile =
    let lexbuf = Lexing.from_channel infile in
    let rec loop () =
        let sexpr = (Parser.parse Lexer.lex) lexbuf in
        match sexpr with
        | None -> ()
        | Some s ->
            print_endline (Sexpr.string_of_sexpr s);
            loop ()
    in
        loop ()

let () =
    if Array.length Sys.argv <> 2 then
        prerr_endline ("usage: " ^ Sys.executable_name ^ " [filename]")
    else
        let infile = open_in Sys.argv.(1) in
        parser_test infile;
        close_in infile
