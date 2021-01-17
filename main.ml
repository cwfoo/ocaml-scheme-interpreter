(* The main program. *)

(* Return a primitive environment. *)
let primitive_env () : Env.env * Env.macro_table =
    (* Create an empty environment. *)
    let root_env = Env.make_env None in
    let macros = Hashtbl.create 10 in
    (* Load primitives implemented in OCaml. *)
    Primitives.load_primitives root_env;
    (* Load primitives implemented in Scheme. *)
    Primitives.load_scm_file
        root_env
        macros
        (Filename.concat (Filename.dirname Sys.executable_name)
        "src-scheme/primitives.scm");
    (root_env, macros)

(* Read an s-expression string. *)
let input_scheme_string () : string =
    let stack = Stack.create () in
    let rec loop accumulated =
        let input =
            if accumulated = "" then
                read_line ()
            else
                (print_string "  ";  (* Indent if not on first line of input. *)
                read_line ())
        in
        (String.iter
            (fun c ->
                match c with
                | '(' | '[' ->
                    if Stack.is_empty stack then
                        Stack.push c stack
                    (* Check if '(' is within a string. *)
                    else if not (Stack.top stack = '"') then
                        Stack.push c stack
                | ')' ->
                    if Stack.is_empty stack then
                        failwith "Unbalanced parentheses."
                    (* Check if ')' is within a string. *)
                    else if Stack.top stack = '"' then
                        ignore ()
                    else if Stack.top stack = '(' then
                        ignore (Stack.pop stack)
                    else
                        failwith "Invalid parentheses."
                | ']' ->
                    if Stack.is_empty stack then
                        failwith "Unbalanced parentheses."
                    (* Check if ']' is within a string. *)
                    else if Stack.top stack = '"' then
                        ignore ()
                    else if Stack.top stack = '[' then
                        ignore (Stack.pop stack)
                    else
                        failwith "Invalid parentheses."
                | '"' as x ->
                    if Stack.is_empty stack then
                        Stack.push x stack
                    else if Stack.top stack = '\\' then
                        ignore (Stack.pop stack)  (* Pop \. *)
                    else if Stack.top stack = x then
                        ignore (Stack.pop stack)
                    else
                        Stack.push x stack
                | '\\' as x ->
                    if Stack.is_empty stack then
                        ignore ()
                    (* "\\" i.e. two consecutive backward slashes.*)
                    else if Stack.top stack = '\\' then
                        ignore (Stack.pop stack)  (* Pop \. *)
                    else
                        Stack.push x stack
                | _ ->
                    if Stack.is_empty stack then
                        ignore ()
                    else if Stack.top stack = '\\' then
                        ignore (Stack.pop stack))  (* Pop \. *)
            input;
        if Stack.is_empty stack then
            accumulated ^ input
        else
            loop (accumulated ^ input ^ String.make 1 '\n'))
    in
    loop ""

let repl env macros =
    (* Raise Sys.Break on Ctrl-C (SIGINT). *)
    Sys.catch_break true;
    let rec driver_loop () =
        print_string "> ";  (* Prompt for user input. *)
        try
            let input = input_scheme_string () in
            (try
                let lexbuf = Lexing.from_string input in
                let sexpr = (Parser.parse Lexer.lex) lexbuf in
                match sexpr with
                | None -> ()
                | Some s ->
                    let exp = Env.value_of_sexpr s in
                    let value = Eval.eval exp env macros in
                    print_endline ("= value: " ^ Env.string_of_value !value);
                    driver_loop ()
            with
                | Failure msg ->
                    print_endline ("Failure: " ^ msg);
                    driver_loop ()
                | Invalid_argument msg ->
                    print_endline ("Invalid argument: " ^ msg);
                    driver_loop ()
                | Parsing.Parse_error ->
                    print_endline "Parse error.";
                    driver_loop ()
                (* Ctrl-C. User wants to stop the evaluation. *)
                | Sys.Break ->
                    (print_endline "Interrupted.";
                    driver_loop ()))
        with
            (* Ctrl-C. User cancelled the input. *)
            | Sys.Break ->
                print_newline ();
                driver_loop ()
            (* Ctrl-D. User wants to exit. *)
            | End_of_file -> exit 0
    in
    print_endline "Scheme REPL";
    driver_loop ()

let () =
    match Sys.argv with
    | [|_|] ->
        let (env, macros) = primitive_env () in
        repl env macros
    | [|_; "-h"|] | [|_; "--help"|] ->
        let usage = "usage: ./scheme [filename] [-h | --help]" in
        print_endline usage
    | [|_; path|] ->
        let (env, macros) = primitive_env () in
        Primitives.load_scm_file env macros path
    | _ ->
        prerr_endline "Invalid arguments provided.";
        exit 1
