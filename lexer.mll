{
    (* Header. *)

    let bool_of_sym = function "#t" | "#true" -> true
                             | "#f" | "#false" -> false
                             | _ -> failwith "BOOL_OF_SYM"

    (* Helper function.
     * string -> list of chars *)
    let char_list_of_string s =
        let rec loop index l =
            if index < 0 then l
            else loop (index - 1) (s.[index]::l) in
        loop ((String.length s) - 1) []

    (* Helper function.
     * list of chars -> string *)
    let string_of_char_list l =
        let rec loop current remaining =
            match remaining with
            | [] -> current
            | (c::cs) -> loop (current ^ String.make 1 c) cs in
        loop "" l

    (* "#\a" -> 'a'
     * "#\space" -> ' '
     * "#\newline" -> '\n' *)
    let get_char (s:string) : char =
        match char_list_of_string s with
        | '#' :: '\\' :: [c] -> c
        | '#' :: '\\' :: c ->
            (match string_of_char_list c with
                | "newline" -> '\n'
                | "space" -> ' '
                | "tab" -> '\t'
                | _ -> failwith ("Invalid character: " ^ s))
        | _ -> failwith ("Invalid character: " ^ s)
}

let whitespace = [' ' '\t' '\n']
let letter = ['A' - 'Z' 'a' - 'z' '+' '-' '*' '/' '=' '<' '>' '?' '!']
let digit = ['0' - '9']
let integer    = '-'? digit+
let identifier = letter (letter | digit)*
let str = '"' [^ '"']* '"'
let boolean = "#t" | "#f" | "#true" | "#false"
let comment = ';' [^ '\n']* '\n'
let character = identifier | digit

rule lex = parse
  | whitespace  { lex lexbuf }  (* Discard whitespace *)
  | '(' { Parser.LPAREN }
  | ')' { Parser.RPAREN }
  | '[' { Parser.LSPAREN }  (* Square bracket. *)
  | ']' { Parser.RSPAREN }
  | '.' { Parser.DOT }
  | "'" { Parser.QUOTE }  (* Single quote. *)
  | "`" { Parser.QUASIQUOTE }  (* Backtick. *)
  | "," { Parser.UNQUOTE }  (* Comma. *)
  | ",@" { Parser.UNQUOTE_SPLICING }
  | (digit+ '.' digit*) as r { Parser.REAL (float_of_string r) }
  | ("#\\" character) as c { Parser.CHAR (get_char c) }
  | "#\\ " { Parser.CHAR ' ' }  (* #\  represents the space character. *)
  | boolean as b { Parser.BOOL (bool_of_sym b) }
  | integer as i { Parser.INT (int_of_string i) }
  | identifier as id { Parser.ID id }
  | '"' { Parser.STRING (string_scanner (Buffer.create 100) lexbuf) }
  | '#' { Parser.HASH }
  | comment { lex lexbuf }  (* Discard comments. *)
  | eof { Parser.EOF }
  | _ { raise (Failure ("unrecognized token: " ^ (Lexing.lexeme lexbuf))) }

(* Special scanner for strings. Handles newlines and the escape of quotes.
 * Reference: https://medium.com/@huund/recipes-for-ocamllex-bb4efa0afe53 *)
and string_scanner buf = parse
    | [^ '"' '\n' '\\']+ {
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        string_scanner buf lexbuf }
    | '\n' {
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        Lexing.new_line lexbuf;
        string_scanner buf lexbuf }
    | '\\' '"' {
        Buffer.add_char buf '"';
        string_scanner buf lexbuf }
    | '\\' {
        Buffer.add_char buf '\\';
        string_scanner buf lexbuf }
    | '"' { Buffer.contents buf }  (* End of string. Return. *)
    | eof { failwith "EOF inside a string" }
    (* If there happen to be other cases that haven't been considered. *)
    | _   { failwith ("Unable to handle string: " ^ (Lexing.lexeme lexbuf)) }

(* Trailer. *)
{}
