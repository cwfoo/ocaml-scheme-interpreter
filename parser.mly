%{  (* Header. *)
let rec cons_of_list l last =
    match l with
    | [] -> last  (* The last element at the end of the chain of conses. *)
    | x::xs -> Sexpr.Cons (x, cons_of_list xs last)
%}

/* Ocamlyacc declarations */
%token LPAREN RPAREN
%token LSPAREN RSPAREN
%token DOT
%token <bool> BOOL
%token <int> INT
%token <float> REAL
%token <string> ID
%token <char> CHAR
%token <string> STRING
%token QUOTE
%token QUASIQUOTE
%token UNQUOTE
%token UNQUOTE_SPLICING
%token HASH
%token EOF

%start parse
%type <Sexpr.sexpr option> parse
%type <Sexpr.sexpr>        sexpr
%type <Sexpr.sexpr>        atom
%type <Sexpr.sexpr list>   slist
%type <Sexpr.sexpr list>   sexpr_list

%%

/* Grammar rules. */
parse: sexpr { Some $1 }
  | EOF { None }
;

sexpr: atom { $1 }
  | quoted { Sexpr.Cons (Sexpr.Id "quote", Sexpr.Cons ($1, Sexpr.Nil)) }
  | quasiquoted { Sexpr.Cons (Sexpr.Id "quasiquote", Sexpr.Cons ($1, Sexpr.Nil)) }
  | unquoted { Sexpr.Cons (Sexpr.Id "unquote", Sexpr.Cons ($1, Sexpr.Nil)) }
  | unquoted_splicing { Sexpr.Cons (Sexpr.Id "unquote-splicing",
                                    Sexpr.Cons ($1, Sexpr.Nil)) }
  | slist { cons_of_list $1 Sexpr.Nil }
  | dotted_slist { match $1 with (l, e) -> cons_of_list l e }
  | vector { Sexpr.Vector $1 }
;

atom: BOOL { Sexpr.Bool $1 }
  | INT { Sexpr.Int $1 }
  | REAL { Sexpr.Real $1 }
  | CHAR { Sexpr.Char $1 }
  | STRING { Sexpr.String $1 }
  | ID { Sexpr.Id $1 }
;

quoted: QUOTE sexpr { $2 }
;

quasiquoted: QUASIQUOTE sexpr { $2 }
;

unquoted: UNQUOTE sexpr { $2 }
;

unquoted_splicing: UNQUOTE_SPLICING sexpr { $2 }
;

/* List of S-expressions, surrounded by parentheses. */
slist: LPAREN RPAREN { [] }
  | LSPAREN RSPAREN { [] }
  | LPAREN sexpr_list RPAREN { $2 }
  | LSPAREN sexpr_list RSPAREN { $2 }
;

dotted_slist: LPAREN sexpr_list DOT sexpr RPAREN { ($2, $4) }
  | LSPAREN sexpr_list DOT sexpr RSPAREN { ($2, $4) }
;

vector: HASH LPAREN RPAREN { [||] }
  | HASH LSPAREN RSPAREN { [||] }
  | HASH LPAREN sexpr_list RPAREN { Array.of_list $3 }
  | HASH LSPAREN sexpr_list RSPAREN { Array.of_list $3 }
;

/* Contents of a list of s-expressions, without the parentheses.
 * Empty lists are valid s-expressions.
 */
sexpr_list: sexpr { [$1] }
  | sexpr_list sexpr { $1 @ [$2] }
;

%%

(* trailer *)
