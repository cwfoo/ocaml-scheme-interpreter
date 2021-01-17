(* Environments and values.  *)

type id = string

(* Value of data in environments. *)
type value =
    | Bool of bool
    | Int of int
    | Real of float
    | Char of char
    | String of string
    | Id of string
    | Cons of value ref * value ref
    | Nil
    | Vector of value ref array
    | Primitive of (env -> macro_table -> value list -> value ref)  (* Procedures defined using OCaml. *)
    (* (lambda <id list> <Ast.expr list>) *)
    | Lambda of env * value * value list  (* User-defined procedures. *)
and macro_table = (string, value) Hashtbl.t
and env = {
    parent : env option;  (* Parent frame of this environment. *)
    bindings : (id, value ref) Hashtbl.t;
}


let rec value_of_sexpr = function
    | Sexpr.Bool b -> Bool b
    | Sexpr.Int i -> Int i
    | Sexpr.Real r -> Real r
    | Sexpr.Char c -> Char c
    | Sexpr.String s -> String s
    | Sexpr.Id id -> Id id
    | Sexpr.Cons (car, cdr) ->
        Cons (ref (value_of_sexpr car), ref (value_of_sexpr cdr))
    | Sexpr.Nil -> Nil
    | Sexpr.Vector v ->
        let contents = Array.map (fun sx -> ref (value_of_sexpr sx)) v in
        Vector contents

(* Values. *)
let rec string_of_value = function
    | Int i -> (string_of_int i)
    | Real r -> (string_of_float r)
    | Bool true -> "#t"
    | Bool false -> "#f"
    | Char '\n' -> "#\\newline"
    | Char ' ' -> "#\\space"
    | Char '\t' -> "#\\tab"
    | Char c -> "#\\" ^ String.make 1 c
    | String s -> "\"" ^ s ^ "\""
    | Id s -> s
    | Nil -> "()"
    | Cons (car, cdr) as cons ->
        let rec string_of_cons acc rem =
            (match rem with
            | Cons ({contents=car}, {contents=Nil}) ->
                if acc = "" then
                    string_of_value car
                else
                    string_of_value car ^ " " ^ acc
            | Cons ({contents=car}, {contents=cdr}) ->
                (match cdr with
                | Cons _ ->
                    string_of_value car ^ " " ^ string_of_cons acc cdr
                | _ ->
                    string_of_value car ^ " . " ^ string_of_value cdr)
            | _ -> failwith "") in
        "(" ^ string_of_cons "" cons ^ ")"
    | Vector v ->
        let l = Array.to_list v in
        "#("
        ^ String.concat " " (List.map (fun e -> string_of_value !e) l)
        ^ ")"
    | Primitive _ -> "<primitive procedure>"
    | Lambda _ -> "<lambda expression>"

let string_of_value_type = function
    | Int _ -> "integer"
    | Real _ -> "real"
    | Bool _ -> "boolean"
    | Char _ -> "character"
    | Id _ -> "id"
    | String _ -> "string"
    | Cons _ -> "cons"
    | Nil -> "null"
    | Vector _ -> "vector"
    | Primitive _ | Lambda _ -> "procedure"

(* Environments. *)
let make_env parent_env =
    {
        parent = parent_env;
        bindings = Hashtbl.create 5;
    }

let rec lookup env name =
    let {parent; bindings} = env in
    try
        Hashtbl.find bindings name
    with
        Not_found -> match parent with  (* Lookup in parent frame. *)
                     | None -> failwith (name ^ " not found in environment. LOOKUP.")
                     | Some parent_env -> lookup parent_env name

(* Add a value to an environment. *)
let add env name ref_val =
    let {bindings; _} = env in
    Hashtbl.add bindings name ref_val  (* Side effecting. *)

let rec set env name value =
    let {parent; bindings} = env in
    try
        let v = Hashtbl.find bindings name in
        v := value
    with
        Not_found -> match parent with  (* Lookup in parent frame. *)
                     | None -> failwith (name ^ " not found in environment. SET.")
                     | Some parent_env -> set parent_env name value
