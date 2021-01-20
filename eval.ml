(* Evaluator. *)

(* Helper function.
 * Take n elements from a list. *)
let rec take n ls =
    if n = 0 then
        []
    else if n < 0 then
        failwith "Number of elements requested is less than zero."
    else
        match ls with
        | [] ->
            failwith "Number of elements requested is greater than length of list."
        | x::xs ->
            x :: take (n - 1) xs

(* Helper function.
 * Drop n elements from a list. *)
let rec drop n ls =
    if n = 0 then
        ls
    else if n < 0 then
        failwith "Number of elements requested is less than zero."
    else
        match ls with
        | [] ->
            failwith "Number of elements requested is greater than length of list."
        | _::xs ->
            drop (n - 1) xs

type cons_list =
    | List of Env.value list
    | Dotted_list of Env.value list * Env.value

let cons_list_of_cons p =
    (match p with
    | Env.Cons _ | Env.Nil -> ()
    | _ -> failwith "Only cons and null allowed.");
    let rec loop acc rem =
        (match rem with
        | Env.Nil -> List (List.rev acc)
        | Env.Cons ({contents=car}, {contents=cdr}) ->
            loop (car::acc) cdr
        | x -> Dotted_list (List.rev acc, x)) in
    loop [] p

let rec list_to_cons l last =
    match l with
    | [] -> last
    | x::xs -> Env.Cons (ref x, ref (list_to_cons xs last))

let rec eval exp env macros =
    match exp with
    | Env.Nil -> failwith "Invalid: (). The procedure to apply is missing."
    (* These are produced by the evaluator (e.g. through the evaluation
     * of the lambda form), and stored in the environment, but not consumed by
     * the evaluator. *)
    | Env.Lambda _
    | Env.Primitive _ -> failwith ""
    (* Self evaluating expressions. *)
    | Env.Bool _
    | Env.Int _
    | Env.Real _
    | Env.Char _
    | Env.String _ -> ref exp
    | Env.Id name -> Env.lookup env name
    | Env.Vector _ as v -> ref v
    | Env.Cons ({contents=car}, {contents=cdr}) ->
        (match car with
        | Env.Id "quote" ->
            (match cdr with
            | Env.Cons ({contents=v}, {contents=Env.Nil}) ->
                ref v
            | _ -> failwith "Invalid quote.")
        (* Simple non standards compliant macro system. *)
        | Env.Id "define-macro" ->
            (match cdr with
            | Env.Cons ({contents=Env.Id macro_name},
                        {contents=Env.Cons ({contents=transformer},
                                            {contents=Env.Nil})}) ->
                Hashtbl.add macros macro_name transformer;
                ref (Env.Id "banana!")
            | _ -> failwith "Invalid define-macro")
        | Env.Id "define" ->
            (match cons_list_of_cons cdr with
            (* (define x y) *)
            | List (Env.Id var::[value]) ->
                let v = eval value env macros in
                Env.add env var v;
                ref (Env.Id "banana!")  (* Return value for define is unspecified. *)
            (* (define (f x) ...)
             * (define (f . x) ...)
             * (define (f x . y) ...) *)
            | List (Env.Cons _ as l::body) ->
                (match cons_list_of_cons l with
                (* (define (f x) x) *)
                | List (Env.Id proc_name::arg_ids) ->
                    Env.add env proc_name (ref (Env.Lambda (env, list_to_cons arg_ids Env.Nil, body)));
                    ref (Env.Id "banana!")
                (* (define (f . x) ...) *)
                | Dotted_list (Env.Id proc_name::[], rest_id) ->
                    Env.add env proc_name (ref (Env.Lambda (env, rest_id, body)));
                    ref (Env.Id "banana!")
                (* (define (f x . y) ...) *)
                | Dotted_list (Env.Id proc_name::arg_ids, rest_id) ->
                    Env.add env proc_name (ref (Env.Lambda (env, list_to_cons
                    arg_ids rest_id, body)));
                    ref (Env.Id "banana!")
                | _ -> failwith "Invalid function define.")
            | Dotted_list _ -> failwith "(define x y . z) form is invalid."
            | _ -> failwith "Invalid define.")
        | Env.Id "lambda" ->
            (match cons_list_of_cons cdr with
            (* (lambda () ...) *)
            | List (Env.Nil::body) ->
                ref (Env.Lambda (env, Env.Nil, body))
            (* (lambda (x) ...)
               (lambda (x . y) ...) *)
            | List (Env.Cons _ as l::body) ->
                (* Check validity of argument list. *)
                let all_ids vals =
                    List.iter
                        (fun e ->
                            match e with
                            | Env.Id _ -> ()
                            | _ -> failwith "Arguments in lambda must be ids.")
                        vals in
                (match cons_list_of_cons l with
                (* (lambda (x) ...) *)
                | List arg_ids -> all_ids arg_ids
                (* (lambda (x . y) ...) *)
                | Dotted_list (arg_ids, rest_id) ->
                    all_ids arg_ids;
                    (match rest_id with
                    | Env.Id _ -> ()
                    | _ -> failwith "Arguments in lambda must be ids."));
                ref (Env.Lambda (env, l, body))
            (* (lambda x ...) *)
            | List (Env.Id _ as arg::body) ->
                ref (Env.Lambda (env, arg, body))
            | Dotted_list _ -> failwith "(lambda x y . z) form is invalid."
            | _ -> failwith "Invalid lambda.")
        | Env.Id "if" ->
            (match cons_list_of_cons cdr with
            | List [predicate; consequent; alternative] ->
                (match !(eval predicate env macros) with
                | Env.Bool false -> eval alternative env macros
                | _ -> eval consequent env macros)
            | Dotted_list _ -> failwith "(if x y . z) form is invalid."
            | _ -> failwith "Invalid if.")
        (* Apply. *)
        | proc_exp ->
            let eval_proc proc_exp cdr env macros =
                let args = match cons_list_of_cons cdr with
                           | List l -> l
                           | Dotted_list _ ->
                                 failwith "APPLY (x y . z) form is invalid." in
                (match !(eval proc_exp env macros) with
                | Env.Primitive f ->
                    f env macros args
                | Env.Lambda (proc_env, arg_ids, body) ->
                    let arg_vals = List.map (fun e -> eval e env macros) args in
                    apply proc_env arg_ids body arg_vals macros
                | _ -> failwith "Cannot APPLY non-procedure.") in
            (match proc_exp with
            | Env.Id name -> (
                try  (* If macro. *)
                    let transformer = Hashtbl.find macros name in
                    let to_expand =
                        Env.Cons (ref transformer,
                                  ref (Env.Cons (ref (Env.Cons (ref (Env.Id "quote"),
                                                                ref (Env.Cons (ref exp,
                                                                               ref Env.Nil)))),
                                                 ref Env.Nil))) in
                    let expanded = eval to_expand env macros in
                    eval !expanded env macros
                with  (* If not macro. *)
                    Not_found ->
                        eval_proc proc_exp cdr env macros)
            | _ -> eval_proc proc_exp cdr env macros))

and apply proc_env arg_ids body arg_vals macros =
    (* The environment in which to evaluate the body of the procedure. *)
    let temp_env = Env.make_env (Some proc_env) in (
    (* Add values of operands to the temporary environment. *)
    (match arg_ids with
    (* (lambda () ...)*)
    | Env.Nil -> ()
    (* (lambda x ...) *)
    | Env.Id id ->
        let rec list_to_cons = function
            | [] -> Env.Nil
            | x::xs -> Env.Cons (x, ref (list_to_cons xs)) in
        Env.add temp_env id (ref (list_to_cons arg_vals))
    (* (lambda (x) ...)
     * (lambda (x . y) ...) *)
    | Env.Cons _ as l ->
        (match cons_list_of_cons l with
        | List l ->
            let ids = List.map
                        (fun e ->
                            match e with
                            | Env.Id id -> id
                            | _ -> failwith "Argument list can only contain ids.")
                        l in
            if List.length ids = List.length arg_vals then
                List.iter2
                    (fun id v ->
                        Env.add temp_env id v)
                    ids arg_vals
            else
                failwith "APPLY: Incorrect number of arguments."
        | Dotted_list (l, last) ->
            let ids = List.map
                        (fun e ->
                            match e with
                            | Env.Id id -> id
                            | _ -> failwith "Argument list can only contain ids.")
                        l in
            (if List.length arg_vals >= List.length ids then
                List.iter2
                    (fun id v ->
                        Env.add temp_env id v)
                    ids (take (List.length ids) arg_vals)
            else
                failwith "APPLY: Incorrect number of arguments.");
            match last with
            | Env.Id id ->
                let rec list_to_cons = function
                    | [] -> Env.Nil
                    | x::xs -> Env.Cons (x, ref (list_to_cons xs)) in
                Env.add temp_env id (ref (list_to_cons (drop (List.length ids) arg_vals)))
            | _ -> failwith "Argument list can only contain ids.")
    | _ -> failwith "Invalid lambda var ids.");
    let rec eval_body env macros = function
        | [x] ->  (* Last clause in body. *)
            (* This makes tail calls possible. Underlying tail call
             * mechanism provided for free by OCaml. *)
            eval x env macros
        | x::xs ->
            (ignore (eval x env macros);
             eval_body env macros xs)
        | _ -> failwith "LAMBDA must have a body." in
    eval_body temp_env macros body)
