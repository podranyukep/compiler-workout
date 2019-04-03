(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t
    (* function call    *) | Call  of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * int option
                                                            
    (* Expression evaluator

          val eval : env -> config -> t -> config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns resulting configuration
    *)                                                       
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)
         
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string                                                                                                                  
    *)
    ostap (                                      
      parse:
      !(Ostap.Util.expr 
             (fun x -> x)
         (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
        `Lefta, ["!!"];
        `Lefta, ["&&"];
        `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
        `Lefta, ["+" ; "-"];
        `Lefta, ["*" ; "/"; "%"];
              |] 
         )
         primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)
    let rec eval env (s, i, o) t =
        match t with
            | Read x -> (match i with
                | z :: i_rest -> (State.update x z s, i_rest, o)
                | _           -> failwith "Input read fail")
            | Write   e             -> (s, i, o @ [Expr.eval s e])
            | Assign (x, e)         -> (State.update x (Expr.eval s e) s, i, o)
            | Seq    (s1, s2) -> 
                let stmt = eval env (s, i, o) s1
                in eval env stmt s2
            | Skip            -> (s, i, o)
            | If (e, thenStmt, elseStmt) ->
                if (Expr.eval s e) != 0 then eval env (s, i, o) thenStmt else eval env (s, i, o) elseStmt
            | While (e, wStmt)  ->
                let r = Expr.eval s e in
                if r != 0 then eval env (eval env (s, i, o) wStmt) (While (e, wStmt)) else (s, i, o)
            | Repeat (ruStmt, e)  ->
                let (s, i, o) = eval env (s, i, o) ruStmt in
                let r = Expr.eval s e in
                if r != 0 then (s, i ,o) else eval env (s, i, o) t
            | Call (name, args) ->
                let (arg_names, locals, body) = env#definition name in
                let args = List.combine arg_names (List.map (Expr.eval s) args) in
                let state = State.push_scope s (arg_names @ locals) in
                let fun_env_w_args = List.fold_left (fun s (name, value) -> State.update name value s) state args in
                let (new_s, i, o) = eval env (fun_env_w_args, i, o) body in
                (State.drop_scope new_s s, i, o)
         
    (* Statement parser *)
    ostap (                                      
      line:
        "read" "(" x:IDENT ")"         {Read x}
        | "write" "(" e:!(Expr.parse) ")" {Write e}
        | x:IDENT ":=" e:!(Expr.parse)    {Assign (x, e)}
        | "if" e:!(Expr.parse) "then" thenStmt:parse "else" elseStmt:parse "fi" {If (e, thenStmt, elseStmt)}
        | "if" e:!(Expr.parse) "then" thenStmt:parse "fi" {If (e, thenStmt, Skip)}
        | "if" e:!(Expr.parse) "then" thenStmt:parse elifStmt:elif {If (e, thenStmt, elifStmt)}
        | "skip" {Skip}
        | "while" e:!(Expr.parse) "do" wStmt:parse "od" {While (e, wStmt)}
        | "repeat" ruStmt:parse "until" e:!(Expr.parse) {Repeat (ruStmt, e)}
        | "for" e1:parse "," e2:!(Expr.parse) "," e3:parse "do" s:parse "od" {Seq (e1, While (e2, Seq(s, e3)))}
        | name:IDENT "(" args:(!(Expr.parse))* ")" {Call (name, args)};
      parse:
        l:line ";" rest:parse {Seq (l, rest)} | line;
      elif:
        "elif" e:!(Expr.parse) "then" thenStmt:parse "else" elseStmt:parse "fi" {If (e, thenStmt, elseStmt)}
        | "elif" e:!(Expr.parse) "then" thenStmt:parse "fi" {If (e, thenStmt, Skip)}
        | "elif" e:!(Expr.parse) "then" thenStmt:parse elifStmt:elif {If (e, thenStmt, elifStmt)}
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (                                      
      parse: "fun" name:IDENT "(" args:(IDENT)* ")" local:(%"local" (IDENT)*)? "{" body:!(Stmt.parse) "}"
        {
            let local = match local with
            | Some x -> x
            | _ -> [] in
            name, (args, local, body)
        }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args (st, i, o, r) =                                                                      
           let xs, locs, s      =  snd @@ M.find f m in
           let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
           let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
           (State.leave st'' st, i', o', r')
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
