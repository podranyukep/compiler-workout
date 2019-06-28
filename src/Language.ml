(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y
    
    (* Some helping code for further work:
    boolToInt converts boolean b to integer
    intToBool converts integer i to boolean
    *)  
    let boolToInt b = if b then 1 else 0
    let intToBool i = i != 0


    (* Possible operations *)
    let operation oper leftExpr rightExpr = match oper with
        |"!!" -> boolToInt (( || ) (intToBool leftExpr) (intToBool rightExpr))
        |"&&" -> boolToInt (( && ) (intToBool leftExpr) (intToBool rightExpr))
        |"==" -> boolToInt (( == ) leftExpr rightExpr)
        |"!=" -> boolToInt (( != ) leftExpr rightExpr)
        |"<=" -> boolToInt (( <= ) leftExpr rightExpr)
        |"<" -> boolToInt (( <  ) leftExpr rightExpr)
        |">=" -> boolToInt (( >= ) leftExpr rightExpr)
        |">" -> boolToInt (( >  ) leftExpr rightExpr)
        |"+" -> ( +  ) leftExpr rightExpr
        |"-" -> ( -  ) leftExpr rightExpr
        |"*" -> ( *  ) leftExpr rightExpr
        |"/" -> ( /  ) leftExpr rightExpr
        |"%" -> ( mod ) leftExpr rightExpr

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let rec eval state expr = match expr with
    |Const cName -> cName
    |Var varName -> state varName
    |Binop (oper, leftExpr, rightExpr) -> 
        operation oper (eval state leftExpr) (eval state rightExpr)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    let do_Bin oper =  ostap(- $(oper)), (fun x y -> Binop (oper, x, y))

    ostap (
          expr:
        !(Ostap.Util.expr
            (fun x -> x)
            (Array.map (fun (a, ops) -> a, List.map do_Bin ops)
                [|
                    `Lefta, ["!!"];
                    `Lefta, ["&&"];
                    `Nona , ["=="; "!="; "<="; ">="; "<"; ">"];
                    `Lefta, ["+"; "-"];
                    `Lefta, ["*"; "/"; "%"];
                |]
            )
            primary
            );
        primary: x:IDENT {Var x} | c:DECIMAL {Const c} | -"(" expr -")"
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
    (* loop with a post-condition       *) | RepeatUntil    of t * Expr.t
    (* foreach                          *) | ForEach    of Expr.t * Expr.t * Expr.t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval conf stmt: config = 
        let (s, i, o) = conf in
        match stmt with
            | Read x -> (match i with
                | z :: i_rest -> (Expr.update x z s, i_rest, o)
                | _           -> failwith "Input read fail")
            | Write   e             -> (s, i, o @ [Expr.eval s e])
            | Assign (x, e)         -> (Expr.update x (Expr.eval s e) s, i, o)
            | Seq    (s1, s2) -> eval (eval conf s1) s2
            | Skip                              -> (s, i, o)
            | If (e, thenStmt, elseStmt)        -> eval (s, i, o) (if Expr.intToBool (Expr.eval s e) then thenStmt else elseStmt)
            | While (e, wStmt)                  -> if Expr.intToBool (Expr.eval s e) then eval (eval (s, i, o) wStmt) stmt else (s, i, o)
            | ForEach (x, e1, e2, b)            -> if Expr.eval s e1 <= Expr.eval s e2 then 
			(*Expr.update x (Expr.eval s e1) s, i, o*)
			eval (eval (s, i, o) b) stmt else (s, i, o)
            | RepeatUntil (ruStmt, e)           -> let (sNew, iNew, oNew) = eval (s, i, o) ruStmt in
            if not (Expr.intToBool (Expr.eval sNew e)) then eval (sNew, iNew, oNew) stmt else (sNew, iNew, oNew);;
                               
    (* Statement parser *)
    ostap (
      simple:
        "read" "(" x:IDENT ")"         {Read x}
        | "write" "(" e:!(Expr.expr) ")" {Write e}
        | x:IDENT ":=" e:!(Expr.expr)    {Assign (x, e)};
      ifStmt:
        "if" e:!(Expr.expr) "then" thenBody:parse
      elifBranches: (%"elif" elifE:!(Expr.expr) %"then" elifBody:!(parse))*
      elseBranch: (%"else" elseBody:!(parse))?
        "fi" {
            let elseBranch' = match elseBranch with
                | Some x -> x
                | None   -> Skip in
                    let expandedElseBody = List.fold_right (fun (e', body') else' -> If (e', body', else')) elifBranches elseBranch' in
                    If (e, thenBody, expandedElseBody)
             };
      whileStmt:
        "while" e:!(Expr.expr) "do" body:parse "od" {While (e, body)};
      forStmt:
        "for" initStmt:stmt "," whileCond:!(Expr.expr) "," forStmt:stmt
        "do" body:parse "od" {

			Seq (initStmt, While (whileCond, Seq (body, forStmt)))
		};
      repeatUntilStmt:
        "repeat" body:parse "until" e:!(Expr.expr) {RepeatUntil (body, e)};
      foreach:
        "foreach" x:!(Expr.expr) "in [" e1:!(Expr.expr) "..." e2:!(Expr.expr) "]" 
            "do" body:parse "od" {
                ForEach(x, e1, e2, body);
            };
      control:
        ifStmt
        | whileStmt
        | forStmt
        | repeatUntilStmt
		| foreach
        | "skip" {Skip};
      stmt:
        simple
        | control;
      parse:
        stmt1:stmt ";" rest:parse {Seq (stmt1, rest)}
        | stmt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
