#use "read.ml" ;;
#use "CS17setup.ml" ;;
(* these type definitions are in comments
 * because they've already been defined in read.ml
type raw_program = string

type concrete_program_piece =
  | Number of int
  | Symbol of string
  | List of concrete_program_piece list

type concrete_program = concrete_program_piece list
*)

type identifier = ID of string ;;

(* a Rackette expression *)
type expression =
| Num of int
| Bool of bool
| Empty
| Ident of identifier
| And of expression * expression
| Or of expression * expression
| If of expression * expression * expression
| Cond of (expression * expression) list
| Lambda of identifier list * expression
| Let of (identifier * expression) list * expression
| Application of expression list ;;

(* a Rackette definition *)
type definition = identifier * expression ;;

(* a piece of Rackette that can be processed:
 * either a definition or an expression *)
type abstract_program_piece =
| Definition of definition
| Expression of expression ;;

(* a representation of a Rackette program --
 * any number of pieces *)
type abstract_program = abstract_program_piece list ;;

(* a Rackette value: the result of evaluating a Rackette expression *)
type value =()
| VNum of int
| VBool of bool
| VList of value list
| VBuiltin of string * (value list -> value)
| VClosure of identifier list * expression * environment
and binding = identifier * value
and environment = binding list ;;

(* TODO: fill this with your initial top level environment,
 * consisting of built-in procedures like + or empty? *)
let initial_tle : environment = [] 



;;

(* TODO: write the design recipe for and implement parse_expression *)

let rec parse_expression (input : concrete_program_piece) : expression = match input with
   | Num(x) -> Num x 
   | Symbol(m) -> (match m with 
                  | "true" ->  Bool true
                  | "false" ->  Bool false 
                  | ___ -> Ident m)
   | [] -> Empty
   | List some1 -> match some with
     | Symbol "and" :: a :: b -> And ((parse_expression a), (parse_expression b))
     | Symbol "or" :: a :: b -> Or ((parse_expression a), (parse_expression b))
     | Symbol "if" :: a :: b :: c -> If ((parse_expression a), (parse_expression b), (parse_expression c))
     | (Symbol "cond")::tlcond -> parse_cond ((Symbol "cond")::tlcond)
     | (Symbol "lambda")::tllam -> parse_lambda ((Symbol "lambda")::tllam)
     | (Symbol "let")::tllet -> parse_let ((Symbol "let")::tllet);;
     | hd::tl -> (parse_expression hd)::(parse_expression (List tl))
     | [] -> []
     | List some2 -> parse_expression some2

let parse_lambda (input : concrete_program_piece) : expression = match input with 
   | (Symbol "lambda")::x::y::[] -> Lambda ([parse_expression x],  
   | ([]) -> []
   | (List (a::b::[])::tl) -> ((parse_expression a, parse_expression b))::(parse_cond tl);;

let parse_let (input : concrete_program_piece) : expression = match input with 
   | (Symbol "cond")::tl -> Let (parse_let tl) 
   | ([]) -> []
   | (List (a::b::[])::tl) -> ((parse_expression a, parse_expression b))::(parse_cond tl);;

let parse_cond (input : concrete_program_piece) : expression = match input with 
   | (Symbol "cond")::tl -> Cond (parse_cond tl) 
   | ([]) -> []
   | (List (a::b::[])::tl) -> ((parse_expression a, parse_expression b))::(parse_cond tl);;



(* parse_definition *)

(* Input : a concrete_program_piece, input *)
(* Output : If the input is a list of three concrete program pieces, the Symbol
            "define", a second Symbol, and a concree program piece, the output 
            is a definition whith the second symbol and the parsed concrete 
            program piece *)

let parse_definition (input : concrete_program_piece) : definition =
match input with 
| List ((Symbol "define")::(Symbol x)::tl) -> 
  ((Ident (ID x)), (parse_expression tl))
| List ((Symbol "define")::_::[]) | List ((Symbol "define")::[]) -> 
  "A definition must contain 3 parts"
| List ((Symbol "define")::_::_) -> 
   failwith "Define must be followed by valid arguement" ;;

(* Test Cases *)
check_expect (parse_definition (List [Symbol "define"; Symbol x; Symbol y])) 
  ((Ident (ID x)), ));;
check_expect (parse_definition (List [Symbol "define"; Symbol x; Number 3]))
  ((Ident (ID x)), Num 3);;



let parse_piece (input : concrete_program_piece) : abstract_program_piece =
  match input with
  | List (Symbol "define" :: _) -> Definition (parse_definition input)
  | _ -> Expression (parse_expression input) ;;





(* TODO: write the design recipe for parse *)
let parse (input : concrete_program) : abstract_program =
  (* this will parse all of the pieces of this program,
   * giving us a list of pieces, our abstract syntax *)
  List.map parse_piece input ;;





(* TODO: write the design recipe for and implement eval *)
let eval (tle : environment) (env : environment) (expr : expression) : value =
  (* NOTE: tle is the top level environment and env is the local environment *)
  failwith "eval is not yet implemented" ;;



(* TODO: write the design recipe for and implement add_definition *)
let add_definition (env : environment) (id, expr : definition) : environment =
  if List.mem (id, _) env 
  then failwith "This ID is already in use in the top level invironment"
  else  (id,exp)::env ;;

(* TODO: write the design recipe for and implement string_of_value *)
let string_of_value (a_value : value) : string =
  failwith "string_of_value is not yet implemented" ;;

(* process: this procedure processes the abstract_syntax
 * representation of a Rackette program following the
 * Rackette rules of processing
 * I/P: an abstract_syntax representation of a Rackette program
 * O/P: the list of values corresponding to
 * the evauation of any expressions present in pieces *)
let process (pieces : abstract_program) : value list =
  let rec process_helper (tle : environment) (pieces : abstract_program) : value list =
  match pieces with
  | [] -> []
  | (Definition def) :: tl -> process_helper (add_definition tle def) tl
  | (Expression expr) :: tl -> (eval tle [] expr) :: (process_helper tle tl)
in process_helper initial_tle pieces ;;

(* TODO: write test cases for process *)

(* rackette: this procedure will interpret a Rackette program
 * and return its value as a string, if it has one
 * I/P: a Rackette program represented as concrete_syntax, program
 * O/P: the string representation of
 *      the evaluated Rackette expressions in programs *)
let rackette (input : raw_program) : string list =
  List.map string_of_value (process (parse (read_all input))) ;;

(* TODO: write test cases for rackette *)
