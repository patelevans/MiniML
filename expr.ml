(* 
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | F_Negate
;;
    
type binop =
  | Plus
  | F_Plus
  | Minus
  | F_Minus
  | Times
  | F_Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal ;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> SS.empty
  | Unop (_, exp1) -> free_vars exp1
  | Binop (_, exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  | Conditional (if_exp, then_exp, else_exp) ->
     SS.union (free_vars if_exp)
              (SS.union (free_vars then_exp) (free_vars else_exp))
  | Fun (v_name, exp1) -> SS.remove v_name (free_vars exp1)
  | Let (v_name, def_exp, in_exp) ->
     SS.union (free_vars def_exp) (SS.remove v_name (free_vars in_exp))
  | Letrec (v_name, def_exp, in_exp) ->
     SS.remove v_name (SS.union (free_vars def_exp) (free_vars in_exp))
  | App (exp1, exp2) ->
     SS.union (free_vars exp1) (free_vars exp2) ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname () : varid =
  let gensym =
    let c = ref 0 in
    fun (name : string) ->
      let new_var_name = name ^ string_of_int (!c) in
      c := succ !c;
      new_var_name in
  gensym "newvar" ;;

(*......................................................................
  Substitution
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  match exp with
  | Var v -> if var_name = v then repl else exp
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> exp
  | Unop (op, exp1) -> Unop (op, subst var_name repl exp1)
  | Binop (op, exp1, exp2) ->
     Binop (op, subst var_name repl exp1, subst var_name repl exp2)
  | Conditional (if_exp, then_exp, else_exp) ->
     Conditional (subst var_name repl if_exp, subst var_name repl then_exp,
                  subst var_name repl else_exp)
  | Fun (v_name, exp1) ->
     if v_name = var_name then exp 
     else if SS.mem v_name (free_vars repl)
     then let n_name = new_varname () in 
         Fun(n_name, subst var_name repl (subst v_name (Var (n_name)) exp1))
     else Fun(v_name, subst var_name repl exp1) 
  | Let (v_name, def_exp, in_exp) ->
     if v_name = var_name 
     then Let (v_name, subst var_name repl def_exp, in_exp)
     else if SS.mem v_name (free_vars repl)
     then let n_name = new_varname () in 
           Let(n_name, subst var_name repl def_exp, 
               subst var_name repl (subst v_name (Var (n_name)) in_exp))
     else Let (v_name, subst var_name repl def_exp, 
                   subst var_name repl in_exp)
  | Letrec (v_name, def_exp, in_exp) ->
     if v_name = var_name then exp
     else if SS.mem v_name (free_vars repl)
     then let n_name = new_varname () in 
       Letrec (n_name, subst var_name repl def_exp, 
               subst var_name repl (subst v_name (Var (n_name)) in_exp))
     else Letrec (v_name, subst var_name repl def_exp, 
                  subst var_name repl in_exp)
  | App (exp1, exp2) ->
     App (subst var_name repl exp1, subst var_name repl exp2) ;;
     
(*......................................................................
  String representations of expressions
 *)

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  let unop_to_concrete (op : unop) : string =
    match op with
    | Negate -> "-"
    | F_Negate -> "-." in

  let binop_to_concrete (op : binop) : string =
    match op with
    | Plus -> "+"
    | F_Plus -> "+."
    | Minus -> "-"
    | F_Minus -> "-."
    | Times -> "*"
    | F_Times -> "*."
    | Equals -> "="
    | LessThan -> "<" in

  match exp with
  | Var v -> v
  | Num i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Unop (op, exp1) -> (unop_to_concrete op) ^ (exp_to_concrete_string exp1)
  | Binop (op, exp1, exp2) ->
     (exp_to_concrete_string exp1) ^ " " ^
     (binop_to_concrete op) ^ " " ^
     (exp_to_concrete_string exp2)
  | Conditional (if_exp, then_exp, else_exp) ->
     "if " ^ (exp_to_concrete_string if_exp) ^ " then " ^
     (exp_to_concrete_string then_exp) ^ " else " ^
     (exp_to_concrete_string else_exp)
  | Fun (v_name, exp1) ->
     "fun " ^ v_name ^ " -> " ^ (exp_to_concrete_string exp1)
  | Let (v_name, def_exp, in_exp) ->
     "let " ^ v_name ^ " = " ^ (exp_to_concrete_string def_exp) ^
     " in " ^ (exp_to_concrete_string in_exp)
  | Letrec (v_name, def_exp, in_exp) ->
     "let rec " ^ v_name ^ " = " ^ (exp_to_concrete_string def_exp) ^
     " in " ^ (exp_to_concrete_string in_exp)
  | Raise -> "raise Exception"
  | Unassigned -> "Unassigned"
  | App (exp1, exp2) ->
     (exp_to_concrete_string exp1) ^ " " ^ (exp_to_concrete_string exp2) ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  let unop_to_abstract (op : unop) : string =
    match op with
    | Negate -> "Negate"
    | F_Negate -> "F_Negate" in

  let binop_to_abstract (op : binop) : string =
    match op with
    | Plus -> "Plus"
    | F_Plus -> "F_Plus"
    | Minus -> "Minus"
    | F_Minus -> "F_Minus"
    | Times -> "Times"
    | F_Times -> "F_Times"
    | Equals -> "Equals"
    | LessThan -> "LessThan" in
  
  match exp with
  | Var v -> "Var (" ^ v ^ ")"
  | Num i -> "Num (" ^ (string_of_int i) ^ ")"
  | Float f -> "Float (" ^ (string_of_float f) ^ ")"
  | Bool b -> "Bool (" ^ (string_of_bool b) ^ ")"
  | Unop (op, exp1) ->
     "Unop (" ^ (unop_to_abstract op) ^ ", "
     ^ (exp_to_abstract_string exp1) ^ ")"
  | Binop (op, exp1, exp2) ->
     "Binop (" ^ (binop_to_abstract op) ^ ", " ^
     (exp_to_abstract_string exp1) ^ ", " ^
     (exp_to_abstract_string exp2) ^ ")"
  | Conditional (if_exp, then_exp, else_exp) ->
     "Conditional (" ^ (exp_to_abstract_string if_exp) ^ ", " ^
     (exp_to_abstract_string then_exp) ^ ", " ^
     (exp_to_abstract_string else_exp) ^ ")"
  | Fun (v_name, exp1) ->
     "Fun (" ^ v_name ^ ", " ^ (exp_to_abstract_string exp1) ^ ")"
  | Let (v_name, def_exp, in_exp) ->
     "Let (" ^ v_name ^ ", " ^
     (exp_to_abstract_string def_exp) ^ ", " ^
     (exp_to_abstract_string in_exp) ^ ")"
  | Letrec (v_name, def_exp, in_exp) ->
     "Letrec (" ^ v_name ^ ", " ^
     (exp_to_abstract_string def_exp) ^ ", " ^
     (exp_to_abstract_string in_exp) ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (exp1, exp2) ->
     "App (" ^ (exp_to_abstract_string exp1) ^ ", "
     ^ (exp_to_abstract_string exp2) ^ ")" ;;
