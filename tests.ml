open Expr ;;
open Evaluation ;;

open CS51Utils ;; 
open Absbook ;;

let free_vars_tests () =
  unit_test (free_vars (Num (1)) = vars_of_list []) "free_vars Num";
  unit_test (free_vars (Bool (true)) = vars_of_list []) "free_vars Bool";
  unit_test (free_vars Raise = vars_of_list []) "free_vars Raise";
  unit_test (free_vars Unassigned = vars_of_list []) "free_vars Unassigned";
  unit_test (free_vars (Binop (Plus, Num (1), Var ("x"))) = vars_of_list ["x"])
            "free_vars 1 + x";
  unit_test (free_vars (Let("a", Fun ("b", Var ("a")), Var ("a")))
             = vars_of_list ["a"])
            "free_vars let a = fun b -> a in a ;;";
  unit_test (free_vars (Let ("a", Num (1),
                             Let ("b", Var ("a"),
                                  App (App (Var ("f"), Var ("a")),
                                           Var ("b")))))
             = vars_of_list ["f"])
            "free_vars let a = 1 in let b = a in f a b";
  unit_test (same_vars (free_vars (Conditional (Var ("a"),
                                                Var ("b"), Var ("c"))))
                       (vars_of_list ["a"; "b"; "c"]))
            "free_vars if x then y else z";
  unit_test (same_vars (free_vars
                         (Letrec ("f",
                                  Binop (Plus, Num (1),
                                         App (Var ("f"),
                                              Var ("x"))),
                                  App (Var ("f"), Num (2)))))
                       (vars_of_list ["x"]))
            "free_vars let rec f = 1 + f x in f 2" ;;

let subst_tests () =
  unit_test (subst "x" (Num (1)) (Num (100)) = Num (100)) "subst Num";
  unit_test (subst "x" (Num (1)) (Var ("x")) = Num (1)) "subst Var";
  unit_test (subst "x" (Num (1)) (Var ("y")) = Var ("y")) "subst wrong Var";
  unit_test (subst "x" (Num (1)) (Unop(Negate, Var "x"))
             = Unop(Negate, Num (1)))
            "subst Unop";
  unit_test (subst "x" (Num (1)) (Binop (Minus, Var ("y"), Var ("x")))
             = Binop (Minus, Var ("y"), Num (1)))
            "subst Binop";
  unit_test (subst "x" (Num (3)) (Let ("y", Binop (Times, Num (1), Var ("x")),
        	                             Binop (Minus, Var ("x"), Var ("y"))))
             = Let ("y", Binop (Times, Num (1), Num (3)),
                    Binop (Minus, Num (3), Var "y")))
            "subst Let";
  unit_test (subst "x" (Num 1) (Fun ("y", Binop (Times, Var ("x"), Num (2))))
             = Fun ("y", Binop (Times, Num (1), Num (2))))
            "subst Fun"; ;;

let envir = Env.empty () ;;

(* 1 ;; *)
let exp1 = Num (1) ;;
(* let x = 7 in x ;; *)
let exp2 = Let ("x", Num (7), Var ("x")) ;;
(* let a = 3 in let b = 2 in a ;; *)
let exp3 = Let ("a", Num (3), Let ("b", Num (2), Var ("a"))) ;;
(* let x = 13 in let x = x - 3 in x ;; *)
let exp4 = Let ("x", Num (13),
                Let ("x", Binop (Minus, Var ("x"), Num (3)), Var ("x"))) ;;
(* let f = fun x -> x in f 35 ;; *)
let exp5 = Let ("f",
                Fun("x", Var ("x")), App (Var ("f"), Num (35))) ;;
(* let f = fun x -> 2 * x in f (f 2) ;; *)
let exp6 = Let ("f",
                Fun ("x",
                     Binop (Times, Num (2), Var ("x"))),
                App (Var ("f"), App (Var ("f"), Num (2)))) ;;
(* let f = fun x -> if x < 5 then 10 else 0 in f 3 ;; *)
let exp7 = Let ("f",
                Fun ("x",
                     Conditional (Binop (LessThan,
                                         Var ("x"),
                                         Num (5)), Num (10), Num (0))),
                App (Var ("f"), Num (3))) ;;
(* let f = fun x -> if x < 5 then 10 else 0 in f 8 ;; *)
let exp8 = Let ("f",
                Fun ("x",
                     Conditional (Binop (LessThan,
                                         Var ("x"),
                                         Num (5)), Num (10), Num (0))),
                App (Var ("f"), Num (8))) ;;
(* let f = fun x -> ~-x in f 3 ;; *)
let exp9 = Let ("f",
                Fun ("x", Unop (Negate, Var ("x"))),
                App (Var ("f"), Num (3))) ;;
(* if true then true else false ;; *)
let exp10 = Conditional (Bool (true), Bool (true), Bool (false)) ;;
(* if false then true else false ;; *)
let exp11 = Conditional (Bool (false), Bool (true), Bool (false)) ;;
(* let rec f = fun x -> if x = 0 then 0 else x + f(x - 1) in f 3 ;; *)
let exp12 = Letrec ("f",
                    Fun ("x",
                         Conditional (Binop (Equals,
                                             Var ("x"),
                                             Num (0)),
                                      Num (0),
                                      Binop (Plus,
                                             Var ("x"),
                                             App (Var ("f"),
                                                  Binop (Minus,
                                                         Var ("x"),
                                                         Num (1)))))),
                    App (Var ("f"), Num (3))) ;;
(* let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;; *)
let exp13 = Let ("x",
                 Num (1),
                 Let ("f",
                      Fun ("y",
                           Binop (Plus, Var ("x"), Var ("y"))),
                      Let ("x", Num (2), App (Var ("f"), Num (3))))) ;;
(* 3. ;; *)
let exp14 = Float (3.) ;;
(* ~-. 3. ;; *)
let exp15 = Unop (F_Negate, Float (3.)) ;;
(* 3. +. 4. ;; *)
let exp16 = Binop (F_Plus, Float (3.), Float (4.)) ;;
(* 3. = 4. ;; *)
let exp17 = Binop (Equals, Float (3.), Float (4.)) ;;
(* 3. -. 4. ;; *)
let exp18 = Binop (F_Minus, Float (3.), Float (4.)) ;;
(* 3. *. 5. ;; *)
let exp19 = Binop (F_Times, Float (3.), Float (5.)) ;;
(* 3. < 4. *)
let exp20 = Binop (LessThan, Float (3.), Float (4.)) ;;

let eval_s_tests () =
  unit_test (eval_s exp1 envir = Val (Num (1))) "eval_s 1 ;;";
  unit_test (eval_s exp2 envir = Val (Num (7))) "eval_s let x = 7 in x ;;";
  unit_test (eval_s exp3 envir = Val (Num (3)))
            "eval_s let a = 3 in let b = 2 in a ;;";
  unit_test (eval_s exp4 envir = Val (Num (10)))
            "eval_s let x = 13 in let x = x - 3 in x ;;";
  unit_test (eval_s exp5 envir = Val (Num (35)))
            "eval_s let f = fun x -> x in f 35 ;;";
  unit_test (eval_s exp6 envir = Val (Num (8)))
            "eval_s let f = fun x -> 2 * x in f (f 2) ;;";
  unit_test (eval_s exp7 envir = Val (Num (10)))
            "eval_s let f = fun x -> if x < 5 then 10 else 0 in f 3 ;;";
  unit_test (eval_s exp8 envir = Val (Num (0)))
            "eval_s let f = fun x -> if x < 5 then 10 else 0 in f 8 ;;";
  unit_test (eval_s exp9 envir = Val (Num (-3)))
            "eval_s let f = fun x -> ~-x in f 3 ;;";
  unit_test (eval_s exp10 envir = Val (Bool (true)))
            "eval_s if true then true else false ;;";
  unit_test (eval_s exp11 envir = Val (Bool (false)))
            "eval_s if false then true else false ;;";
  unit_test (eval_s exp12 envir = Val (Num (6)))
            "eval_s let rec f = fun x -> if x = 0 then 0 else x + f(x - 1) in f 3 ;;";
  unit_test (eval_s exp13 envir = Val (Num (4)))
            "eval_s let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;";
  unit_test (eval_s exp14 envir = Val (Float (3.))) "eval_s 3. ;;";
  unit_test (eval_s exp15 envir = Val (Float (-3.))) "eval_s ~-. 3. ;;";
  unit_test (eval_s exp16 envir = Val (Float (7.))) "eval_s 3. +. 4. ;;";
  unit_test (eval_s exp17 envir = Val (Bool (false))) "eval_s 3. = 4. ;;";
  unit_test (eval_s exp18 envir = Val (Float (-1.))) "eval_s 3. -. 4. ;;";
  unit_test (eval_s exp19 envir = Val (Float (15.))) "eval_s 3. *. 5. ;;";
  unit_test (eval_s exp20 envir = Val (Bool (true))) "eval_s 3. < 4. ;;" ;;

let eval_d_tests () =
  unit_test (eval_d exp1 envir = Val (Num (1))) "eval_d 1 ;;";
  unit_test (eval_d exp2 envir = Val (Num (7))) "eval_d let x = 7 in x ;;";
  unit_test (eval_d exp3 envir = Val (Num (3)))
            "eval_d let a = 3 in let b = 2 in a ;;";
  unit_test (eval_d exp4 envir = Val (Num (10)))
            "eval_d let x = 13 in let x = x - 3 in x ;;";
  unit_test (eval_d exp5 envir = Val (Num (35)))
            "eval_d let f = fun x -> x in f 35 ;;";
  unit_test (eval_d exp6 envir = Val (Num (8)))
            "eval_d let f = fun x -> 2 * x in f (f 2) ;;";
  unit_test (eval_d exp7 envir = Val (Num (10)))
            "eval_d let f = fun x -> if x < 5 then 10 else 0 in f 3 ;;";
  unit_test (eval_d exp8 envir = Val (Num (0)))
            "eval_d let f = fun x -> if x < 5 then 10 else 0 in f 8 ;;";
  unit_test (eval_d exp9 envir = Val (Num (-3)))
            "eval_d let f = fun x -> ~-x in f 3 ;;";
  unit_test (eval_d exp10 envir = Val (Bool (true)))
            "eval_d if true then true else false ;;";
  unit_test (eval_d exp11 envir = Val (Bool (false)))
            "eval_d if false then true else false ;;";
  unit_test (eval_d exp12 envir = Val (Num (6)))
            "eval_d let rec f = fun x -> if x = 0 then 0 else x + f(x - 1) in f 3 ;;";
  unit_test (eval_d exp13 envir = Val (Num (5)))
            "eval_d let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;";
  unit_test (eval_d exp14 envir = Val (Float (3.))) "eval_d 3. ;;";
  unit_test (eval_d exp15 envir = Val (Float (-3.))) "eval_d ~-. 3. ;;";
  unit_test (eval_d exp16 envir = Val (Float (7.))) "eval_d 3. +. 4. ;;";
  unit_test (eval_d exp17 envir = Val (Bool (false))) "eval_d 3. = 4. ;;";
  unit_test (eval_d exp18 envir = Val (Float (-1.))) "eval_d 3. -. 4. ;;";
  unit_test (eval_d exp19 envir = Val (Float (15.))) "eval_d 3. *. 5. ;;";
  unit_test (eval_d exp20 envir = Val (Bool (true))) "eval_d 3. < 4. ;;" ;;

let tests () =
  free_vars_tests ();
  subst_tests ();
  eval_s_tests ();
  eval_d_tests () ;;

let _ = tests () ;;