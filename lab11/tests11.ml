(*

  TESTS11. An (extended) OCaml interpreter for Pure Lisp.

    James Moen
    17 Nov 19

  35 points. There may be bugs. Beware!

*)

(* THING. A Lisp object. *)

type
  thing =
    Closure of thing * thing * environment ref |
    Cons of thing * thing |
    Nil |
    Number of int |
    Symbol of string
and
  environment = (string * thing) list ;;

(* CAN'T EVALUATE. Raise this if EVALUATE gets something bad. The string tells
   where the bad thing was detected. *)

exception Can'tEvaluate of string;;

(* TEE. NIL means FALSE. Anything else means TRUE, so TEE means TRUE too. *)

let tee = Symbol "t" ;;

(* GLOBAL. The global environment. *)

let global =
  ref
    [("nil", Nil) ;
     ("t",   tee)] ;;

(* EVALUATE PRODUCT. Return LEFT times RIGHT. *)

let evaluateProduct left right =
  match (left, right)
  with (Number left, Number right) -> Number (left * right) |
       _                           -> raise (Can'tEvaluate "*") ;;

(* EVALUATE SUM. Return LEFT plus RIGHT. *)

let evaluateSum left right =
  match (left, right)
  with (Number left, Number right) -> Number (left + right) |
       _                           -> raise (Can'tEvaluate "+") ;;

(* EVALUATE DIFFERENCE. Return LEFT minus RIGHT. *)

let evaluateDifference left right =
  match (left, right)
  with (Number left, Number right) -> Number (left - right) |
       _                           -> raise (Can'tEvaluate "-") ;;

(* EVALUATE QUOTIENT. Return LEFT divided by RIGHT. We can't divide by 0. *)

let evaluateQuotient left right =
  match (left, right)
  with (Number _, Number 0)        -> raise (Can'tEvaluate "/") |
       (Number left, Number right) -> Number (left / right) |
       _                           -> raise (Can'tEvaluate "/") ;;

(* EVALUATE LESS. Test if LEFT is less than RIGHT. *)

let evaluateLess left right =
  match (left, right)
  with (Number left, Number right) -> if left < right then tee else Nil |
       _                           -> raise (Can'tEvaluate "<") ;;

(* EVALUATE EQUAL. Test if an atom LEFT equals an atom RIGHT. *)

let evaluateEqual left right =
  match (left, right)
  with (Nil,         Nil         ) -> tee |
       (Number left, Number right) -> if left = right then tee else Nil |
       (Symbol left, Symbol right) -> if left = right then tee else Nil |
       _                           -> raise (Can'tEvaluate "eq") ;;

(* EVALUATE GREATER. Test if LEFT is greater than RIGHT. *)

let evaluateGreater left right =
  match (left, right)
  with (Number left, Number right) -> if left > right then tee else Nil |
       _                           -> raise (Can'tEvaluate ">") ;;

(* EVALUATE ATOM. Test if RIGHT is NIL, a NUMBER, or a SYMBOL. *)

let evaluateAtom right =
  match right
  with Nil      -> tee |
       Number _ -> tee |
       Symbol _ -> tee |
       _        -> Nil ;;

(* EVALUATE NUMBER. TEST if RIGHT is NUMBER *)

let evaluateNumber right =
  match right with
    Number x -> tee |
    _ -> Nil ;;

(* EVALUATE CAR. Return the first element of the list RIGHT. *)

let evaluateCar right =
  match right
  with Cons (left, _) -> left |
       _              -> raise (Can'tEvaluate "car") ;;

(* EVALUATE CDR. Return all but the first element of the list RIGHT. *)

let evaluateCdr right =
  match right
  with Cons (_, right) -> right |
       _               -> raise (Can'tEvaluate "cdr") ;;

(* EVALUATE CONS. Return a list whose first element is LEFT, and whose other
   elements are in the list RIGHT. *)

let evaluateCons left right =
  match right
  with Cons (_, _) -> Cons (left, right) |
       Nil         -> Cons (left, Nil) |
       _           -> raise (Can'tEvaluate "cons") ;;

(* EVALUATE DEFINE. Bind symbol LEFT to RIGHT in the GLOBAL environment. *)

let evaluateDefine left right =
  match left
  with Symbol name -> global := (name, right) :: ! global ; left |
       _           -> raise (Can'tEvaluate "define") ;;

(* EVALUATE LAMBDA. Return a closure for a function with PARAMETERS, BODY, and
   ENVIRONMENT. *)

let evaluateLambda parameters body environment =
  if environment == ! global
  then Closure (parameters, body, global)
  else Closure (parameters, body, ref environment) ;;

(* EVALUATE SYMBOL. Return the binding of string NAME in ENVIRONMENT. NAME is
   from a SYMBOL. *)

let evaluateSymbol name environment =

  let rec evaluatingSymbol environment =
    match environment
    with [] ->
           raise (Can'tEvaluate name) |

         (otherName, otherThing) :: otherEnvironment ->
           if name = otherName
           then otherThing
           else evaluatingSymbol otherEnvironment

  in evaluatingSymbol environment ;;

(* EVALUATE. Evaluate EXPRESSION in ENVIRONMENT. *)

let rec evaluate expression environment =

(* EVALUATING. Evaluate EXPRESSION. We dispatch to code that handles all these
   expressions:

   (∗ α β)           Return α times β.
   (+ α β)           Return α plus β.
   (− α β)           Return α minus β.
   (/ α β)           Return α divided by β.
   (< α β)           Test if α is less than β.
   (= α β)           Test if the atom α equals the atom β.
   (> α β)           Test if α is greater than β.
   (ATOM α)          Test if α is an atom.
   (DEFINE α β)      Define α to be β in the global environment.
   (CAR α)           Return the first element of the list α.
   (CDR α)           Return all but the first element of the list α.
   (CONS α β)        Return a list whose CAR is α and whose CDR is β.
   (IF α β γ)        If α = NIL then evaluate γ, otherwise evaluate β.
   (LAMBDA α β)      Return a function closure with parameters α and body β.
   (λ σ β)           A synonym for LAMBDA α β.
   (QUOTE α)         Return α without evaluating it.
   (α β₁ β₂ ... βⱼ)  Apply closure α to arguments β₁ β₂ ... βⱼ.

   We also handle NIL's, NUMBER's and SYMBOL's here.
*)

  let rec evaluating expression =
    match expression
    with Cons (Symbol "*", Cons (left, Cons (right, Nil))) ->
           evaluateProduct
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "+", Cons (left, Cons (right, Nil))) ->
           evaluateSum
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "-", Cons (left, Cons (right, Nil))) ->
           evaluateDifference
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "/", Cons (left, Cons (right, Nil))) ->
           evaluateQuotient
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "<", Cons (left, Cons (right, Nil))) ->
           evaluateLess
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "=", Cons (left, Cons(right, Nil))) ->
           evaluateEqual
             (evaluating left)
             (evaluating right) |

         Cons (Symbol ">", Cons (left, Cons(right, Nil))) ->
           evaluateGreater
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "atom", Cons (right, Nil)) ->
           evaluateAtom (evaluating right) |

         Cons (Symbol "number", Cons (right,Nil)) ->
           evaluateNumber (evaluating right) |

         Cons (Symbol "car", Cons (right, Nil)) ->
           evaluateCar
             (evaluating right) |

         Cons (Symbol "cdr", Cons (right, Nil)) ->
           evaluateCdr
             (evaluating right) |

         Cons (Symbol "cons", Cons (left, Cons (right, Nil))) ->
           evaluateCons
             (evaluating left)
             (evaluating right) |

         Cons(Symbol "define", Cons (left, Cons (right, Nil))) ->
           evaluateDefine
             left
             (evaluate right ! global) |

         Cons (Symbol "if", Cons (test, Cons (left, Cons (right, Nil)))) ->
           if evaluating test = Nil
           then evaluating right
           else evaluating left |

         Cons (Symbol "list", list1) ->
            let rec evaluateList list1  =
              match list1 with
                Cons (left,right) -> Cons (evaluating left, evaluateList right) |
                _ -> Nil
            in (evaluateList list1) |

         Cons(Symbol "let", expression) ->
             let rec evaluateLet environment expression =
               match expression with
                  Cons(left,Nil) -> evaluate left environment |
                  Cons(Symbol left, Cons(left1,right1)) -> evaluateLet ((left,evaluating left1) :: environment) (right1) |
                  _ -> raise (Can'tEvaluate "let")

             in evaluateLet environment expression |

         Cons (Symbol "lambda", Cons (parameters, Cons (body, Nil))) ->
           evaluateLambda
             parameters
             body
             environment |

         Cons (Symbol "λ", Cons (parameters, Cons (body, Nil))) ->
           evaluateLambda
             parameters
             body
             environment |

         Cons (Symbol "quote", Cons (thing, Nil)) ->
           thing |

         Cons (procedure, arguments) ->
           apply
             (evaluating procedure)
             arguments |

         Nil ->
           Nil |

         Number _ ->
           expression |

         Symbol string ->
           evaluateSymbol string environment |

         _ ->
           raise (Can'tEvaluate "evaluate")

(* APPLY. Apply CLOSURE to its ARGUMENTS. *)

  and apply closure arguments =
    match closure
    with Closure (parameters, body, environment) ->
           let rec applying environment parameters arguments =
             match (parameters, arguments)
             with (Nil, Nil) ->
                    evaluate body environment |

                  (Nil, Cons(_, _)) ->
                    raise (Can'tEvaluate "apply") |

                  (Cons(_, _), Nil) ->
                    raise (Can'tEvaluate "apply") |

                  (Cons (Symbol parameter, otherParameters),
                    Cons (argument, otherArguments)) ->
                      applying
                       ((parameter, evaluating argument) :: environment)
                       otherArguments
                       otherParameters |

                  _ -> raise (Can'tEvaluate "apply")

           in applying ! environment parameters arguments |

         _ ->
           raise (Can'tEvaluate "apply")

(* This is EVALUATE's body. *)

  in evaluating expression ;;

(* EVAL. Evaluate EXPRESSION in the GLOBAL environment. *)

let eval expression =
  evaluate expression ! global ;;

(* Tests. Each test shows the Pure Lisp expression to be evaluated, its horrid
   OCaml representation, what the result of the evaluation should be, and the
   number of points that the correct result is worth. *)

(* (number 0) *)
eval
 (Cons (Symbol "number", Cons (Number 0, Nil))) ;;
(* 2 pts.
   Symbol "t" *)

(* (number (quote a)) *)
eval
 (Cons (Symbol "number",
   Cons (Cons (Symbol "quote", Cons (Symbol "a", Nil)),
    Nil))) ;;
(* 3 pts.
   Nil *)

(* (number (+ 1 2)) *)
eval
 (Cons (Symbol "number",
   Cons (Cons (Symbol "+", Cons (Number 1, Cons (Number 2, Nil))),
    Nil))) ;;
(* 5 pts.
   Symbol "t" *)

(* (list) *)
eval
 (Cons (Symbol "list", Nil)) ;;
(* 2 pts.
   Nil *)

(* (list 1) *)
eval
 (Cons (Symbol "list", Cons (Number 1, Nil))) ;;
(* 3 pts.
   Cons (Number 1, Nil) *)

(* (list 1 2) *)
eval
 (Cons (Symbol "list", Cons (Number 1, Cons (Number 2, Nil)))) ;;
(* 5 pts.
   Cons (Number 1, Cons (Number 2, Nil)) *)

(* (list (quote a) (quote (b)) (+ 1 1)) *)
eval
 (Cons (Symbol "list",
   Cons (Cons (Symbol "quote", Cons (Symbol "a", Nil)),
    Cons (Cons (Symbol "quote", Cons (Cons (Symbol "b", Nil), Nil)),
     Cons (Cons (Symbol "+", Cons (Number 1, Cons (Number 1, Nil))), Nil)))))
     ;;
(* 5 pts.
   Cons (Symbol "a", Cons (Cons (Symbol "b", Nil), Cons (Number 2, Nil))) *)

(* (define a 1) *)
eval
 (Cons (Symbol "define",
   Cons (Symbol "a",
    Cons (Number 1, Nil)))) ;;
(* Symbol "a" *)

(* (define b 2) *)
eval
 (Cons (Symbol "define",
   Cons (Symbol "b",
    Cons (Number 2, Nil)))) ;;
(* Symbol "b" *)

(* (let
     a (+ a 3)
     b (+ a 1)
     c 3
     (+ a (+ b c))) *)
eval
 (Cons (Symbol "let",
   Cons (Symbol "a",
    Cons (Cons (Symbol "+", Cons (Symbol "a", Cons (Number 3, Nil))),
     Cons (Symbol "b",
      Cons (Cons (Symbol "+", Cons (Symbol "a", Cons (Number 1, Nil))),
       Cons (Symbol "c",
        Cons (Number 3,
         Cons
          (Cons (Symbol "+",
            Cons (Symbol "a",
             Cons
              (Cons (Symbol "+", Cons (Symbol "b", Cons (Symbol "c", Nil))),
              Nil))),
          Nil))))))))) ;;
(* 10 pts.
   Number 9 *)

(* a *)
eval
 (Symbol "a") ;;
(* 3 pts.
   Number 1 *)

(* b *)
eval
 (Symbol "b") ;;
(* 2 pts.
   Number 2 *)
