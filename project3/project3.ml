open Printf ;;

type token =
  CloseParenToken |
  EndToken |
  NumberToken of int |
  OpenParenToken |
  SymbolToken of string ;;

type thing =
    Closure of thing * thing * environment ref |
    Cons of thing * thing |
    Nil |
    Number of int |
    Symbol of string
and
  environment = (string * thing) list ;;

exception BadString;;

(* CAN'T EVALUATE. Raise this if EVALUATE gets something bad. The string tells
   where the bad thing was detected. *)

exception Can'tEvaluate of string;;

(* CAN'T PARSE. Raised if the parser fails. *)

exception Can'tParse of string ;;

(* TEE. NIL means FALSE. Anything else means TRUE, so TEE means TRUE too. *)

let tee = Symbol "t" ;;

(* GLOBAL. The global environment. It's a variable so we can define functions
   in arbitrary order. *)

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
       _                           -> Nil ;;

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

   (∗ α β)              Return α times β.
   (+ α β)              Return α plus β.
   (− α β)              Return α minus β.
   (/ α β)              Return α divided by β.
   (< α β)              Test if α is less than β.
   (= α β)              Test if the atom α equals the atom β.
   (> α β)              Test if α is greater than β.
   (ATOM α)             Test if α is an atom.
   (DEFINE α β)         Define α to be β in the global environment.
   (CAR α)              Return the first element of the list α.
   (CDR α)              Return all but the first element of the list α.
   (CONS α β)           Return a list whose CAR is α and whose CDR is β.
   (IF α β γ)           If α = NIL then evaluate γ, otherwise evaluate β.
   (LAMBDA α β)         Return a function closure with parameters α and body β.
   (LIST α₁ α₂ ... αⱼ)  Return a list whose elements are α₁, α₂ ..., αⱼ.
   (λ σ β)              A synonym for LAMBDA α β.
   (QUOTE α)            Return α without evaluating it.
   (α β₁ β₂ ... βⱼ)     Apply closure α to arguments β₁, β₂ ..., βⱼ.

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

         Cons (Symbol "list", rights) ->
           let rec evaluateList rights =
             match rights
             with Nil ->
                    Nil |
                  Cons (first, rest) ->
                    Cons (evaluating first, evaluateList rest) |
                  _ ->
                    raise (Can'tEvaluate "list")
           in evaluateList rights |

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
                       otherParameters
                       otherArguments |

                  _ ->
                    raise (Can'tEvaluate "apply")

           in applying ! environment parameters arguments |

         _ ->
           raise (Can'tEvaluate "apply")

(* This is EVALUATE's body. *)

  in evaluating expression ;;

(* EVAL. Evaluate EXPRESSION in the GLOBAL environment. *)

let eval expression =
  evaluate expression ! global ;;

(* MAKE SCANNER. Return a function NEXT TOKEN that reads TOKENs from a file
   with pathname PATH. OCaml RAISEs an exception if there's no such file. *)

let makeScanner path =

(* INPUT. Read chars from this channel. *)

  let input = (open_in path)
  in

(* CH. The char most recently read from INPUT. *)

  let ch = ref ' '
  in

(* NEXT CHAR. Advance CH to the next char from INPUT, or to '\000' if we're at
   the end of INPUT. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file -> ch := '\000'
  in

  let nextEndToken () =
    EndToken
  in

  let nextOpenParenToken () =
    nextChar ();
    OpenParenToken
  in

  let nextCloseParenToken () =
    nextChar ();
    CloseParenToken
  in

  let rec nextNumberToken prefix =
    match !ch with
      '\000' -> let n = int_of_string prefix
                  in NumberToken n |
      '\n'   -> let n = int_of_string prefix
                  in NumberToken n |
      ' '    -> let n = int_of_string prefix
                  in NumberToken n |
      '('    -> let n = int_of_string prefix
                  in NumberToken n |
      ')'    -> let n = int_of_string prefix
                  in NumberToken n |
      _      -> let temp_string = Char.escaped !ch
                  in let temp_prefix = prefix ^ temp_string
                    in nextChar ();
                       nextNumberToken temp_prefix
   in

   let rec nextSymbolToken prefix =
     match !ch with
       '\000' -> let s = prefix
                   in SymbolToken s |
       '\n'   -> let s = prefix
                   in SymbolToken s |
       ' '    -> let s = prefix
                   in SymbolToken s |
       '('    -> let s = prefix
                   in SymbolToken s |
       ')'    -> let s = prefix
                   in SymbolToken s |
        _     -> let temp_string = Char.escaped !ch
                   in let temp_prefix = prefix ^ temp_string
                     in nextChar ();
                        nextSymbolToken temp_prefix
    in

    let nextNumberOrSymbolToken () =
      nextChar();
      match !ch with
      '0' -> nextNumberToken "-" |
      '1' -> nextNumberToken "-" |
      '2' -> nextNumberToken "-" |
      '3' -> nextNumberToken "-" |
      '4' -> nextNumberToken "-" |
      '5' -> nextNumberToken "-" |
      '6' -> nextNumberToken "-" |
      '7' -> nextNumberToken "-" |
      '8' -> nextNumberToken "-" |
      '9' -> nextNumberToken "-" |
       _  -> nextSymbolToken "-"
    in

    let rec nextToken () =
      match !ch with
      '\000' -> nextEndToken () |

      ' '    -> nextChar ();
                nextToken () |

      '\n'   -> nextChar ();
                nextToken () |

      '('    -> nextOpenParenToken () |

      ')'    -> nextCloseParenToken () |

      '-'    -> nextNumberOrSymbolToken () |

      '0' -> nextNumberToken "" |
      '1' -> nextNumberToken "" |
      '2' -> nextNumberToken "" |
      '3' -> nextNumberToken "" |
      '4' -> nextNumberToken "" |
      '5' -> nextNumberToken "" |
      '6' -> nextNumberToken "" |
      '7' -> nextNumberToken "" |
      '8' -> nextNumberToken "" |
      '9' -> nextNumberToken "" |

       _  -> nextSymbolToken ""

(* Finally initialize CH, and return NEXT TOKEN as promised. *)

    in nextChar () ;
      nextToken ;;

(* NEXT TOKENS. Test the token scanner by reading tokens from the file whose
   pathname is PATH, and writing one-line descriptions of each token. *)

let nextTokens path =
  let nextToken = makeScanner path
  in let rec nextTokensing token =
       match token
       with CloseParenToken ->
              Printf.printf "CloseParenToken\n" ;
              nextTokensing (nextToken ()) |

            EndToken ->
              Printf.printf "EndToken\n" |

            NumberToken number ->
              Printf.printf "NumberToken %i\n" number ;
              nextTokensing (nextToken ()) |

            OpenParenToken ->
              Printf.printf "OpenParenToken\n" ;
              nextTokensing (nextToken ()) |

            SymbolToken string ->
              Printf.printf "SymbolToken \"%s\"\n" string ;
              nextTokensing (nextToken ())

     in nextTokensing (nextToken ()) ;;


(* MAKE PARSER. Return a parser that reads THINGs from the file whose pathname
  is PATH. OCaml raises an exception if there's no such file. *)

let makeParser path =
 let nextToken = makeScanner path

(* NEXT THING. Read the THING whose first token is TOKEN, and return it. *)

 in let rec nextThing token =

(* NEXT THINGS. Read a series of THINGs as a Pure Lisp list, and return that
  list. *)

  let rec nextThings token =
    match token
    with CloseParenToken ->
           Nil |

         EndToken ->
           raise (Can'tParse "Unexpected end of file.") |

         _ ->
           let first = nextThing token
           in let rest = nextThings (nextToken ())
              in Cons (first, rest)

(* This is NEXT THINGS's body. *)

  in match token
     with CloseParenToken ->
            raise (Can'tParse "Unexpected close parenthesis.") |

          EndToken ->
            raise (Can'tParse "Unexpected end of file.") |

          NumberToken integer ->
            Number integer |

          OpenParenToken ->
            nextThings (nextToken ()) |

          SymbolToken string ->
            Symbol string

  (* This is NEXT THING's body. *)

  in (fun () -> nextThing (nextToken ())) ;;


(* THING. The type of a Pure Lisp object, from the lectures. *)

let rec printingThing thing =
  let rec printingThings thing =
    match thing with
     Cons (left,right) -> printf " ";
                          printingThing left;
                          printingThings right |
     Nil -> () |
     _ -> raise BadString

  in match thing with
    Closure (left,right,environment) -> printf "[closure]" |
    Cons (left,right) -> printf "(";
                        printingThing left;
                        printingThings right;
                        printf ")" |
    Nil -> printf "nil" |
    Number integer -> printf "%i" integer |
    Symbol string_ -> printf "%s" string_


let printThing thing =
  printingThing thing;
  printf ("\n");;


let lisp path =
  let nextThing = makeParser path
  in let rec lisping thing =
    match thing with
      Symbol ("end") -> () |
      _ -> printThing (eval thing);
           lisping (nextThing())
   in lisping (nextThing());;


(* OUTPUT FROM TEST CASES:

solve
solving
solving-add
solving-subtract
solving-multiply
solving-divide
is-inside
operator
left
right
t
nil
t
nil
nil
t
(= x (- c a))
(= x (- c b))
(= x (- a c))
(= x (+ b c))
(= x (/ c a))
(= x (/ c b))
(= x (/ a c))
(= x (* b c))
(= y (+ (* m x) b))
(= x (/ (- y b) m))
(= a (* e (+ f (/ (+ b c) d))))
- : unit = ()

*)
*)
*)
*)
