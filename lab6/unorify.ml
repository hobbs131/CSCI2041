  (* Tanner Hobbs
     Lab #: 6 *)
  (*
    CSci 2041 Lab Assignment 6

      James Moen
      14 Oct 19

    It's worth 30 points.
  *)

  (* PROPOSITION. An expression in propositional logic using '¬', '∧', and '∨'.

     a, b, c ...  ↝  Var "a", Var "b", Var "c" ...
     α ∧ β        ↝  And (α, β)
     α ∨ β        ↝  Or (α, β)
     ¬ α          ↝  Not α

     The squiggly arrow '↝' means "represented as." *)

  type proposition =
    Var of string |
    And of proposition * proposition |
    Or of proposition * proposition |
    Not of proposition ;;




  (*
let rec unorify p =
  match p with
  And ( a, b) -> And (unorify a,unorify b) |
  Or ( a,  b) -> Not(And(unorify (Not a), unorify (Not b))) |
  Not (Var a) -> Not (Var a) |
  Var a -> Var a |
  Not (And (a,b)) -> Not(And( unorify a, unorify b)) |
  Not (Or (a, b)) -> And(unorify (Not (a)), unorify (Not ( b))) |
  Not (Not (a)) ->  unorify a |
  Not (a) -> Not (unorify a) ;;

  *)



  (* Unorify the proposition a. *)

  unorify (Var "a");;

  (* 2 points if you get: Var "a" *)




  (* Unorify the proposition ¬ a. *)

  unorify (Not(Var "a"));;

  (* 3 points if you get: Not (Var "a") *)




  (* Unorify the proposition ¬ ¬ a. *)

  unorify (Not(Not(Var "a")));;

  (* 5 points if you get: Var "a" *)




  (* Unorify the proposition ¬ (a ∨ b). *)

 unorify (Not(Or(Var "a", Var "b")));;

  (* 5 points if you get: And (Var "a", Var "b") *)




  (* Unorify the proposition ¬ (a ∧ b). *)

  unorify (Not(And(Var "a", Var "b")));;

  (* 5 points if you get: Not (And (Var "a", Var "b")) *)




  (* Unorify the proposition ¬ a ∨ ¬ b. *)

  unorify (Or(Not(Var "a"), Not(Var "b")));;

  (* 5 points if you get: Not (And (Var "a", Var "b")) *)




  (* Unorify the proposition ¬ ¬ a ∨ ¬ b. *)

  unorify (Or(Not (Not (Var "a")), Not (Var "b")));;

  (* 5 points if you get: Not (And (Not (Var "a"), Var "a")) *)
