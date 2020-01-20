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

YOUR DEFINITION FOR UNORIFY GOES HERE!

*)




(* Unorify the proposition a. *)

(* YOUR CALL TO UNORIFY GOES HERE! *)

(* 2 points if you get: Var "a" *)




(* Unorify the proposition ¬ a. *)

(* YOUR CALL TO UNORIFY GOES HERE! *)

(* 3 points if you get: Not (Var "a") *)




(* Unorify the proposition ¬ ¬ a. *)

(* YOUR CALL TO UNORIFY GOES HERE! *)

(* 5 points if you get: Var "a" *)




(* Unorify the proposition ¬ (a ∨ b). *)

(* YOUR CALL TO UNORIFY GOES HERE! *)

(* 5 points if you get: And (Var "a", Var "b") *)




(* Unorify the proposition ¬ (a ∧ b). *)

(* YOUR CALL TO UNORIFY GOES HERE! *)

(* 5 points if you get: Not (And (Var "a", Var "b")) *)




(* Unorify the proposition ¬ a ∨ ¬ b. *)

(* YOUR CALL TO UNORIFY GOES HERE! *)

(* 5 points if you get: Not (And (Var "a", Var "b")) *)




(* Unorify the proposition ¬ ¬ a ∨ ¬ b. *)

(* YOUR CALL TO UNORIFY GOES HERE! *)

(* 5 points if you get: Not (And (Not (Var "a"), Var "a")) *)
