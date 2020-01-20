(*
  Tests for CSci 2041 Computer Laboratory 2
  James Moen
  17 Sep 19

  45 points total
*)

open Printf ;;

(* PRINT PAIR. Print P, a tuple of two integers, to standard output. *)

let printPair p =
  printf "(%i, %i)\n" (fst p) (snd p) ;;

(* PRINT BOOL. Print B, a Boolean. *)

let printBool b =
  printf "%b\n" b ;;

(* Simple tests for rational arithmetic functions. *)

printPair (rat 0 1) ;;                          (* 2 (0, 1) *)
printPair (rat 1 1) ;;                          (* 2 (1, 1) *)
printPair (rat 33 99) ;;                        (* 2 (1, 3) *)

printPair (ratAdd (rat 0 1) (rat 1 2)) ;;       (* 2 (1, 2) *)
printPair (ratAdd (rat 1 2) (rat 1 2)) ;;       (* 2 (1, 1) *)
printPair (ratAdd (rat 3 5) (rat 2 5)) ;;       (* 2 (1, 1) *)

printPair (ratMul (rat 0 1) (rat 1 2)) ;;       (* 2 (0, 1) *)
printPair (ratMul (rat 1 2) (rat 2 1)) ;;       (* 2 (1, 1) *)
printPair (ratMul (rat 1 3) (rat 6 1)) ;;       (* 2 (2, 1) *)

printPair (ratDiv (rat 1 1) (rat 2 1)) ;;       (* 2 (1, 2) *)
printPair (ratDiv (rat 2 1) (rat 2 1)) ;;       (* 2 (1, 1) *)
printPair (ratDiv (rat 2 1) (rat 1 2)) ;;       (* 2 (4, 1) *)

printBool (ratGt (rat 0 1) (rat 0 1)) ;;        (* 2 false *)
printBool (ratGt (rat 1 2) (rat 0 1)) ;;        (* 2 true  *)
printBool (ratGt (rat 1 3) (rat 3 9)) ;;        (* 2 false *)

(* A complicated test for EULER. *)

printPair (euler (rat 1 100000)) ;;             (* 15 (109601, 40320) *)
