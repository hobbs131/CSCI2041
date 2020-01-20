(*
  Tests for CSci 2041 Computer Laboratory 1
  James Moen
  10 Sep 19

  36 points total
*)

open Printf ;;

(* PRINT OBJECTS. Print a list of OBJECTs L to standard output using a function
   F. You don't have to write this, or even know how it works. *)

let printObjects f l =
  let rec printingObjects l =
    match l with
      [] -> () |
      h :: t -> printf " ; " ; f h ; printingObjects t
  in printf "[" ;
     (match l with
       [] -> () |
       h :: t -> f h ; printingObjects t) ;
     printf "]\n" ;;

(* PRINT INTS, PRINT STRINGS. Print a list of INTs or a list of STRINGs. *)

let printInts    = printObjects (fun e -> printf "%i" e) ;;
let printStrings = printObjects (fun e -> printf "\"%s\"" e) ;;

(* Tests for MEMBER. *)

printf "%b\n" (member 1 []) ;;                         (* 2 false *)
printf "%b\n" (member 1 [1]) ;;                        (* 2 true  *)
printf "%b\n" (member 2 [1 ; 2 ; 3]) ;;                (* 2 true  *)
printf "%b\n" (member 5 [2 ; 4 ; 6]) ;;                (* 2 false *)
printf "%b\n" (member "c" ["a" ; "b" ; "c" ; "d"]) ;;  (* 2 true  *)
printf "%b\n" (member "x" ["a" ; "b" ; "c" ; "d"]) ;;  (* 2 false *)

(* Tests for DELETE. *)

printInts    (delete 1 []) ;;                          (* 2 []          *)
printInts    (delete 1 [1]) ;;                         (* 2 []          *)
printInts    (delete 1 [1 ; 2 ; 3]) ;;                 (* 2 [2 ; 3]     *)
printInts    (delete 4 [1 ; 2 ; 3]) ;;                 (* 2 [1 ; 2 ; 3] *)
printInts    (delete 3 [1 ; 2 ; 3]) ;;                 (* 2 [1 ; 2]     *)
printInts    (delete 1 [1 ; 2 ; 1 ; 3 ; 1 ; 4]) ;;     (* 2 [2 ; 3 ; 4] *)
printStrings (delete "a" ["x" ; "a" ; "y" ; "a"]) ;;   (* 2 ["x" ; "y"] *)
printStrings (delete "a" ["a" ; "a" ; "a"]) ;;         (* 2 []          *)

(* Tests for MEAN. *)

printf "%G\n" (mean [1.0]) ;;                          (* 2 1           *)
printf "%G\n" (mean [1.0 ; 2.0]) ;;                    (* 2 1.5         *)
printf "%G\n" (mean [1.0 ; 1.0 ; 1.0]) ;;              (* 2 1.0         *)
printf "%G\n" (mean [1.0 ; 0.0 ; -1.0 ; 1.0]) ;;       (* 2 0.25        *)
