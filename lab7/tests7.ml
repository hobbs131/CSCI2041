(*
  CSci 2041 Lab Assignment 7

    James Moen
    22 Oct 19

  30 points.
*)

(* YOUR CODE FOR SOME GOES HERE! *)

some (fun number -> number mod 2 = 0) [] ;;          (* 1 pt. false *)
some (fun number -> number mod 2 = 0) [1] ;;         (* 1 pt. false *)
some (fun number -> number mod 2 = 0) [2] ;;         (* 1 pt. true  *)
some (fun number -> number mod 2 = 0) [1 ; 2 ; 3] ;; (* 1 pt. true  *)
some (fun number -> number mod 2 = 0) [1 ; 3 ; 5] ;; (* 1 pt. false *)

(* YOUR CODE FOR EVERY GOES HERE! *)

every (fun number -> number mod 2 = 0) [] ;;          (* 1 pt. true  *)
every (fun number -> number mod 2 = 0) [1] ;;         (* 1 pt. false *)
every (fun number -> number mod 2 = 0) [2] ;;         (* 1 pt. true  *)
every (fun number -> number mod 2 = 0) [1 ; 2 ; 3] ;; (* 1 pt. false *)
every (fun number -> number mod 2 = 0) [2 ; 4 ; 6] ;; (* 1 pt. true  *)

(* YOUR CODE FOR MAPCON GOES HERE! *)

mapcon (fun thing -> thing) [] ;;
(* 1 pt. [] *)

mapcon (fun thing -> thing) [1 ; 2 ; 3] ;;
(* 2 pt. [1 ; 2 ; 3 ; 2 ; 3 ; 3] *)

mapcon (fun thing -> [List.length thing]) [1 ; 2 ; 3] ;;
(* 2 pt. [3 ; 2 ; 1 ; 0] *)

(* YOUR CODE FOR UNIQUIFY GOES HERE! *)

uniquify [] ;;               (* 1 pt. [] *)
uniquify [1] ;;              (* 1 pt. [1] *)
uniquify [1 ; 2] ;;          (* 1 pt. [1 ; 2] *)
uniquify [1 ; 1] ;;          (* 1 pt. [1] *)
uniquify [1 ; 2; 1; 3; 1] ;; (* 1 pt. [2 ; 3 ; 1] *)

(* YOUR CODE FOR ENUMERATION GOES HERE! *)


let oneThruThree = enumeration 1 3 1 ;;
let byTwos       = enumeration 0 4 2 ;;

oneThruThree () ;;                                               (* 1 pt.  1 *)
byTwos () ;;                                                     (* 1 pt.  0 *)
oneThruThree () ;;                                               (* 1 pt.  2 *)
oneThruThree () ;;                                               (* 1 pt.  3 *)
try oneThruThree () with EnumerationOutOfRange -> -1 | _ -> 0 ;; (* 2 pt. -1 *)
byTwos () ;;                                                     (* 1 pt.  2 *)
try oneThruThree () with EnumerationOutOfRange -> -1 | _ -> 0 ;; (* 1 pt. -1 *)
byTwos () ;;                                                     (* 1 pt.  4 *)
try byTwos () with EnumerationOutOfRange -> -1 | _ -> 0 ;;       (* 1 pt. -1 *)
