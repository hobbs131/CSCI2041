(* Name: Tanner Hobbs
   Lab #: 6
*)

let rec gcd i j =
  if i <> 0
  then if j > i
       then gcd i (j - i)
       else gcd (i - j) j
  else j ;;

let rat n d  =
  let fst = n / gcd n d in
  let snd = d / gcd n d in
  (fst,snd) ;;

let ratAdd a b =
  let num_a = fst (a) in
  let num_b = fst(b) in
  let den_a = snd (a) in
  let den_b = snd (b) in

  let new_num_a = num_a * den_b in
  let new_num_b = num_b * den_a in
  let new_den_a = den_a * den_b in
  let total_num = new_num_a + new_num_b in
  rat total_num new_den_a

let ratMul a b =
  let num_a = fst (a) in
  let num_b = fst(b) in
  let den_a = snd (a) in
  let den_b = snd (b) in
  let multiplied_num = num_a * num_b in
  let multiplied_den = den_a * den_b in
  rat multiplied_num multiplied_den;;

let ratDiv a b =
  let num_b = fst(b) in
  let den_b = snd (b) in
  ratMul (a) (den_b, num_b);;

let ratGt a b =
  let num_a = fst (a) in
  let num_b = fst(b) in
  let den_a = snd (a) in
  let den_b = snd (b) in
  let new_num_a = num_a * den_b in
  let new_num_b = num_b * den_a in

  if new_num_a > new_num_b
    then true
  else
    false

let euler epsilon =
  let rec epsiloning c s t =

    if ratGt (t) (epsilon)
      then
      let new_s = ratAdd (s) (t) in
      let new_t = ratDiv (t) (c) in
      let new_c = ratAdd (c) (1,1) in
      epsiloning (new_c) (new_s) (new_t)

    else
      s
  in epsiloning (rat 1 1) (rat 0 1) (rat 1 1);;

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

  (*
  RESULTS:
  #use "functions.ml";;
  val gcd : int -> int -> int = <fun>
  val rat : int -> int -> int * int = <fun>
  val ratAdd : int * int -> int * int -> int * int = <fun>
  val ratMul : int * int -> int * int -> int * int = <fun>
  val ratDiv : int * int -> int * int -> int * int = <fun>
  val ratGt : int * int -> int * int -> bool = <fun>
  val euler : int * int -> int * int = <fun>
  val printPair : int * int -> unit = <fun>
  val printBool : bool -> unit = <fun>
  (0, 1)
  - : unit = ()
  (1, 1)
  - : unit = ()
  (1, 3)
  - : unit = ()
  (1, 2)
  - : unit = ()
  (1, 1)
  - : unit = ()
  (1, 1)
  - : unit = ()
  (0, 1)
  - : unit = ()
  (1, 1)
  - : unit = ()
  (2, 1)
  - : unit = ()
  (1, 2)
  - : unit = ()
  (1, 1)
  - : unit = ()
  (4, 1)
  - : unit = ()
  false
  - : unit = ()
  true
  - : unit = ()
  false
  - : unit = ()
  (109601, 40320)
  - : unit = ()
*)
