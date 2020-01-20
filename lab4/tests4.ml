(*
  CSci 2041 Lab Assignment 4

    James Moen
    01 Oct 19

  Tests.
*)

(* MUTY QUEUE. A mutable queue of BASEs, as a circular doubly linked list. The
   type doesn't say that the list is circular or doubly linked. That's done by
   the functions that manipulate MUTY QUEUEs. All those functions must work in
   O(1) time. *)

type 'base mutyQueue =
  MutyQueueNode of
   'base *
   'base mutyQueue ref *
   'base mutyQueue ref ;;

(* 

  Put your code for MUTY QUEUE MAKE, MUTY QUEUE EMPTY, MUTY QUEUE ENQUEUE and
  MUTY QUEUE DEQUEUE here.

*)

(* Make a QUEUE whose sentinel is the empty string "" and test it. The comments
   say what each test should return, and how many points you get (if any) for
   successful tests. *)

let queue = mutyQueueMake "" ;;

(* 2 pt. MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>}) *)

mutyQueueEmpty queue ;;           (* 2 pt. true *)

mutyQueueDequeue queue ;;         (* 2 pt. "" *)

mutyQueueEnqueue queue "A" ;;     (* 1 pt. () *)

mutyQueueEmpty queue ;;           (* 2 pt. false *)

mutyQueueEnqueue queue "B" ;;     (* 1 pt. () *)

mutyQueueEnqueue queue "C" ;;     (* 1 pt. () *)

mutyQueueDequeue queue ;;         (* 5 pt. "A" *)

mutyQueueDequeue queue ;;         (* 5 pt. "B" *)

mutyQueueDequeue queue ;;         (* 5 pt. "C" *)

mutyQueueEmpty queue ;;           (* 2 pt. true *)

mutyQueueDequeue queue ;;         (* 5 pt. "" *)

mutyQueueDequeue queue ;;         (* 2 pt. "" *)
