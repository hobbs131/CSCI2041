(* Tanner Hobbs
   Lab: 6 *)

type 'base mutyQueue =
  MutyQueueNode
  of 'base *
     'base mutyQueue ref *
     'base mutyQueue ref ;;

let mutyQueueMake s =
  let rec a =
    MutyQueueNode (s, ref a, ref a)
  in a

let mutyQueueEmpty q =
  match q with
  MutyQueueNode(q_key,left,right) ->
  if q == !left && q == !right
    then true
  else
    false

let mutyQueueEnqueue q e =
  match q with
  MutyQueueNode(q_key,left,right) ->
    match !left with
    MutyQueueNode(left_key,left_1,right_1) ->
      let e_node = MutyQueueNode(e, left, ref q)
      in
      right_1 := e_node;
      left := e_node


let mutyQueueDequeue q =
  match q
  with MutyQueueNode (q_key,left,right) ->
    if mutyQueueEmpty q
      then q_key
    else
      match q with
      MutyQueueNode(q_key,left,right) ->
        match !right with
          MutyQueueNode(e, left_1, right_of_right) ->
            match !right_of_right with
              MutyQueueNode(q_key,left_2,right_2) ->
                right := !right_of_right ;
                left_2 := !right ;
    e
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


(* Output from tests:

#use "queue.ml";;
type 'base mutyQueue =  MutyQueueNode of 'base * 'base mutyQueue ref * 'base mutyQueue ref          val mutyQueueMake : 'a -> 'a mutyQueue = <fun>
val mutyQueueEmpty : 'a mutyQueue -> bool = <fun>
val mutyQueueEnqueue : 'a mutyQueue -> 'a -> unit = <fun>
val mutyQueueDequeue : 'a mutyQueue -> 'a = <fun>
type 'base mutyQueue =
    MutyQueueNode of 'base * 'base mutyQueue ref * 'base mutyQueue ref
val queue : string mutyQueue =
  MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>})

- : bool = true
- : string = ""
- : unit = ()
- : bool = false
- : unit = ()
- : unit = ()
- : string = "A"
- : string = "B"
- : string = "C"
- : bool = true
- : string = ""
- : string = ""  *)
