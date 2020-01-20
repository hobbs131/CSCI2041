open List;;
exception EnumerationOutOfRange;;

let rec some pred things =
  if things = []
    then false
  else if pred (hd things) = true
    then true
  else
    some pred (tl things)

let rec every pred things =
  if things = []
    then true
  else if pred (hd things) = true
    then every pred (tl things)
  else
    false

let rec mapcon func things =
  let a = things in
  if things = []
    then func a
  else
    func a @ (mapcon func (tl things))

let rec uniquify things =
  mapcon (fun things -> match
                        things with [] -> [] |
                        h::t -> if some (fun element -> element = h) t
                                  then []
                                else
                                  [h] ) things;;

let enumeration start stop step =
  let current = ref start in
  let rec enumerating () =
    if !current <= stop
     then let temp = !current in current := temp + step;
                                            temp
    else raise EnumerationOutOfRange

    in enumerating;;


    (*
      CSci 2041 Lab Assignment 7

        James Moen
        22 Oct 19

      30 points.
    *)

    (*
let rec some pred things =
  if things = []
    then false
  else if pred (hd things) = true
    then true
  else
    some pred (tl things)
        *)

    some (fun number -> number mod 2 = 0) [] ;;          (* 1 pt. false *)
    some (fun number -> number mod 2 = 0) [1] ;;         (* 1 pt. false *)
    some (fun number -> number mod 2 = 0) [2] ;;         (* 1 pt. true  *)
    some (fun number -> number mod 2 = 0) [1 ; 2 ; 3] ;; (* 1 pt. true  *)
    some (fun number -> number mod 2 = 0) [1 ; 3 ; 5] ;; (* 1 pt. false *)

    (*
let rec every pred things =
      if things = []
        then true
      else if pred (hd things) = true
        then every pred (tl things)
      else
        false
            *)

    every (fun number -> number mod 2 = 0) [] ;;          (* 1 pt. true  *)
    every (fun number -> number mod 2 = 0) [1] ;;         (* 1 pt. false *)
    every (fun number -> number mod 2 = 0) [2] ;;         (* 1 pt. true  *)
    every (fun number -> number mod 2 = 0) [1 ; 2 ; 3] ;; (* 1 pt. false *)
    every (fun number -> number mod 2 = 0) [2 ; 4 ; 6] ;; (* 1 pt. true  *)

    (*
let rec mapcon func things =
  let a = things in
  if things = []
    then func a
  else
    func a @ (mapcon func (tl things))
         *)

    mapcon (fun thing -> thing) [] ;;
    (* 1 pt. [] *)

    mapcon (fun thing -> thing) [1 ; 2 ; 3] ;;
    (* 2 pt. [1 ; 2 ; 3 ; 2 ; 3 ; 3] *)

    mapcon (fun thing -> [List.length thing]) [1 ; 2 ; 3] ;;
    (* 2 pt. [3 ; 2 ; 1 ; 0] *)

    (*
let rec uniquify things =
  mapcon (fun things -> match
                        things with [] -> [] |
                        h::t -> if some (fun element -> element = h) t
                                  then []
                                else
                                  [h] ) things;;
                                      *)

    uniquify [] ;;               (* 1 pt. [] *)
    uniquify [1] ;;              (* 1 pt. [1] *)
    uniquify [1 ; 2] ;;          (* 1 pt. [1 ; 2] *)
    uniquify [1 ; 1] ;;          (* 1 pt. [1] *)
    uniquify [1 ; 2; 1; 3; 1] ;; (* 1 pt. [2 ; 3 ; 1] *)

    (*
let enumeration start stop step =
  let current = ref start in
  let rec enumerating () =
    if !current <= stop
     then let temp = !current in current := temp + step;
                                            temp
    else raise EnumerationOutOfRange

    in enumerating;;
        *)


    let oneThruThree = enumeration 1 3 1 ;;
    let byTwos       = enumeration 0 4 2 ;;

    oneThruThree ()  ;;                                               (* 1 pt.  1 *)
    byTwos () ;;                                                     (* 1 pt.  0 *)
    oneThruThree () ;;                                               (* 1 pt.  2 *)
    oneThruThree () ;;                                               (* 1 pt.  3 *)
    try oneThruThree () with EnumerationOutOfRange -> -1 | _ -> 0 ;; (* 2 pt. -1 *)
    byTwos () ;;                                                     (* 1 pt.  2 *)
    try oneThruThree () with EnumerationOutOfRange -> -1 | _ -> 0 ;; (* 1 pt. -1 *)
    byTwos () ;;                                                     (* 1 pt.  4 *)
    try byTwos () with EnumerationOutOfRange -> -1 | _ -> 0 ;;       (* 1 pt. -1 *)
