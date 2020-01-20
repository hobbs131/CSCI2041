open List;;


let rec member e l =
  if l = []
    then false
  else if e = (hd l)
    then true
  else
    member e (tl l)


let rec delete e l =
  if l = []
    then l
  else if e = (hd l)
    then delete e (tl l)
  else
    (hd l) :: delete e (tl l)

let mean l =
  let rec length l =
    if l = []
      then 0.0
    else
     length (tl l) +. 1.0
  in

  let rec sum l =
    if l = []
      then 0.0
    else sum (tl l) +. (hd l)
  in sum l /. length l

(*

false
- : unit = ()
true
- : unit = ()
true
- : unit = ()
false
- : unit = ()
true
- : unit = ()
false
- : unit = ()
[]
- : unit = ()
[]
- : unit = ()
[2 ; 3]
- : unit = ()
[1 ; 2 ; 3]
- : unit = ()
[1 ; 2]
- : unit = ()
[2 ; 3 ; 4]
- : unit = ()
["x" ; "y"]
- : unit = ()
[]
- : unit = ()
1
- : unit = ()
1.5
- : unit = ()
1
- : unit = ()
0.25
- : unit = ()

*)
