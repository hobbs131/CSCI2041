(*
  CSci 2041 Lab Assignment 10

    James Moen
    10 Nov 19

  22 points.
*)

(* BST. An unbalanced binary search tree with STRING keys and VALUE values. *)

type 'value bst =
  BSTEmpty |
  BSTNode of (string * 'value * 'value bst * 'value bst) ;;

(* LAYERS. A list of BSTs whose values are 'VALUEs. *)

type 'value layers = ('value bst) list ;;
open Printf;;
open List;;

(* LAYER ERROR. RAISEd if there's an error in one of the LAYER functions. *)

exception LayerError of string ;;

let rec layerGet layers name =
  let rec layerGetting layer =
    match layer with
      BSTEmpty -> layerGet (tl layers) name |
      BSTNode (key,value,left,right) -> if name > key
                                          then layerGetting right
                                        else if name < key
                                          then layerGetting left
                                        else value
  in match layers with
    [] -> raise (LayerError ("No binding for " ^ (name))) |
    _ -> layerGetting (hd layers)

(* An example environment with three layers. Its trees bind "a" through "h" to
   some integers. *)

let environment =
  [BSTNode ("i", 7,
     BSTNode ("h", 8,
       BSTNode ("g", 9, BSTEmpty, BSTEmpty), BSTEmpty),
       BSTEmpty);
   BSTNode ("d", 4,
     BSTEmpty,
     BSTNode ("e", 5, BSTEmpty,
       BSTNode ("f", 6, BSTEmpty, BSTEmpty)));
   BSTNode ("b", 1,
     BSTNode ("a", 2, BSTEmpty, BSTEmpty),
     BSTNode ("c", 3, BSTEmpty, BSTEmpty))] ;;


(* Tests, each with a point value. *)

layerGet environment "b" ;;       (* 2 pt. 1 *)

layerGet environment "a" ;;       (* 2 pt. 2 *)

layerGet environment "c" ;;       (* 2 pt. 3 *)

layerGet environment "d" ;;       (* 2 pt. 4 *)

layerGet environment "e" ;;       (* 2 pt. 5 *)

layerGet environment "f" ;;       (* 2 pt. 6 *)

layerGet environment "i" ;;       (* 2 pt. 7 *)

layerGet environment "h" ;;       (* 2 pt. 8 *)

layerGet environment "g" ;;       (* 2 pt. 9 *)

try layerGet environment "x"      (* 2 pt. No binding for x. *)
with LayerError message ->
  Printf.printf "%s\n" message ;
  0 ;;

try layerGet [] "y"               (* 2 pt. No binding for y. *)
with LayerError message ->
  Printf.printf "%s\n" message ;
  0 ;;
