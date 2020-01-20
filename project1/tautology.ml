type proposition =
  False |
  True |
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition |
  Imply of proposition * proposition |
  Equiv of proposition * proposition ;;

type conditional =
  IffyFalse |
  IffyTrue |
  IffyVar of string |
  If of conditional * conditional * conditional ;;

let rec ifify p =
  match p with
    False -> IffyFalse |
    True -> IffyTrue |
    Var a -> IffyVar a |
    And (a,b) -> If (ifify a, ifify b, IffyFalse) |
    Or (a,b) ->  If (ifify a, IffyTrue, ifify b) |
    Not a -> If (ifify a, IffyFalse, IffyTrue) |
    Imply (a,b) -> If (ifify a, ifify b, IffyTrue) |
    Equiv (a,b) -> If (ifify a, ifify b, If(ifify b, IffyFalse, IffyTrue));;

let rec normalize c =
  let rec normalizing pi a b =
    match pi with
      If (left,mid,right) -> normalizing left (If(mid,a,b)) (If(right,a,b)) |
      _ -> If( pi, normalize a, normalize b) in
      match c with
        If(pi1,a1,b1) -> normalizing pi1 a1 b1 |
        _ -> c

let substitute c v b =
  let rec substituting c =
    match c with
      If(pi,a,b) -> If(substituting pi, substituting a, substituting b) |
      otherTerm ->
        if c = v
          then b
        else otherTerm
  in substituting c;;

let rec simplify c =
  match c with
    If(pi,a,b) -> let a = simplify (substitute a pi IffyTrue) in
                  let b = simplify (substitute b pi IffyFalse) in
                  if pi = IffyTrue
                    then a
                  else if pi = IffyFalse
                    then b
                  else if a = IffyTrue && b = IffyFalse then
                    pi
                  else if a = b
                    then a
                  else If(pi,a,b) |
    _ -> c;;


let tautology p =
  let c = ifify p in
  let d = normalize c in
  let e = simplify d in
  match e with IffyTrue -> true |
               IffyFalse -> false |
               _ -> false;;


 (* Calls:   (Imply(Not (And(Var "p", Var "q")), Or(Not (Var "p"), Not (Var "q"))))
             (And (Var "p", Not(Var "p")))
             (Or (Var "p", Not(Var "p")))
             (Imply(Var "p", Var "p"))
             (Imply(Not(Var "p"), Var "p"))
             (Imply(Var "p", Or(Var "p", Var "p")))
             (Imply(Not (Var "p"), Or(Var "p", Var "p")))
             (Imply(Var "p", And(Var "p", Var "p")))
             (Imply(Not(Var "p"), And(Var "p", Var "p")))
             (Imply(And(Var "p", Var "q"), Var "q"))
             (Imply(And(Var "p", Var "q"), Not(Var "q")))
             *)
