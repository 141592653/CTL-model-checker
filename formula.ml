
type formula =
    | False
    | True
    | Prop of string
    | Not of formula
    | And of formula * formula
    | Or of formula * formula
    | Imply of formula * formula
    | Equiv of formula * formula
    | AX of formula
    | EX of formula
    | AF of formula
    | EF of formula
    | AG of formula
    | EG of formula
    | AU of formula * formula
    | EU of formula * formula
;;

let rec simplify_formula f =
  (** Simplifies the formula so that the formula contains only the following operators : 
      False, True, Prop, Not, Or , EU, EX, EG
  *)
  match f with 
  | False | True | Prop(_) | Not(_) | Or(_,_) | EX(_) | EG(_) | EU(_) -> f
  | And(a,b) -> 
    Not(
      Or(
	Not (simplify_formula a),
	Not (simplify_formula b)
      )
    )
  | Imply(a,b) -> Or (Not (simplify_formula a) ,(simplify_formula b))
  | Equiv(a,b) -> simplify_formula (And(Imply(a,b), Imply(b,a)))
  | AX(a) -> Not (EX (Not (simplify_formula a)))
  | EF(a) -> EU (True, (simplify_formula a))
  | AG(a) -> Not (EF (Not (simplify_formula a)))
  | AF(a) -> Not (EG (Not (simplify_formula a)))
  | AU(a,b) -> let a_simp = simplify_formula a and b_simp = simplify_formula b in 
    Not(
      Or(
	EG (Not b_simp),
	EU (
	  Not b_simp,
	  And(Not a_simp, Not b_simp)
	)
      )
    )

 
