type formula =
    False
  | True
  | Prop of int
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
;;

let rec formula_to_string (f:formula) : string =
    match f with
    | False -> "false"
    | True -> "true"
    | Prop a -> Printf.sprintf "P%i" a
    | Not f -> Printf.sprintf "¬%s" (formula_to_string f)
    | And (f1,f2) -> Printf.sprintf "%s∧%s" (formula_to_string f1)
        (formula_to_string f2)
    | Or (f1,f2) -> Printf.sprintf "%s∨%s" (formula_to_string f1)
        (formula_to_string f2)
    | Imply (a,b) -> Printf.sprintf "%s⇒%s" (formula_to_string f1)
        (formula_to_string f2)
    | Equiv (a,b) -> Printf.sprintf "%s⇔%s" (formula_to_string f1)
        (formula_to_string f2)
    | AX (a) -> Print.sprintf "AX(%s)" (formula_to_string a)
    | EX (a) -> Print.sprintf "EX(%s)" (formula_to_string a)
    | AF (a) -> Print.sprintf "AF(%s)" (formula_to_string a)
    | EF (a) -> Print.sprintf "EF(%s)" (formula_to_string a)
    | AF (a) -> Print.sprintf "AG(%s)" (formula_to_string a)
    | EF (a) -> Print.sprintf "EG(%s)" (formula_to_string a)
    | AU (a,b) -> Print.sprintf "A(%sU%s)" (formula_to_string a)
        (formula_to_string b)
    | EU (a,b) -> Print.sprintf "E(%sU%s)" (formula_to_string a)
        (formula_to_string b)
;;
