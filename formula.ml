type formula =
    | False
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
    (** Simplifies the formula so that the formula contains only the following
    operators :
        False, True, Prop, Not, Or , EU, EX, EG
        *)
    match f with
    | False | True | Prop _ -> f
    | Not a -> Not (simplify_formula a)
    | EX a -> EX (simplify_formula a)
    | EG a -> EG (simplify_formula a)
    | EU(a,b) -> EU  (simplify_formula a, simplify_formula b)
    | Or(a,b)  -> Or (simplify_formula a, simplify_formula a)
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
    | AU(a,b) ->
            let a_simp = simplify_formula a and b_simp = simplify_formula b in
            Not(
                Or(
                    EG (Not b_simp),
                EU (
                    Not b_simp,
                    simplify_formula (And(Not a_simp, Not b_simp))
                    )
                )
                )
;;

let rec optimize_formula f =
    (** This function reduces the size of simplified formulas by using logical
     * equivalences *)
    match f with
    | Not(Not(a)) -> optimize_formula a
    | Not(a) -> (match optimize_formula a with
        | True -> False
        | False -> True
        | _f -> Not(_f))
    | Or(a,b) -> (match (optimize_formula a, optimize_formula b) with
        | (False,False) -> False
        | (True,_) | (_,True) -> True
        | (False,_a) | (_a,False) -> _a
        | (_a,_b) when _a = _b -> _a
        | (_a,Not(_b)) when _a = _b -> True
        | (_a,_b) -> Or(_a,_b))
    | EU(a,b) -> (match (optimize_formula a, optimize_formula b) with
        | (_,True) -> True
        | (_,False) -> False
        | (_a,_b) -> EU(_a,_b))
    | EX(a) -> (match optimize_formula a with
        (* EX(True) is not always True :( *)
        | False -> False
        | _f -> EX(_f))
    | EG(a) -> (match optimize_formula a with
        (* EG(True) is not always True :( *)
        | False -> False
        | _f -> EG(_f))
    | _ -> f
;;

let rec formula_to_string (f:formula) : string =
    (** This function turns a formula into a "human readable" string *)
    match f with
    | False -> "false"
    | True -> "true"
    | Prop a -> Printf.sprintf "P%i" a
    | Not f ->
            Printf.sprintf
            (match f with
            | False | True | Prop(_) | EG(_) | EX(_) | EU(_,_) | Not(_) -> "¬%s"
            | _ -> "¬(%s)")
            (formula_to_string f)
    | And (f1,f2) -> Printf.sprintf "%s ∧ %s" (formula_to_string f1)
        (formula_to_string f2)
    | Or (f1,f2) -> Printf.sprintf "%s ∨ %s" (formula_to_string f1)
        (formula_to_string f2)
    | Imply (f1,f2) -> Printf.sprintf "%s ⇒ %s" (formula_to_string f1)
        (formula_to_string f2)
    | Equiv (f1,f2) -> Printf.sprintf "%s ⇔ %s" (formula_to_string f1)
        (formula_to_string f2)
    | AX (a) -> Printf.sprintf "AX(%s)" (formula_to_string a)
    | EX (a) -> Printf.sprintf "EX(%s)" (formula_to_string a)
    | AF (a) -> Printf.sprintf "AF(%s)" (formula_to_string a)
    | EF (a) -> Printf.sprintf "EF(%s)" (formula_to_string a)
    | AG (a) -> Printf.sprintf "AG(%s)" (formula_to_string a)
    | EG (a) -> Printf.sprintf "EG(%s)" (formula_to_string a)
    | AU (a,b) -> Printf.sprintf "A(%s U %s)" (formula_to_string a)
        (formula_to_string b)
    | EU (a,b) -> Printf.sprintf "E(%s U %s)" (formula_to_string a)
        (formula_to_string b)
;;
