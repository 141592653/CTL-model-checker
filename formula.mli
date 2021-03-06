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

val simplify_formula : formula -> formula
val optimize_formula : formula -> formula
val formula_to_string : formula -> string
