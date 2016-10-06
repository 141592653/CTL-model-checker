
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
