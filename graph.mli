open Formula

type valuation = int -> bool
type graph = ((int  list) * valuation) array

val evaluate : graph -> int -> formula -> bool
