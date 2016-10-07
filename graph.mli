type valuation = int -> bool
type graph = ((int  list) * valuation) array 

val prop_value : graph -> int -> int -> bool
val next_prop_value : graph -> int -> int -> bool
