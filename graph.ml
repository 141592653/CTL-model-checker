type valuation = int -> bool
type graph = ((int  list) * valuation) array

let prop_value g v p =
  (** This function takes as arguments a graph g, a vertex v and a
   * proposition p and return true iff p is true on v in g *)

  snd g.(v) p

let next_prop_value g v p =
  (** This function returns true iff p is true in at least one
   * of the vertices neighbours of v *)
  let rec find_p_true l = match l with
    |[] -> false
    |v'::q -> if prop_value g v' p then
	    true
	  else
	    find_p_true q in
  find_p_true (fst g.(v))
