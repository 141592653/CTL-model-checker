open Formula

type valuation = int -> bool
type graph = ((int  list) * valuation) array

(* The labels put on vertices within algorithms *)
type label = Unvisited | Visited of bool

let rec evaluate g v f =
    (** This function returns true iff formula f is true in g from v *)
    let evaluate_next g v p =
        (** This function returns true iff p is true in at least one
         * of the vertices neighbours of v *)
        let rec find_p_true l =
            match l with
            | [] -> false
            | v'::q -> if evaluate g v' p then true else find_p_true q
        in
        find_p_true (fst g.(v))
    in
    let evaluate_globally g v p =
        (** This function returns true iff there exists an infinite path along
         * which p is true *)
        let labels = Array.make (Array.length g) Unvisited in
        let rec find_p_true node =
            match labels.(node) with
            | Visited(a) -> a
            | Unvisited ->
                    if evaluate g node p
                    then (labels.(node) <- Visited(true);
                                List.exists find_p_true (fst g.(node)))
                    else (labels.(node) <- Visited(false);
                                false)
        in
        find_p_true v
    in
    let evaluate_until g v p q =
        (** This function returns true iff there exists a path along which p is
         * true leading to a state where q is true *)
        let labels = Array.make (Array.length g) Unvisited in
        let rec march node =
            match labels.(node) with
            | Visited(_) -> false
            | Unvisited ->
                    if evaluate g node q then true
                    else (if evaluate g node p
                    then (labels.(node) <- Visited(true);
                        List.exists march (fst g.(node)))
                    else (labels.(node) <- Visited(false);
                        false))
        in
        march v
    in
    match f with
    | True -> true
    | False -> false
    | Prop(a) -> snd g.(v) a
    | Not(a) -> not (evaluate g v a)
    | Or(a,b) -> (evaluate g v a) || (evaluate g v b)
    | EX(a) -> evaluate_next g v a
    | EG(a) -> evaluate_globally g v a
    | EU(a,b) -> evaluate_until g v a b
