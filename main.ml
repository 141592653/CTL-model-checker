open Formula
open Graph

let f = AX(
	    Equiv(
		EU(
		    AG(
			Prop 0
		      ),
		    AU(
			EF(Prop 1),Prop 2
		      )
		  ),
		Or(
		    Not(
			EG(
			    EX(
				And(
				    True,False
				  )
			      )
			  )
		      ),
		    AF(
			Imply(Prop 0,Prop 1)
		      )
		  )
	      ))

let test_EX_f = EX(Prop 0)
let test_EX_g = [|([1;2],fun i -> i == 1);
		  ([], fun i -> i == 0 || i == 1);
		  ([], fun i -> i == 1)
		 |]
let main() =
  Printf.printf "%s\n" "*****************   Model-checker de formules CTL  ********************";
  Printf.printf "Formula:\n%s\n" (formula_to_string f);
  let simple = simplify_formula f in
  Printf.printf "Simplified:\n%s\n" (formula_to_string simple);
  let optimized = optimize_formula simple in
  Printf.printf "Opitmized:\n%s\n" (formula_to_string optimized);
  assert (next_prop_value test_EX_g 0 0);
  Printf.printf "%s\n" "Unit test for EX function has been successful."

let () = main()
