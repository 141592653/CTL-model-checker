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
  Printf.printf "%s\n" (formula_to_string f);
  Printf.printf "%s\n" (formula_to_string (simplify_formula f));
  assert (next_prop_value test_EX_g 0 0);
  Printf.printf "%s\n" "Unit test for EX function has been successful."

let () = main()
