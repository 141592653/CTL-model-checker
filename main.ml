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

let main() =
  Printf.printf "%s\n" "*****************   Model-checker de formules CTL  ********************";
  ignore (simplify_formula f);
  Printf.printf "%s\n" (formula_to_string f)

let () = main()
