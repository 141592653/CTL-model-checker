open Formula
open Graph

let f = AX(
	    Equiv(
		EU(
		    AG(
			Prop("a")
		      ),
		    AU(
			EF(Prop("b")),Prop("c")
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
			Imply(Prop("a"),Prop("b"))
		      )
		  )
	      ))

let main() = 
  Printf.printf "%s\n" "*****************   Model-checker de formules CTL  ********************";
  ignore (simplify_formula f)

let () = main()
  
