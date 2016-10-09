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

let test_EX_f_success = EX(Prop 0)
let test_EX_f_failure = EX(Prop 2)
let test_EX_g =
    [|
        ([1;2],fun i -> i == 1);
        ([], fun i -> i == 0 || i == 1);
        ([], fun i -> i == 1)
    |]

let test_EG_f_success = EG(Prop 0)
let test_EG_f_failure = EG(Prop 1)
let test_EG_g =
    [|
        ([0;1], fun i -> i == 0);
        ([], fun i -> i == 1);
    |]

let test_EU_f_success = EU(Prop 0, Prop 1)
let test_EU_f_failure = EU(Prop 1, Prop 2)
let test_EU_g =
    [|
        ([0;1], fun i -> i == 0);
        ([2], fun i -> i == 0);
        ([], fun i -> i == 1);
    |]

let main() =
  Printf.printf "%s\n" "*****************   Model-checker de formules CTL  ********************";
  Printf.printf "Formula:\n%s\n" (formula_to_string f);
  let simple = simplify_formula f in
  Printf.printf "Simplified:\n%s\n" (formula_to_string simple);
  let optimized = optimize_formula simple in
  Printf.printf "Opitmized:\n%s\n" (formula_to_string optimized);
  Printf.printf "Starting unit tests\n";
  assert (evaluate test_EX_g 0 test_EX_f_success);
  assert (not (evaluate test_EX_g 0 test_EX_f_failure));
  Printf.printf "%s\n" "Unit tests for EX function have been successful.";
  assert (evaluate test_EG_g 0 test_EG_f_success);
  assert (not (evaluate test_EG_g 0 test_EG_f_failure));
  Printf.printf "%s\n" "Unit tests for EG function have been successful.";
  assert (evaluate test_EU_g 0 test_EU_f_success);
  assert (not (evaluate test_EU_g 0 test_EU_f_failure));
  Printf.printf "%s\n" "Unit tests for EU function have been successful.";
  Printf.printf "f is %s on test_EX_g\n"
    (if evaluate test_EX_g 0 optimized then "true" else "false")

let () = main()
