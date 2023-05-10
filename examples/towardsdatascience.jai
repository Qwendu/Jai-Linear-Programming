LP :: #import,file "../module.jai";

//
// Example from:
// https://en.wikipedia.org/wiki/Simplex_algorithm#Exampl
//

// Soltion should be:
// Optimum of value 10000
// Solution for w = 40
// Solution for c = 40


main :: ()
{

	builder : LP.Program_Builder;
	w := LP.add_Variable(*builder, "w", min = 0);
	c := LP.add_Variable(*builder, "c", min = 0);
	
	LP.set_Objective(*builder, .MAX);
	LP.set_Contribution_Objective(*builder, w, 100);
	LP.set_Contribution_Objective(*builder, c, 150);

	c1 := LP.add_Constraint(*builder, name = "c1", leq_or_geq = .LEQ);
	LP.set_Constant_Constraint(*builder, c1, 50);
	LP.set_Contribution_Constraint(*builder, c1, w, 1);

	c2 := LP.add_Constraint(*builder, name = "c2", leq_or_geq = .LEQ);
	LP.set_Constant_Constraint(*builder, c2, 40);
	LP.set_Contribution_Constraint(*builder, c2, c, 1);

	c3 := LP.add_Constraint(*builder, name = "c3", leq_or_geq = .LEQ);
	LP.set_Constant_Constraint(*builder, c3, 120);
	LP.set_Contribution_Constraint(*builder, c3, w, 2);
	LP.set_Contribution_Constraint(*builder, c3, c, 1);

	push_printer(LP.print_Program);
	print("%\n", builder);
	standard := LP.standard_Form(*builder);
	print("Standard Form\n%\n", standard);
	tab := LP.generate_Tableau(*standard);
	print("Tableau\n%\n", <<tab);
	LP.simplex(tab);
	print("Tableau After Solving\n%\n", <<tab);
}


#run main();


push_printer :: ($f : (*String_Builder, Any, *void) -> bool, d : *void = null) #expand
{
	#run assert(f != null, "WHY is the struct_printer function null, thats not how we succeed in the games industry!\n");

	Closure :: struct
	{
		f : (*String_Builder, Any, *void) -> bool;
		d : *void;
	};

	Chain :: struct
	{
		prev, current : Closure;
	}

	chainer :: (builder: *String_Builder, any: Any, data : *void) -> bool
	{
		chain := cast(*Chain)data;
		if chain.current.f(builder, any, chain.current.d) return true;
		if chain.prev.f  return chain.prev.f(builder, any, chain.prev.d);
		return false;
	}
	
	previous := Closure.{context.print_style.struct_printer, context.print_style.struct_printer_data};
	current  := Closure.{f, d};
	chain := Chain.{prev = previous, current = current};

	context.print_style.struct_printer = chainer;
	`defer context.print_style.struct_printer = previous.f;
	context.print_style.struct_printer_data = *chain;
	`defer context.print_style.struct_printer_data = previous.d;
}

#import "Basic";
