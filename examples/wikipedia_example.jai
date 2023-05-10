LP :: #import,file "../module.jai";

//
// From:
// https://en.wikipedia.org/wiki/Simplex_algorithm#Example
//

//
// Solution should be:
// Optimum of value 20
// Solution for z = 5
//


main :: ()
{

	builder : LP.Program_Builder;
	x := LP.add_Variable(*builder, "x", min = 0);
	y := LP.add_Variable(*builder, "y", min = 0);
	z := LP.add_Variable(*builder, "z", min = 0);
	
	LP.set_Objective(*builder, .MIN);
	LP.set_Contribution_Objective(*builder, x, -2);
	LP.set_Contribution_Objective(*builder, y, -3);
	LP.set_Contribution_Objective(*builder, z, -4);

	c1 := LP.add_Constraint(*builder, name = "c1", leq_or_geq = .LEQ);
	LP.set_Constant_Constraint(*builder, c1, 10);
	LP.set_Contribution_Constraint(*builder, c1, x, 3);
	LP.set_Contribution_Constraint(*builder, c1, y, 2);
	LP.set_Contribution_Constraint(*builder, c1, z, 1);

	c2 := LP.add_Constraint(*builder, name = "c2", leq_or_geq = .LEQ);
	LP.set_Constant_Constraint(*builder, c2, 15);
	LP.set_Contribution_Constraint(*builder, c2, x, 2);
	LP.set_Contribution_Constraint(*builder, c2, y, 5);
	LP.set_Contribution_Constraint(*builder, c2, z, 3);

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
