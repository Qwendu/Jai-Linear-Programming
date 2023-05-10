LP :: #import,file "../module.jai";

// This example optimizes Thruster Allocation for the setup
// t1 := Vector2.{1, 0}
// t2 := Vector2.{0, 1}
// t3 := Vector2.{0.3, 0.2}
// 
// And calculates how well it can move in the direction
// k:= Vector2.{1, 0.2}

// Solution:
//
// Optimum of value 1.3
// Solution for k = 1.3
// Solution for t1 = 1
// Solution for t2 = 0.06
// Solution for t3 = 1




main :: ()
{

	builder : LP.Program_Builder;
	k  := LP.add_Variable(*builder, "k", min = 0);
	t1 := LP.add_Variable(*builder, "t1", min = 0, max = 1);
	t2 := LP.add_Variable(*builder, "t2", min = 0, max = 1);
	t3 := LP.add_Variable(*builder, "t3", min = 0, max = 1);

	LP.set_Objective(*builder, .MAX);
	LP.set_Contribution_Objective(*builder, k, 1);
	LP.set_Contribution_Objective(*builder, t1, 0);

	force_x  := LP.add_Constraint(*builder, name = "force_x", leq_or_geq = .EQ);
	force_y  := LP.add_Constraint(*builder, name = "force_y", leq_or_geq = .EQ);


	LP.set_Constant\   _Constraint(*builder, force_x, 0);
	LP.set_Contribution_Constraint(*builder, force_x, k,   1);
	LP.set_Contribution_Constraint(*builder, force_x, t1, -1);
	LP.set_Contribution_Constraint(*builder, force_x, t3, -0.3);

	LP.set_Constant\   _Constraint(*builder, force_y, 0);
	LP.set_Contribution_Constraint(*builder, force_y, k,   0.2);
	LP.set_Contribution_Constraint(*builder, force_y, t2, -1.0);
	LP.set_Contribution_Constraint(*builder, force_y, t3, -0.2);

	push_printer(LP.print_Program);
	print("%\n", builder);
	standard := LP.standard_Form(*builder);
	print("Standard Form\n%\n", standard);
	tab := LP.generate_Tableau(*standard);
	print("Tableau\n%\n", <<tab);
	LP.simplex(tab);
	print("Solved \n%\n", <<tab);
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

