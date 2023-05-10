LP :: #import,file "../module.jai"(true);

// This example optimizes Thruster Allocation
// And calculates how well a configuration of thrusters
// can move a the direction
// ti_f is the force the i-th thruster can produce
// 

// direction is the direction we want to move in

// Solution:
//
// Optimum of value 1
// Solution for k = 1
// Solution for t_0 = 1
// Solved with value: 1




main :: ()
{
	direction := Vector2.{ 1, 0};
	thrusters :: Vector2.[
		.{ 1, 0},
		.{ 0, 1},
		.{-1, 0},
		.{ 0,-1},
	];
	thruster_vars : [thrusters.count] LP.Variable;
	
	
	builder : LP.Program_Builder;
	k  := LP.add_Variable(*builder, "k", min = 0);
	for thrusters  thruster_vars[it_index] = LP.add_Variable(*builder, tprint("t_%", it_index), min = 0, max = 1);

	LP.set_Objective(*builder, .MAX);
	LP.set_Contribution_Objective(*builder, k, 1);

	force_x  := LP.add_Constraint(*builder, name = "force_x", leq_or_geq = .EQ);
	force_y  := LP.add_Constraint(*builder, name = "force_y", leq_or_geq = .EQ);


	LP.set_Constant\   _Constraint(*builder, force_x, 0);
	LP.set_Contribution_Constraint(*builder, force_x, k, -direction.x);
	for thrusters  LP.set_Contribution_Constraint(*builder, force_x, thruster_vars[it_index],   it.x);

	LP.set_Constant\   _Constraint(*builder, force_y, 0);
	LP.set_Contribution_Constraint(*builder, force_y, k, -direction.y);
	for thrusters  LP.set_Contribution_Constraint(*builder, force_y, thruster_vars[it_index],   it.y);

	push_printer(LP.print_Program);
	print("%\n", builder);
	standard := LP.standard_Form(*builder);
	print("Standard Form\n%\n", standard);
	tab := LP.generate_Tableau(*standard);
	print("Tableau\n%\n", <<tab);
	val, solution := LP.simplex(tab);
	print("Solved with value: %\n", val);
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
#import "Math";
