LP :: #import,file "module.jai";



main :: ()
{
	program := LP.Program.
	{
		variables = .[
			"k",
			"t1",
			"t2"
		],
		constraints = .[
			.{.[ 0.5, -1,  0], 0},
			.{.[ 0.2,  0, -2], 0},
			.{.[-0.5,  1,  0], 0},
			.{.[-0.2,  0, -2], 0},
			.{.[ 0  , -1,  0], 0},
			.{.[ 0  ,  0, -1], 0},
			.{.[ 0  ,  1,  0], 1},
			.{.[ 0  ,  0,  1], 1},
			.{.[-1  ,  0,  0], 0},
		],
		value = .[1, 0, 0]
	};

	value := LP.solve(*program);
	
	push_printer(LP.print_Program);
	print("%\n", program);
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
