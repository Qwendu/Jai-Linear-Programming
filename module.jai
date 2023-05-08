
Index :: #type s32;

Constraint :: struct
{
	// factors dot variables <= constant
	factors  : [] float;
	constant :    float;
}


Program :: struct
{
	min_or_max : enum { MAX; MIN;} = .MAX;
	variables   : [] string;
	constraints : [] Constraint;
	value : [] float;
}

solve :: (program : *Program) -> (assignments : [] float)
{

	return .[];
}


#import "Basic";




print_Program :: (builder : *String_Builder, any : Any, data : *void) -> bool
{
	if any.type == type_info(Program)
	{
		// 
		// Print Style is chosen to enable copy paste to:
		// https://online-optimizer.appspot.com/
		//
		prog := cast(*Program)any.value_pointer;
		for prog.variables 
		{
			append(builder, "var ");
			append(builder, it);
			append(builder, ";\n");
		}
		append(builder, ifx prog.min_or_max == .MAX then "maximize " else "minimize ");
		append(builder, "value: ");
		for factor, variable_idx : prog.value
		{	
			if factor >= 0  append(builder, " ");
			if variable_idx != 0  append(builder, "+ ");
			print_to_builder(builder, "% * % ", float_Format(factor), prog.variables[variable_idx]);

		}
		append(builder, ";\n");
		float_Format :: #bake_arguments formatFloat(width = 6, zero_removal = .NO);
		for prog.constraints
		{
			print_to_builder(builder, "subject to constraint_%: ", it_index);
			for factor, variable_idx: it.factors 
			{
				if variable_idx != 0  append(builder, "+ ");
				if factor >= 0 append(builder, " "); // @Hack because formatFloat is bad?
				print_to_builder(builder, "% * % ", float_Format(factor), prog.variables[variable_idx]);
			}
			print_to_builder(builder, "<= %", it.constant);

			append(builder, ";\n");
		}
		append(builder, "end;\n");
		return true;
	}
	return false;
}
