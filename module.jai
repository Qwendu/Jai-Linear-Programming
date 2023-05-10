#module_parameters(PRINT_SIMPLEX : bool = false);


Variable    :: #type s32;
Constraint  :: #type s32;




Program_Builder :: struct
{
	Factor_Of_Variable :: struct
	{
		var   : Variable; @Variablees(Program_Builder.variables);
		value : float;
	};

	Constraint :: struct
	{
		name       : string;
		leq_or_geq : enum {LEQ; GEQ; EQ;} = .LEQ;
		factors    : [..] Factor_Of_Variable;
		constant   : float;
	};

	min_or_max     : enum {MIN; MAX;} = .MAX;
	variables      : [..] string;
	lower_bounds   : [..] Factor_Of_Variable;
	upper_bounds   : [..] Factor_Of_Variable;
	constraints    : [..] Constraint;
	value_function : [..] Factor_Of_Variable;
}


Tableau :: struct
{
	#if PRINT_SIMPLEX
	{
		variable_names : [] string;
	}
	Rows : int;
	Cols : int;
	data : [] float; @"assert(data.count == Rows * Count)";
}



add_Variable :: (builder : *Program_Builder, variable_name : string, min := -FLOAT32_INFINITY, max := FLOAT32_INFINITY) -> Variable
{
	idx : Variable = xx builder.variables.count;
	array_add(*builder.variables, variable_name);
	array_add(*builder.lower_bounds, .{idx, min});
	array_add(*builder.upper_bounds, .{idx, max});
	return idx;
}

lower_Bound :: (builder : *Program_Builder, var : Variable) -> float
{
	for builder.lower_bounds  if it.var == var  return it.value;
	return -FLOAT32_INFINITY;
}

upper_Bound :: (builder : *Program_Builder, var : Variable) -> float
{
	for builder.upper_bounds  if it.var == var  return it.value;
	return -FLOAT32_INFINITY;
}

set_Objective :: (builder : *Program_Builder, obj : type_of(builder.min_or_max))
{
	builder.min_or_max = obj;
}

get_Or_Add :: (arr : *[..]Program_Builder.Factor_Of_Variable, var : Variable) -> int
{
	index := -1;
	for <<arr  if it.var == var  index = it_index;
	if index == -1
	{
		index = arr.count;
		array_add(arr, .{var, 0});
	}
	return index;
}

add_Constraint :: (builder : *Program_Builder, leq_or_geq : type_of(Program_Builder.Constraint.leq_or_geq) = .LEQ, name : string = "") -> Constraint
{
	idx : Constraint = xx builder.constraints.count;
	array_add(*builder.constraints, .{name = name, leq_or_geq = leq_or_geq});
	return idx;
}

set_Contribution_Objective :: (builder : *Program_Builder, var : Variable, factor : float)
{
	builder.value_function[get_Or_Add(*builder.value_function, var)].value = factor;
}

set_Constant_Constraint :: (builder : *Program_Builder, constraint : Constraint, cons : float)
{
	assert(constraint >= 0 && constraint < builder.constraints.count, "You need to add a constraint using add_Constraint() before you can set its factors!");
	builder.constraints[constraint].constant = cons;
}

set_Contribution_Constraint :: (builder : *Program_Builder, constraint : Constraint, var : Variable, factor : float)
{
	assert(constraint >= 0 && constraint < builder.constraints.count, "You need to add a constraint using add_Constraint() before you can set its factors!");
	idx := get_Or_Add(*builder.constraints[constraint].factors, var);
	builder.constraints[constraint].factors[idx].value = factor;
}


standard_Form :: (builder : *Program_Builder) -> Program_Builder
{
	new := Program_Builder.{};
	array_copy(*new.variables, builder.variables);

	array_copy(*new.upper_bounds, builder.upper_bounds); 	
	array_copy(*new.lower_bounds, builder.lower_bounds); 	
	array_copy(*new.value_function, builder.value_function);

	if builder.min_or_max == .MIN
	{
		for * new.value_function
		{
			it.value *= -1;
		}
	}

	for builder.lower_bounds
	{
		if abs(it.value) > EPS
		{
			slack := add_Variable(*new, "", min = 0);
			cons := add_Constraint(*new, name = "", leq_or_geq = .EQ);
			set_Constant\   _Constraint(*new, cons, it.value);
			set_Contribution_Constraint(*new, cons, it.var, 1);
			set_Contribution_Constraint(*new, cons, slack, -1);
		}
	}

	for * new.lower_bounds
	{
		it.value = 0;
	}

	// We dont need the upper bounds, they are constraints!
	// But we keep them anyways to make it more clear
	for builder.upper_bounds
	{
		if it.value == FLOAT32_INFINITY  continue;
		slack := add_Variable(*new, "", min = 0);
		cons  := add_Constraint(*new, name = "", leq_or_geq = .EQ);
		set_Constant\   _Constraint(*new, cons, it.value);
		set_Contribution_Constraint(*new, cons, it.var, 1);
		set_Contribution_Constraint(*new, cons, slack,  1);

	}

	
	for builder.constraints
	{
		c := Program_Builder.Constraint.{name = it.name, constant = it.constant};
		c.leq_or_geq = .EQ; // Standard Form only has equalities
		if it.leq_or_geq == .EQ
		{
			array_copy(*c.factors, it.factors);
		}else if it.leq_or_geq == .LEQ
		{
			slack_variable := add_Variable(*new, "", min = 0);
			array_copy(*c.factors, it.factors);
			array_add(*c.factors, .{slack_variable, 1});
		}else if it.leq_or_geq == .GEQ
		{
			slack_variable := add_Variable(*new, "", min = 0);
			array_copy(*c.factors, it.factors);
			array_add(*c.factors, .{slack_variable, -1});
		}

		array_add(*new.constraints, c);
	}
	
	return new;
}



generate_Tableau :: (builder : *Program_Builder) -> *Tableau
{
	// @Robustness Check that the builder is in Standard Form

	ROWS_FOR_OBJECTIVE_FUNCTION :: 1;
	COLS_FOR_CONSTRAINT_CONSTANTS :: 1;

	rows := builder.constraints.count + ROWS_FOR_OBJECTIVE_FUNCTION;
	cols := builder.variables  .count + COLS_FOR_CONSTRAINT_CONSTANTS;

	mem : *u8 = alloc(size_of(Tableau) + size_of(float) * cols * rows);
	tab := cast(*Tableau) mem;

	<<tab = .{};
	#if PRINT_SIMPLEX
	{
		tab.variable_names = builder.variables;
	}
	tab.Rows = rows;
	tab.Cols = cols;
	tab.data.data = cast(*float)(mem + size_of(Tableau));
	tab.data.count = tab.Rows * tab.Cols;
	memset(tab.data.data, 0, size_of(float) * tab.data.count);

	{
		row := 0;
		for term: builder.value_function
		{
			col := term.var;
			invert := ifx builder.min_or_max == .MIN then -1 else 0;
			tab.data[row * tab.Cols + col] = term.value;

		}
	}
	for cons, cons_idx : builder.constraints
	{
		row := cons_idx + 1;
		for term: cons.factors
		{
			col := term.var;
			tab.data[row * tab.Cols + col] = term.value;
		}
		tab.data[row * tab.Cols + tab.Cols - 1] = cons.constant;
	}
	return tab;
}


canonicalize :: (tab : *Tableau)
{
	for 0..tab.Cols-1
	{
		tab.data[it] *= -1;
	}
}

simplex :: (tab : *Tableau)
{
	#if PRINT_SIMPLEX
	{
		builder : String_Builder;
		append(*builder, "Simplex trace\n\n");
		
		print_to_builder(*builder, "Starting with Tableau\n%\n", <<tab);
	}
	canonicalize(tab);
	#if PRINT_SIMPLEX print_to_builder(*builder, "Tableau after canonicalizing\n%\n", <<tab);
	pivot :: (row : int, col : int) #expand
	{

		value_at_pivot := tab.data[row * tab.Cols + col];
		#if PRINT_SIMPLEX print_to_builder(*builder, "Pivot % %  = %\n", row, col, value_at_pivot);
		assert(abs(value_at_pivot) > EPS);

		#if PRINT_SIMPLEX print_to_builder(*builder, "\n\nNormalizing row % with factor %\n", row, 1/value_at_pivot);
		//
		// Normalize Pivot Row
		//
		for col_idx : 0..tab.Cols-1
		{
			val := tab.data[row * tab.Cols + col_idx];

			tab.data[row * tab.Cols + col_idx] /= value_at_pivot;
			#if PRINT_SIMPLEX print_to_builder(*builder, "column %:  % / % = %\n", col_idx, val, value_at_pivot, tab.data[row * tab.Cols + col_idx]);
		}

		#if PRINT_SIMPLEX  print_to_builder(*builder, "Tableau after normalization\n%\n", <<tab);


		for row_idx : 0..tab.Rows-1
		{

			if row_idx == row  continue;
			factor := tab.data[row_idx * tab.Cols + col];
			#if PRINT_SIMPLEX  print_to_builder(*builder, "\nrow % -= % * row %\n", row_idx, factor, row);
			for col_idx : 0..tab.Cols-1
			{
				current_row_col_value := tab.data[row_idx * tab.Cols + col_idx];
				current_row_pivot_col_value := tab.data[row * tab.Cols + col_idx];
				tab.data[row_idx * tab.Cols + col_idx] -= tab.data[row * tab.Cols + col_idx] * factor;
				//#if PRINT_SIMPLEX  print_to_builder(*builder, "% = % - % * %\n", tab.data[row_idx * tab.Cols + col_idx], current_row_col_value, current_row_pivot_col_value, factor);
			}
			tab.data[row_idx * tab.Cols + col] = 0;
		}
		tab.data[row * tab.Cols + col] = 1;
	}

	select_Pivot :: () -> (row : int, col : int, done : bool) #expand
	{
		//
		// Go through the objective row and select the most negative value
		//
		most_negative_col := -1;
		for col_idx: 0..tab.Cols-2 // -2 to skip RHS column
		{
			val := tab.data[0 * tab.Cols + col_idx];
			if val < 0
			{
				if most_negative_col == -1 
				{
					most_negative_col = col_idx;
				}else{
					if val < tab.data[0 * tab.Cols + most_negative_col]  most_negative_col = col_idx;
				}
			}
		}
		if most_negative_col == -1  return -1, -1, true;

		lowest_row := -1;
		lowest     := FLOAT32_INFINITY;
		for row_idx : 1..tab.Rows-1 // starting at 1 to skip objective function
		{
			denominator := tab.data[row_idx * tab.Cols + most_negative_col];
			if denominator < 0  continue;
			if abs(denominator) < EPS  continue;

			val := tab.data[row_idx * tab.Cols + tab.Cols - 1]/denominator;
			if val < lowest
			{
				lowest_row = row_idx;
				lowest = abs(val);
			}
		}
		return lowest_row, most_negative_col, false;
	}

	while true
	{
		#if PRINT_SIMPLEX print_to_builder(*builder, "%\n", <<tab);

		pivot_row, pivot_col, done := select_Pivot();
		#if PRINT_SIMPLEX print_to_builder(*builder, "% % %\n", pivot_row, pivot_col, done);
		if done  break;
		pivot(pivot_row, pivot_col);
	}


	//
	// Extract solution
	//
	optimim_value := tab.data[0 * tab.Cols + tab.Cols - 1];
	#if PRINT_SIMPLEX print_to_builder(*builder, "Optimum of value %\n", optimim_value);
	for col: 0..tab.Cols-2
	{
		number_non_zeroes := 0;
		number_ones       := 0;
		row_of_the_one  := -1;
		for row: 0..tab.Rows-1
		{
			if abs(tab.data[row * tab.Cols + col])     < EPS   number_non_zeroes += 1;
			if abs(tab.data[row * tab.Cols + col] - 1) < EPS  {number_ones       += 1; row_of_the_one  = row;}
		}
		if number_ones == 1 && number_non_zeroes == tab.Rows - 1 // This is a basic column -> solution
		{
			value := tab.data[row_of_the_one * tab.Cols + tab.Cols - 1];
			#if PRINT_SIMPLEX 
			{
				if tab.variable_names[col]  print_to_builder(*builder, "Solution for % = %\n", tab.variable_names[col], value);
			}
		}
	}
	
	#if PRINT_SIMPLEX print("%", builder_to_string(*builder));
}





print_Program :: (builder : *String_Builder, any : Any, data : *void) -> bool
{
	float_Format :: #bake_arguments formatFloat(width = 6, zero_removal = .NO);
	if any.type == type_info(Program_Builder)
	{
		// 
		// Print Style is chosen to enable copy paste to:
		// https://online-optimizer.appspot.com/
		//
		prog := cast(*Program_Builder)any.value_pointer;
		append(builder, "\n");
	
		for prog.variables
		{
			append(builder, "var ");
			if it == ""
			{
				append(builder, "slack_");
				print_to_builder(builder, "%", it_index);
			}else{
				append(builder, it);
			}
			lower := lower_Bound(prog, xx it_index);
			if lower != -FLOAT32_INFINITY  print_to_builder(builder, ", >= %", float_Format(lower));
			upper := upper_Bound(prog, xx it_index);
			if upper != FLOAT32_INFINITY  print_to_builder(builder, ", <= %", float_Format(upper));
			append(builder, ";\n");
		}
		append(builder, "\n");
		if prog.min_or_max == .MAX append(builder, "maximize ");
		else                       append(builder, "minimize ");
		append(builder, "objective: ");
		for prog.value_function
		{
			if it_index != 0  append(builder, " + ");
			print_to_builder(builder, "% * %", float_Format(it.value), prog.variables[it.var]);
		}
		append(builder, ";\n");

		append(builder, "\n");
		for prog.constraints
		{
			if it.factors.count == 0  continue;
			append(builder, "subject to ");
			if it.name == ""
			{
				append(builder, "constraint_");
				print_to_builder(builder, "%", it_index);
			}else{
				append(builder, it.name);
			}
			append(builder, ": ");
			
			for factor, factor_idx : it.factors 
			{
				if factor_idx != 0  append(builder, " + ");
				print_to_builder(builder, "% * ", float_Format(factor.value));
				if prog.variables[factor.var] == ""
				{
					print_to_builder(builder, "slack_%", factor.var);
				}else{
					append(builder, prog.variables[factor.var]);
				}
			}
			if it.leq_or_geq == .LEQ     append(builder, " <= ");
			else if it.leq_or_geq == .EQ append(builder, "  = ");
			else                         append(builder, " >= ");
			
			print_to_builder(builder, "%", float_Format(it.constant));
			append(builder, ";\n");
		}
		append(builder, "end;\n");
		
		return true;
	}
	if any.type == type_info(Tableau)
	{
		tab := cast(*Tableau) any.value_pointer;
		print_to_builder(builder, "%x%\n",tab.Rows, tab.Cols);
		for row : 0..tab.Rows-1
		{
			for col : 0..tab.Cols-1
			{
				val := tab.data[row * tab.Cols + col];
				if <<cast(*u32)*val != 0x8000_0000 && val >= 0 append(builder, " ");
				print_to_builder(builder, "% ", float_Format(val));
			}
			append(builder, "\n");
		}
		append(builder, "\n");
		return true;
	}
	return false;
}

EPS :: 0.00001;
#import "Math";
#import "Basic";
