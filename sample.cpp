#include <iostream>
#include "math_parser.h"


int main()
{
	using MathParser::CompiledExpression;
	using MathParser::Vars;
	using MathParser::Funcs;



	CompiledExpression expr1(
		"2x + 3",
		Vars{{"x", 4}}
	);


	CompiledExpression expr2(
	"a + b + c + d + hello",
	Vars{{"a", 1},{"b", 1},{"c", 1},{"d", 1},{"hello", 1},}
	);


	CompiledExpression expr3(
		"f(x) + y",
		Funcs{"f(x)=x^2"},
		Vars{{"x", 3}, {"y", 1}}
	);

	// No args
	CompiledExpression expr4(
		"sin(pi/2) + max(2, 5)"
	);


	// Composition functions
	CompiledExpression expr5(
		"f(g(x))",
		Vars{}
	);

	expr5.def({"f(x)=x^(4)", "g(x)=x+1"});
	expr5.set_vars("x", 2);
	expr5.set_vars({{"x", 2}}); // Also works


	// Observe the implicit multiplication/args without * or ()
	std::string reallyLongExpression =
		"sin x^2 + cos(2x + 1)ln(x^2 + 1) "
		"- tan(x/3) + sqrt(x^2 + 4x + 4) / (1 + exp(-x))"
		"+ abs(sin 3x - cos(x))";

	CompiledExpression expr6(reallyLongExpression, Vars{{"x", 1}}
	);


	std::cout << expr1.value() << "\n";
	std::cout << expr2.value() << "\n";
	std::cout << expr3.value() << "\n";
	std::cout << expr4.value() << "\n";
	std::cout << expr5.value() << "\n";
	std::cout << expr6.value() << "\n";

	return 0;



}
