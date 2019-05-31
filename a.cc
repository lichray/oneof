#include "stdex/oneof.h"
#include <iostream>

int main()
{
	stdex::oneof<int, double> x = 1;
	std::cout << x.which() << '\n';
}
