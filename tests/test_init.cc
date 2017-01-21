#include "doctest.h"

#include <stdex/oneof.h>

#include <string>

TEST_CASE("default init")
{
	stdex::oneof<int, std::string> s;
	(void)s;
}
