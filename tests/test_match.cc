#include "doctest.h"

#include <stdex/oneof.h>

#include <string>
#include <memory>

TEST_CASE("match")
{
	stdex::oneof<int, double, std::string, char> s;
	s.emplace<char>('e');

	auto f = [](double) { REQUIRE(0); };
	auto const cf = [](int) { REQUIRE(0); };

	const_cast<decltype(s) const &>(s).match(
	    f, cf, [](char c) { REQUIRE(c == 'e'); },
	    [](std::string const&) { REQUIRE(0); });

	s.emplace<std::string>("nice");

	auto i = s.match(
	    [](std::string& s) {
		    s = "boat";
		    return 1;
	    },
	    [](...) { return 0; });

	REQUIRE(i);
	REQUIRE(s.get<std::string>() == "boat");
}

TEST_CASE("match move-only")
{
	stdex::oneof<std::unique_ptr<int>, double> s =
	    std::make_unique<int>(42);
	std::unique_ptr<int> p;

	std::move(s).match(
	    [&](std::unique_ptr<int>&& ip) { p = std::move(ip); },
	    [](double) {});

	REQUIRE(p);
	REQUIRE(*p == 42);
}
