#include "doctest.h"

#include <stdex/oneof.h>

#include <vector>

TEST_CASE("value semantics")
{
	stdex::oneof<int, std::vector<int>, double> s;
	auto v = s.emplace<std::vector<int>>(3u, 42);

	decltype(s) s2;
	s2 = std::move(s);

	REQUIRE(s2.get<std::vector<int>>().size() == 3);

	s.emplace<std::vector<int>>();
	s = std::move(s2);

	REQUIRE(s.get<std::vector<int>>() == v);

	s2.emplace<double>(3.14);
	s = std::move(s2);

	REQUIRE(s.get<double>() == 3.14);

	s2 = s;

	REQUIRE(s2.get<double>() == 3.14);
	REQUIRE(s.get<double>() == 3.14);

	auto s3 = s;
	s.emplace<std::vector<int>>(2u, 42);

	REQUIRE(s.get<std::vector<int>>().size() == 2);
	REQUIRE(s3.get<double>() == 3.14);

	s3 = s;
	REQUIRE(s3.get<std::vector<int>>() == s.get<std::vector<int>>());

	s = s;
	REQUIRE(s3.get<std::vector<int>>() == s.get<std::vector<int>>());

	using std::swap;
	s2.emplace<double>(0.5);
	swap(s, s2);

	REQUIRE(s.get<double>() == 0.5);
	REQUIRE(s3.get<std::vector<int>>().size() == 2);
}
