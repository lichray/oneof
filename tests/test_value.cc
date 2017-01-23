#include "doctest.h"

#include <stdex/oneof.h>

#include <vector>
#include <string>

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

TEST_CASE("conversions")
{
	using T1 = stdex::oneof<int, std::string, bool>;

	{
		T1 s = false;
		T1 v(false);
		REQUIRE(s.is<bool>());
		REQUIRE(v.is<bool>());
	}

	{
		T1 s = 3;
		T1 v(3);
		REQUIRE(s.is<int>());
		REQUIRE(v.is<int>());
	}

	{
		T1 s = std::string();
		T1 v{ std::string() };
		REQUIRE(s.is<std::string>());
		REQUIRE(v.is<std::string>());
	}

	{
		T1 s("");
		REQUIRE(s.is<std::string>());
	}

	{
		T1 s('x');
		REQUIRE(s.is<int>());
	}

	using T2 = stdex::oneof<bool, double, std::string>;

	{
		T2 s = false;
		T2 v(false);
		REQUIRE(s.is<bool>());
		REQUIRE(v.is<bool>());
	}

	{
		T2 s = 3.14;
		T2 v(3.);
		REQUIRE(s.is<double>());
		REQUIRE(v.is<double>());
	}

	{
		std::string const x = "lvalue";
		T2 s = x;
		T2 v(x);
		REQUIRE(s.is<std::string>());
		REQUIRE(v.is<std::string>());
		REQUIRE(s.get<std::string>() == v.get<std::string>());
	}

	{
		T2 s("pointer");
		REQUIRE(s.is<bool>());
	}

	{
		T2 s(0);
		REQUIRE(s.is<bool>());
	}

	{
		T2 s(0.f);
		REQUIRE(s.is<bool>());
	}
}
