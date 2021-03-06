#include "doctest.h"

#include <stdex/oneof.h>

#include <vector>
#include <string>
#include <limits>

TEST_CASE("values")
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

TEST_CASE("value semantics")
{
	stdex::oneof<int, std::string, double> s = 4.0;
	auto s2 = s;

	REQUIRE(s2 == s);
	REQUIRE_FALSE(s2 != s);

	REQUIRE(s2 <= s);
	REQUIRE(s2 >= s);
	REQUIRE_FALSE(s2 < s);
	REQUIRE_FALSE(s2 > s);

	s2 = 5.0;

	REQUIRE(s2 != s);
	REQUIRE_FALSE(s2 == s);

	REQUIRE(s2 > s);
	REQUIRE(s2 >= s);
	REQUIRE_FALSE(s2 < s);
	REQUIRE_FALSE(s2 <= s);

	s2 = s;

	REQUIRE(s2 == s);
	REQUIRE_FALSE(s2 != s);

	s2 = 4;

	REQUIRE(s2 != s);
	REQUIRE_FALSE(s2 == s);

	REQUIRE(s2 < s);
	REQUIRE(s2 <= s);
	REQUIRE_FALSE(s2 > s);
	REQUIRE_FALSE(s2 >= s);

	using std::swap;
	swap(s, s2);

	REQUIRE(s2 != s);
	REQUIRE_FALSE(s2 == s);

	s = std::numeric_limits<double>::quiet_NaN();

	CHECK(s.is<double>());
	CHECK(s2.is<double>());
	CHECK_FALSE(s2.get<double>() < s.get<double>());
	CHECK_FALSE(s2.get<double>() >= s.get<double>());
	REQUIRE_FALSE(s2 < s);
	REQUIRE_FALSE(s2 >= s);
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
		T1 s = "";
		REQUIRE(s.is<std::string>());
	}

	{
		T1 s = 'x';
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
		T2 s = "pointer";
		REQUIRE(s.is<std::string>());
	}

	static_assert(not stdex::is_constructible_v<T2, int>, "");
	static_assert(not stdex::is_convertible_v<int, T2>, "");
	static_assert(not std::is_assignable<T2, int>::value, "");

	{
		T2 s = 0.f;
		REQUIRE(s.is<double>());
	}
}

TEST_CASE("convert assignment")
{
	stdex::oneof<int, std::string, bool> s;
	REQUIRE(s.which() == 0);

	s = true;
	REQUIRE(s.which() == 2);

	std::string v = "lvalue";
	s = v;
	REQUIRE(s.get<std::string>() == v);

	stdex::oneof<int, bool, char const*> x = "string";
	REQUIRE(x.which() == 2);

	x = short(1);
	REQUIRE(x.which() == 0);

	x = "literal";
	REQUIRE(x.which() == 2);

	x = nullptr;
	REQUIRE(x.which() == 2);
}

TEST_CASE("get pointer")
{
	stdex::oneof<int, std::string> s("str");

	if (auto p = s.maybe<int>())
		REQUIRE(0);
	else
		REQUIRE(!p);

	if (auto p = s.maybe<std::string>())
		REQUIRE(*p == "str");
	else
		REQUIRE(0);

	stdex::oneof<int, long> const x = 3;

	if (auto p = x.maybe<long>())
	{
		REQUIRE(0);
		static_assert(stdex::is_same_v<decltype(*p), long const&>, "");
	}
	else
		REQUIRE(x.maybe<int>());
}
