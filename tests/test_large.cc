#include "doctest.h"

#include <stdex/oneof.h>

#include <array>
#include <string>

using arr_t = std::array<char, 100>;
arr_t const a = { 'a', 'k', 'b', '4', '8', '\0' };

TEST_CASE("large object access")
{
	stdex::oneof<int, arr_t> s;

	REQUIRE_FALSE(s.is<arr_t>());

	s = a;

	REQUIRE(s.is<arr_t>());
	REQUIRE(s.get<arr_t>().data() == std::string("akb48"));

	REQUIRE(s == s);
	REQUIRE_FALSE(s != s);
	REQUIRE_FALSE(s < s);
	REQUIRE_FALSE(s > s);
	REQUIRE(s <= s);
	REQUIRE(s >= s);

	auto p = s.maybe<arr_t>();

	REQUIRE(p);
	REQUIRE(*p == a);

	s.match(
	    [](auto&& a) {
		    static_assert(stdex::is_same_v<decltype(a), arr_t&>, "");
	    },
	    [](int) {});

	std::move(s).match(
	    [](auto&& a) {
		    static_assert(stdex::is_same_v<decltype(a), arr_t&&>, "");
	    },
	    [](int) {});

	decltype(s) const sc{};

	sc.match(
	    [](auto&& a) {
		    static_assert(stdex::is_same_v<decltype(a), arr_t const&>,
		                  "");
	    },
	    [](int) {});

	std::move(sc).match(
	    [](auto&& a) {
		    static_assert(stdex::is_same_v<decltype(a), arr_t const&&>,
		                  "");
	    },
	    [](int) {});
}

TEST_CASE("large object swapping")
{
	stdex::oneof<int, arr_t> s = a;
	static_assert(stdex::is_nothrow_swappable_v<decltype(s)>, "");

	decltype(s) s2 = arr_t{};
	decltype(s) s3 = 42;

	SUBCASE("copy")
	{
		auto tmp = s;
		REQUIRE(tmp == s);
		s = s2;
		s2 = tmp;

		REQUIRE(s.get<arr_t>()[0] == '\0');
		REQUIRE(s2.get<arr_t>() == a);
	}

	SUBCASE("move")
	{
		auto tmp = std::move(s);
		s = std::move(s2);
		s2 = std::move(tmp);

		REQUIRE(s.get<arr_t>()[0] == '\0');
		REQUIRE(s2.get<arr_t>() == a);
	}

	SUBCASE("swap")
	{
		using std::swap;
		swap(s, s2);

		REQUIRE(s.get<arr_t>()[0] == '\0');
		REQUIRE(s2.get<arr_t>() == a);
	}

	SUBCASE("overwrite")
	{
		auto tmp = s;
		s = s3;
		s3 = tmp;

		REQUIRE(s.get<int>() == 42);
		REQUIRE(s3.get<arr_t>() == a);
	}

	SUBCASE("replace")
	{
		auto tmp = std::move(s);
		s = std::move(s3);
		s3 = std::move(tmp);

		REQUIRE(s.get<int>() == 42);
		REQUIRE(s3.get<arr_t>() == a);
	}

	SUBCASE("exchange")
	{
		using std::swap;
		swap(s, s3);

		REQUIRE(s.get<int>() == 42);
		REQUIRE(s3.get<arr_t>() == a);
	}
}
