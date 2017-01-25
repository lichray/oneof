#include "doctest.h"

#include <stdex/oneof.h>

#include <array>
#include <string>

TEST_CASE("large object")
{
	using arr_t = std::array<char, 100>;

	arr_t a = { 'a', 'k', 'b', '4', '8', '\0' };
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
