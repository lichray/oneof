#include "doctest.h"

#include <stdex/oneof.h>

#include <string>

struct CopyOnly
{
	CopyOnly() : s("empty") {}

	CopyOnly(CopyOnly&&) = delete;
	CopyOnly& operator=(CopyOnly&&) = delete;

	CopyOnly(CopyOnly const&) : s("copied to") {}

	CopyOnly& operator=(CopyOnly const&)
	{
		s = "assigned to";
		return *this;
	}

	std::string s;
};

TEST_CASE("wrapper")
{
	stdex::detail::indirection<CopyOnly> x;
	REQUIRE(x.get().s == "empty");

	auto x2 = x;
	REQUIRE(x2.get().s == "copied to");
	REQUIRE(x.get().s == "empty");

	x = x2;
	REQUIRE(x.get().s == "assigned to");
	REQUIRE(x2.get().s == "copied to");

	x2 = decltype(x)();
	REQUIRE(x2.get().s == "empty");

	auto x3 = std::move(x);
	REQUIRE(x3.get().s == "assigned to");

	static_assert(stdex::is_nothrow_move_constructible_v<decltype(x)>, "");
	static_assert(stdex::is_nothrow_move_assignable_v<decltype(x)>, "");

	stdex::detail::indirection<std::string> y(4u, ' ');
	REQUIRE(y.get() == "    ");

	stdex::detail::indirection<std::string> z(std::string("pr"));
	REQUIRE(z.get() == "pr");
}

TEST_CASE("traits")
{
	struct Incomplete
	{
		static_assert(
		    std::is_same<
		        stdex::detail::variant_internal_t<Incomplete[]>,
		        stdex::detail::indirection<Incomplete>>::value,
		    "");
	};

	static_assert(
	    std::is_same<stdex::detail::variant_internal_t<int>, int>::value,
	    "");

	struct Fit
	{
		char s[stdex::detail::variant_bufsize - sizeof(int)];
	};

	static_assert(
	    std::is_same<stdex::detail::variant_internal_t<Fit>, Fit>::value,
	    "");

	struct alignas(double) UnFit
	{
		Fit x;
	};

	static_assert(std::is_same<stdex::detail::variant_internal_t<UnFit>,
	                           stdex::detail::indirection<UnFit>>::value,
	              "");
}
