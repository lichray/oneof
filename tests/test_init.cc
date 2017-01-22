#include "doctest.h"

#include <stdex/oneof.h>

#include <string>

TEST_CASE("default init")
{
	stdex::oneof<int, std::string> s;
	(void)s;
}

TEST_CASE("static features")
{
	using A = stdex::oneof<int, std::string>;

	static_assert(stdex::is_default_constructible_v<A>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<A>, "");
	static_assert(not stdex::is_trivially_destructible_v<A>, "");

	using B = stdex::oneof<int, double, long>;

	static_assert(stdex::is_default_constructible_v<B>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<B>, "");
	static_assert(stdex::is_trivially_destructible_v<B>, "");

	struct HardDefault
	{
		HardDefault() {}
	};

	using C = stdex::oneof<HardDefault, long>;

	static_assert(stdex::is_default_constructible_v<C>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<C>, "");
	static_assert(stdex::is_trivially_destructible_v<C>, "");

	using D = stdex::oneof<stdex::oneof<>, long>;

	static_assert(not stdex::is_default_constructible_v<D>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<D>, "");
	static_assert(stdex::is_trivially_destructible_v<D>, "");

	using E = stdex::oneof<stdex::oneof<>, std::string>;

	static_assert(not stdex::is_default_constructible_v<E>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<E>, "");
	static_assert(not stdex::is_trivially_destructible_v<E>, "");
}
