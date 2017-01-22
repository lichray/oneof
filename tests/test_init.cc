#include "doctest.h"

#include <stdex/oneof.h>

#include <string>

TEST_CASE("default init")
{
	stdex::oneof<int, std::string> s;
	(void)s;
}

struct HardDefaultNothrow
{
	HardDefaultNothrow() noexcept {}
};

struct HardDefault
{
	HardDefault() {}
};

struct HardDefaultUDestruct
{
	HardDefaultUDestruct() {}
	~HardDefaultUDestruct() {}
};

struct NoDefaultUDestruct
{
	NoDefaultUDestruct() = delete;
	~NoDefaultUDestruct() {}
};

TEST_CASE("static features")
{
	using A = stdex::oneof<int, std::string>;

	static_assert(stdex::is_default_constructible_v<A>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<A>, "");
	static_assert(not stdex::is_trivially_destructible_v<A>, "");

	using B = stdex::oneof<HardDefaultNothrow, double, long>;

	static_assert(stdex::is_default_constructible_v<B>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<B>, "");
	static_assert(stdex::is_trivially_destructible_v<B>, "");

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

TEST_CASE("static features single")
{
	using A = stdex::oneof<int>;

	static_assert(stdex::is_default_constructible_v<A>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<A>, "");
	static_assert(stdex::is_trivially_destructible_v<A>, "");

	using B = stdex::oneof<HardDefaultNothrow>;

	static_assert(stdex::is_default_constructible_v<B>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<B>, "");
	static_assert(stdex::is_trivially_destructible_v<B>, "");

	using C = stdex::oneof<HardDefault>;

	static_assert(stdex::is_default_constructible_v<C>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<C>, "");
	static_assert(stdex::is_trivially_destructible_v<C>, "");

	using D = stdex::oneof<stdex::oneof<>>;

	static_assert(not stdex::is_default_constructible_v<D>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<D>, "");
	static_assert(stdex::is_trivially_destructible_v<D>, "");

	using E = stdex::oneof<HardDefaultUDestruct>;

	static_assert(stdex::is_default_constructible_v<E>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<E>, "");
	static_assert(not stdex::is_trivially_destructible_v<E>, "");

	using F = stdex::oneof<NoDefaultUDestruct>;

	static_assert(not stdex::is_default_constructible_v<F>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<F>, "");
	static_assert(not stdex::is_trivially_destructible_v<F>, "");
}
