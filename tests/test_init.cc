#include "doctest.h"

#include <stdex/oneof.h>

#include <string>

TEST_CASE("default init")
{
	stdex::oneof<int, std::string> s;
	(void)s;

	struct R
	{
		stdex::oneof<double, int, R[]> rep;
	} y;
	(void)y;
}

TEST_CASE("value init")
{
	stdex::oneof<int, std::string> s{};
	REQUIRE(s.get<int>() == 0);

	REQUIRE((stdex::oneof<std::string>().get<std::string>()) == "");
	REQUIRE((stdex::oneof<std::string, int>().get<std::string>()) == "");

	stdex::oneof<long[], std::string> const x{};
	REQUIRE(x.get<long>() == 0);
	REQUIRE(std::move(x).get<long>() == 0);
}

TEST_CASE("emplace")
{
	stdex::oneof<int, std::string> s;
	auto&& x2 = s.emplace<std::string>("meow");

	REQUIRE(s.get<std::string>() == "meow");
	REQUIRE(x2 == "meow");
	REQUIRE_THROWS(s.get<int>());

	s.emplace<std::string>(6u, '+');
	REQUIRE(s.get<std::string>() == "++++++");
	REQUIRE_THROWS_AS(s.get<int>(), std::exception&);

	s.emplace<int>(10);
	REQUIRE(s.get<int>() == 10);
	REQUIRE_THROWS_AS(s.get<std::string>(), stdex::bad_variant_access&);
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
