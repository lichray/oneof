#include "doctest.h"

#include <stdex/oneof.h>

#include <string>
#include <memory>

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

	auto s2 = std::move(s);
	REQUIRE(s2.get<std::string>() == "meow");

	auto s3 = s2;
	REQUIRE(s3.get<std::string>() == "meow");
	REQUIRE(s2.get<std::string>() == "meow");

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
	static_assert(not stdex::is_trivially_copyable_v<A>, "");

	using B = stdex::oneof<HardDefaultNothrow, double, long>;

	static_assert(stdex::is_default_constructible_v<B>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<B>, "");
	static_assert(stdex::is_trivially_destructible_v<B>, "");
	static_assert(stdex::is_trivially_copyable_v<B>, "");

	using C = stdex::oneof<HardDefault, long>;

	static_assert(stdex::is_default_constructible_v<C>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<C>, "");
	static_assert(stdex::is_trivially_destructible_v<C>, "");
	static_assert(stdex::is_trivially_copyable_v<C>, "");

	using D = stdex::oneof<stdex::oneof<>, long>;

	static_assert(not stdex::is_default_constructible_v<D>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<D>, "");
	static_assert(stdex::is_trivially_destructible_v<D>, "");
	static_assert(stdex::is_trivially_copyable_v<D>, "");

	using E = stdex::oneof<stdex::oneof<>, std::string>;

	static_assert(not stdex::is_default_constructible_v<E>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<E>, "");
	static_assert(not stdex::is_trivially_destructible_v<E>, "");
	static_assert(not stdex::is_trivially_copyable_v<E>, "");
}

TEST_CASE("static features single")
{
	using A = stdex::oneof<int>;

	static_assert(stdex::is_default_constructible_v<A>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<A>, "");
	static_assert(stdex::is_trivially_destructible_v<A>, "");
	static_assert(stdex::is_trivially_copyable_v<A>, "");

	using B = stdex::oneof<HardDefaultNothrow>;

	static_assert(stdex::is_default_constructible_v<B>, "");
	static_assert(stdex::is_nothrow_default_constructible_v<B>, "");
	static_assert(stdex::is_trivially_destructible_v<B>, "");
	static_assert(stdex::is_trivially_copyable_v<B>, "");

	using C = stdex::oneof<HardDefault>;

	static_assert(stdex::is_default_constructible_v<C>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<C>, "");
	static_assert(stdex::is_trivially_destructible_v<C>, "");
	static_assert(stdex::is_trivially_copyable_v<C>, "");

	using D = stdex::oneof<stdex::oneof<>>;

	static_assert(not stdex::is_default_constructible_v<D>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<D>, "");
	static_assert(stdex::is_trivially_destructible_v<D>, "");
	static_assert(stdex::is_trivially_copyable_v<D>, "");

	using E = stdex::oneof<HardDefaultUDestruct>;

	static_assert(stdex::is_default_constructible_v<E>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<E>, "");
	static_assert(not stdex::is_trivially_destructible_v<E>, "");
	static_assert(not stdex::is_trivially_copyable_v<E>, "");

	using F = stdex::oneof<NoDefaultUDestruct>;

	static_assert(not stdex::is_default_constructible_v<F>, "");
	static_assert(not stdex::is_nothrow_default_constructible_v<F>, "");
	static_assert(not stdex::is_trivially_destructible_v<F>, "");
	static_assert(not stdex::is_trivially_copyable_v<F>, "");
}

TEST_CASE("static properties")
{
	using A = stdex::oneof<int, std::unique_ptr<int>>;

	static_assert(stdex::is_constructible_v<A, int>, "");
	static_assert(stdex::is_convertible_v<int, A>, "");
	static_assert(std::is_assignable<A, int>::value, "");

	static_assert(stdex::is_constructible_v<A, bool>, "");
	static_assert(stdex::is_convertible_v<bool, A>, "");
	static_assert(std::is_assignable<A, bool>::value, "");

	static_assert(not stdex::is_constructible_v<A, unsigned>, "");
	static_assert(not stdex::is_convertible_v<unsigned, A>, "");
	static_assert(not std::is_assignable<A, unsigned>::value, "");

	enum S
	{
		Ok,
		Nope
	};

	using B = stdex::oneof<std::unique_ptr<int>, unsigned long>;

	static_assert(stdex::is_constructible_v<B, S>, "");
	static_assert(stdex::is_convertible_v<S, B>, "");
	static_assert(std::is_assignable<B, S>::value, "");

	static_assert(not stdex::is_constructible_v<B, int>, "");
	static_assert(not stdex::is_convertible_v<int, B>, "");
	static_assert(not std::is_assignable<B, int>::value, "");
}

struct Empty
{
};

struct MoveOnly
{
	MoveOnly(MoveOnly&&) = default;
	MoveOnly& operator=(MoveOnly&&) = default;
	MoveOnly(MoveOnly const&) = delete;
	MoveOnly& operator=(MoveOnly const&) = delete;
};

struct MoveCtorOnly
{
	MoveCtorOnly(MoveCtorOnly&&) = default;
	MoveCtorOnly& operator=(MoveCtorOnly&&) = delete;
	MoveCtorOnly(MoveCtorOnly const&) = delete;
	MoveCtorOnly& operator=(MoveCtorOnly const&) = delete;
};

struct NoMove
{
	NoMove(NoMove&&) = delete;
};

struct NoMoveCtor
{
	NoMoveCtor(NoMoveCtor&&) = delete;
	NoMoveCtor& operator=(NoMoveCtor&&) = default;
};

struct NontrivialCopy
{
	NontrivialCopy(NontrivialCopy const&) {}
	NontrivialCopy& operator=(NontrivialCopy const&) = default;
};

struct NontrivialCopyCtorOnly
{
	NontrivialCopyCtorOnly(NontrivialCopyCtorOnly const&) noexcept {}
	NontrivialCopyCtorOnly&
	operator=(NontrivialCopyCtorOnly const&) = delete;
	NontrivialCopyCtorOnly& operator=(NontrivialCopyCtorOnly&&) = default;
};

struct NontrivialMoveOnly
{
	NontrivialMoveOnly(NontrivialMoveOnly&&) noexcept {}
	NontrivialMoveOnly& operator=(NontrivialMoveOnly&&) = default;
	NontrivialMoveOnly(NontrivialMoveOnly const&) = delete;
	NontrivialMoveOnly& operator=(NontrivialMoveOnly const&) = delete;
};

struct NontrivialMoveCtorOnly
{
	NontrivialMoveCtorOnly(NontrivialMoveCtorOnly&&) noexcept {}
	NontrivialMoveCtorOnly& operator=(NontrivialMoveCtorOnly&&) = delete;
	NontrivialMoveCtorOnly(NontrivialMoveCtorOnly const&) = delete;
	NontrivialMoveCtorOnly&
	operator=(NontrivialMoveCtorOnly const&) = delete;
};

struct HardMoveOnly
{
	HardMoveOnly(HardMoveOnly&&) {}
	HardMoveOnly& operator=(HardMoveOnly&&) = default;
	HardMoveOnly(HardMoveOnly const&) = delete;
	HardMoveOnly& operator=(HardMoveOnly const&) = delete;
};

struct HardNoCopyCtor
{
	HardNoCopyCtor(HardNoCopyCtor&&) {}
	HardNoCopyCtor& operator=(HardNoCopyCtor&&) { return *this; }
	HardNoCopyCtor(HardNoCopyCtor const&) = delete;
	HardNoCopyCtor& operator=(HardNoCopyCtor const&) { return *this; }
};

TEST_CASE("copy/move/swap")
{
	using A = stdex::oneof<int, Empty>;

	static_assert(std::is_trivially_copy_constructible<A>::value, "");
	static_assert(std::is_trivially_move_constructible<A>::value, "");
	static_assert(std::is_trivially_copy_assignable<A>::value, "");
	static_assert(std::is_trivially_move_assignable<A>::value, "");
	static_assert(std::is_trivially_copyable<A>::value, "");
	static_assert(stdex::is_nothrow_swappable_v<A>, "");

	using B = stdex::oneof<int, MoveOnly>;

	static_assert(not std::is_copy_constructible<B>::value, "");
	static_assert(std::is_move_constructible<B>::value, "");
	static_assert(not std::is_copy_assignable<B>::value, "");
	static_assert(std::is_move_assignable<B>::value, "");
	static_assert(std::is_trivially_copyable<B>::value, "");
	static_assert(stdex::is_nothrow_swappable_v<B>, "");

	using B2 = stdex::oneof<int, MoveCtorOnly>;

	static_assert(not std::is_copy_constructible<B2>::value, "");
	static_assert(std::is_move_constructible<B2>::value, "");
	static_assert(not std::is_copy_assignable<B2>::value, "");
	static_assert(not std::is_move_assignable<B2>::value, "");
	static_assert(std::is_trivially_copyable<B2>::value, "");
	static_assert(not std::is_move_assignable<MoveCtorOnly>::value, "");
	static_assert(stdex::is_swappable_v<B2> ==
	                  stdex::is_swappable_v<MoveCtorOnly>,
	              "");

	using B3 = stdex::oneof<int, NontrivialMoveOnly>;

	static_assert(not std::is_copy_constructible<B3>::value, "");
	static_assert(std::is_move_constructible<B3>::value, "");
	static_assert(not std::is_copy_assignable<B3>::value, "");
	static_assert(std::is_move_assignable<B3>::value, "");
	static_assert(not std::is_trivially_copyable<B3>::value, "");
	static_assert(stdex::is_nothrow_swappable_v<B3>, "");

	using B4 = stdex::oneof<int, NontrivialMoveCtorOnly>;

	static_assert(not std::is_copy_constructible<B4>::value, "");
	static_assert(std::is_move_constructible<B4>::value, "");
	static_assert(not std::is_copy_assignable<B4>::value, "");
	static_assert(not std::is_move_assignable<B4>::value, "");
	static_assert(not std::is_trivially_copyable<B4>::value, "");
	static_assert(stdex::is_swappable_v<B4> ==
	                  stdex::is_swappable_v<NontrivialMoveCtorOnly>,
	              "");

	using C = stdex::oneof<int, NoMove>;

	static_assert(not std::is_copy_constructible<C>::value, "");
	static_assert(not std::is_move_constructible<C>::value, "");
	static_assert(not std::is_copy_assignable<C>::value, "");
	static_assert(not std::is_move_assignable<C>::value, "");

	using C2 = stdex::oneof<int, NoMoveCtor>;

	static_assert(not std::is_copy_constructible<C2>::value, "");
	static_assert(not std::is_move_constructible<C2>::value, "");
	static_assert(not std::is_copy_assignable<C2>::value, "");
	static_assert(not std::is_move_assignable<C2>::value, "");

	using D = stdex::oneof<int, NontrivialCopy>;

	static_assert(std::is_copy_constructible<D>::value, "");
	static_assert(std::is_move_constructible<D>::value, "");
	static_assert(std::is_copy_assignable<D>::value, "");
	static_assert(std::is_move_assignable<D>::value, "");
	static_assert(stdex::is_nothrow_swappable_v<D>, "");

	using D2 = stdex::oneof<int, NontrivialCopyCtorOnly>;

	static_assert(std::is_copy_constructible<D2>::value, "");
	static_assert(std::is_move_constructible<D2>::value, "");
	static_assert(not std::is_copy_assignable<D2>::value, "");
	static_assert(std::is_move_assignable<D2>::value, "");
	static_assert(stdex::is_nothrow_swappable_v<D2>, "");

	using E = stdex::oneof<int, HardMoveOnly>;

	static_assert(not std::is_copy_constructible<E>::value, "");
	static_assert(std::is_move_constructible<E>::value, "");
	static_assert(not std::is_copy_assignable<E>::value, "");
	static_assert(std::is_move_assignable<E>::value, "");
	static_assert(stdex::is_nothrow_swappable_v<E>, "");

	using F = stdex::oneof<int, HardNoCopyCtor>;

	static_assert(not std::is_copy_constructible<F>::value, "");
	static_assert(std::is_move_constructible<F>::value, "");
	static_assert(not std::is_copy_assignable<F>::value, "");
	static_assert(std::is_move_assignable<F>::value, "");
	static_assert(stdex::is_nothrow_swappable_v<F>, "");

	struct Rec
	{
		std::unique_ptr<int> m;
		stdex::oneof<Rec[], int> n;
	};

	static_assert(not std::is_copy_constructible<Rec>::value, "");
	static_assert(std::is_move_constructible<Rec>::value, "");
	static_assert(not std::is_copy_assignable<Rec>::value, "");
	static_assert(std::is_move_assignable<Rec>::value, "");
	static_assert(stdex::is_nothrow_swappable_v<Rec>, "");
}
