/*-
 * Copyright (c) 2017 Zhihao Yuan.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#pragma once

#include "overload.h"

#if defined(_MSC_VER)
#include <type_traits>
#else
#include <experimental/type_traits>
#endif
#include <exception>
#include <new>
#include <stdlib.h>

namespace stdex
{

#if defined(_MSC_VER)
using std::is_same_v;
using std::is_convertible_v;
using std::is_constructible_v;
using std::is_nothrow_default_constructible_v;
using std::is_nothrow_constructible_v;
using std::is_nothrow_copy_constructible_v;
using std::is_nothrow_move_constructible_v;
using std::is_nothrow_move_assignable_v;
using std::is_default_constructible_v;
using std::is_trivially_default_constructible_v;
using std::is_trivially_destructible_v;
using std::is_lvalue_reference_v;
#else
using std::experimental::is_same_v;
using std::experimental::is_convertible_v;
using std::experimental::is_constructible_v;
using std::experimental::is_nothrow_default_constructible_v;
using std::experimental::is_nothrow_constructible_v;
using std::experimental::is_nothrow_copy_constructible_v;
using std::experimental::is_nothrow_move_constructible_v;
using std::experimental::is_nothrow_move_assignable_v;
using std::experimental::is_default_constructible_v;
using std::experimental::is_trivially_default_constructible_v;
using std::experimental::is_trivially_destructible_v;
using std::experimental::is_lvalue_reference_v;
#endif

using std::enable_if_t;

template <typename... T>
struct oneof;

struct bad_variant_access : std::exception
{
};

namespace detail
{

constexpr int variant_bufsize = 64;

template <bool...>
struct bool_seq;

template <bool... Xs>
constexpr bool and_v =
    is_same_v<bool_seq<Xs...>, bool_seq<((void)Xs, true)...>>;

template <bool... Xs>
constexpr bool or_v =
    not is_same_v<bool_seq<Xs...>, bool_seq<((void)Xs, false)...>>;

template <typename T>
constexpr bool can_be_alternative_v = is_same_v<std::decay_t<T>, T>;

template <typename T>
constexpr bool can_be_alternative_v<T[]> = true;

template <typename T>
struct variant_draft
{
	int index;
	T value;
};

template <typename T>
struct indirection
{
public:
	indirection() : p_(new T{}) {}

	indirection(indirection&& other) noexcept : p_(other.p_)
	{
		other.p_ = nullptr;
	}

	indirection& operator=(indirection&& other) noexcept
	{
		this->~indirection();
		return *new (this) indirection{ std::move(other) };
	}

	indirection(indirection const& other) : indirection(other.get()) {}

	indirection& operator=(indirection const& other)
	{
		get() = other.get();
		return *this;
	}

	template <typename A, typename... As,
	          disable_capturing<indirection, A> = 0>
	explicit indirection(A&& a, As&&... as)
	    : p_(new T(std::forward<A>(a), std::forward<As>(as)...))
	{
	}

	~indirection() { delete p_; }

	operator T&() { return this->get(); }
	operator T const&() const { return this->get(); }

	T& get() { return *p_; }
	T const& get() const { return *p_; }

private:
	T* p_;
};

template <typename T, typename = void>
struct variant_internal_impl
{
	using type = indirection<T>;
};

template <typename T>
struct variant_internal_impl<
    T, enable_if_t<sizeof(variant_draft<T>) <= variant_bufsize and
                   is_nothrow_move_constructible_v<T>>>
{
	using type = T;
};

template <typename T>
struct variant_internal
{
	using type = typename variant_internal_impl<T>::type;
	using element_type = T;
};

template <typename T>
struct variant_internal<T[]>
{
	using type = indirection<T>;
	using element_type = T;
};

template <typename T>
using variant_internal_t = typename variant_internal<T>::type;

template <typename T>
using variant_element_t = typename variant_internal<T>::element_type;

template <bool union_default, bool trivial_dtor, typename... T>
union variant_storage_rep;

template <typename T, typename... Ts>
union variant_storage_rep<true, true, T, Ts...>
{
	T first;
	std::aligned_union_t<1, Ts...> rest;
};

template <typename T, typename... Ts>
union variant_storage_rep<false, true, T, Ts...>
{
	constexpr variant_storage_rep() noexcept(
	    is_nothrow_default_constructible_v<T>)
	    : first()
	{
	}

	T first;
	std::aligned_union_t<1, Ts...> rest;
};

template <typename T, typename... Ts>
union variant_storage_rep<true, false, T, Ts...>
{
	~variant_storage_rep() {}

	T first;
	std::aligned_union_t<1, Ts...> rest;
};

template <typename T, typename... Ts>
union variant_storage_rep<false, false, T, Ts...>
{
	constexpr variant_storage_rep() noexcept(
	    is_nothrow_default_constructible_v<T>)
	    : first()
	{
	}

	~variant_storage_rep() {}

	T first;
	std::aligned_union_t<1, Ts...> rest;
};

template <int I, typename... Ts>
struct choose;

template <typename T, typename... Ts>
struct choose<0, T, Ts...>
{
	using type = T;
};

template <int I, typename... Ts>
using choose_t = typename choose<I, Ts...>::type;

template <int I, typename T, typename... Ts>
struct choose<I, T, Ts...>
{
	using type = choose_t<I - 1, Ts...>;
};

template <int I, typename V>
struct choose_alternative;

template <int I, typename... Ts>
struct choose_alternative<I, oneof<Ts...>>
{
	using type = choose_t<I, variant_internal_t<Ts>...>;
};

template <int I, typename V>
using choose_alternative_t = typename choose_alternative<I, V>::type;

template <typename T, typename... Xs>
struct directing;

template <typename T, typename... Xs>
struct directing<T, T, Xs...>
{
	static constexpr int value = 0;
};

template <typename T, typename... Xs>
constexpr int directing_v = directing<T, Xs...>::value;

template <typename T, typename X, typename... Xs>
struct directing<T, X, Xs...>
{
	static constexpr int value = directing_v<T, Xs...> + 1;
};

template <typename T>
struct directing<T>
{
	static_assert(alignof(T) == 0, "type not found");
};

template <typename, typename A, typename... Ts>
struct first_conv_construct
{
};

template <typename A, typename T, typename... Ts>
struct first_conv_construct<
    enable_if_t<is_constructible_v<T, A> and is_convertible_v<A, T>>, A, T,
    Ts...>
{
	using type = T;
};

template <typename X, typename A, typename T, typename... Ts>
struct first_conv_construct<X, A, T, Ts...> : first_conv_construct<X, A, Ts...>
{
};

template <typename A, typename... Ts>
using first_conv_construct_t =
    typename first_conv_construct<void, A, Ts...>::type;

template <typename, typename A, typename... Ts>
struct first_self_construct
{
};

template <typename A, typename T, typename... Ts>
struct first_self_construct<
    enable_if_t<is_constructible_v<T, A> and is_same_v<std::decay_t<A>, T>>, A,
    T, Ts...>
{
	using type = T;
};

template <typename X, typename A, typename T, typename... Ts>
struct first_self_construct<X, A, T, Ts...> : first_self_construct<X, A, Ts...>
{
};

template <typename A, typename... Ts>
using first_self_construct_t =
    typename first_self_construct<void, A, Ts...>::type;

template <typename, typename A, typename... Ts>
struct any_self_construct : std::false_type
{
};

template <typename A, typename... Ts>
struct any_self_construct<decltype(void(first_self_construct_t<A, Ts...>{})),
                          A, Ts...> : std::true_type
{
};

template <typename A, typename... Ts>
constexpr bool any_self_construct_v =
    any_self_construct<void, A, Ts...>::value;

template <typename T, typename V>
struct find_alternative;

template <typename T, typename... Xs>
struct find_alternative<T, oneof<Xs...>>
{
	static constexpr int value = directing_v<T, variant_element_t<Xs>...>;
};

template <typename T, typename V>
constexpr int find_alternative_v = find_alternative<T, V>::value;

template <int I>
struct index_t : std::integral_constant<int, I>
{
};

template <int I>
constexpr index_t<I> index_c = {};

template <typename T, typename... Ts>
struct variant_storage
{
	static constexpr int size = sizeof...(Ts) + 1;

	constexpr auto& rget(index_t<0>) noexcept { return u_.first; }
	constexpr auto& rget(index_t<0>) const noexcept { return u_.first; }

	template <int I>
	decltype(auto) rget(index_t<I>) noexcept
	{
		return reinterpret_cast<choose_t<I, T, Ts...>&>(u_.rest);
	}

	template <int I>
	decltype(auto) rget(index_t<I>) const noexcept
	{
		return reinterpret_cast<choose_t<I, T, Ts...> const&>(u_.rest);
	}

	variant_storage_rep<is_trivially_default_constructible_v<T> or
	                        not is_default_constructible_v<T>,
	                    is_trivially_destructible_v<T>, T, Ts...>
	    u_;
};

template <typename T>
struct variant_storage<T>
{
	static constexpr int size = 1;

	constexpr auto& rget(index_t<0>) noexcept { return u_; }
	constexpr auto& rget(index_t<0>) const noexcept { return u_; }

	T u_;
};

template <typename X, typename... Ts>
decltype(auto) rget(variant_storage<Ts...>& v)
{
	constexpr int i = directing_v<X, Ts...>;
	return v.rget(index_c<i>);
}

template <typename X, typename... Ts>
decltype(auto) rget(variant_storage<Ts...> const& v)
{
	constexpr int i = directing_v<X, Ts...>;
	return v.rget(index_c<i>);
}

template <typename S, typename A>
decltype(auto) get_like(S&& s, A const&)
{
	return detail::rget<A>(std::forward<S>(s));
}

template <typename R, int Low, int High, int Mid = (Low + High) / 2,
          typename = void>
struct _rvisit_at;

template <typename R, int Low, int High, int Mid>
struct _rvisit_at<R, Low, High, Mid, enable_if_t<(Low > High)>>
{
	template <typename... T>
	[[noreturn]] static R apply(int, T&&...)
	{
#if !defined(NDEBUG)
		abort();
#elif defined(_MSC_VER)
		__assume(0);
#else
		__builtin_unreachable();
#endif
	}
};

template <typename R, int Mid>
struct _rvisit_at<R, Mid, Mid, Mid>
{
	template <typename Raw, typename F,
	          enable_if_t<is_lvalue_reference_v<Raw>, int> = 0>
	static decltype(auto) apply(int n, F&& f, Raw&& tp)
	{
		return std::forward<F>(f)(tp.rget(index_c<Mid>));
	}

	template <typename Raw, typename F,
	          enable_if_t<not is_lvalue_reference_v<Raw>, int> = 0>
	static decltype(auto) apply(int n, F&& f, Raw&& tp)
	{
		return std::forward<F>(f)(std::move(tp.rget(index_c<Mid>)));
	}
};

template <typename R, int Low, int High, int Mid>
struct _rvisit_at<R, Low, High, Mid, enable_if_t<(Low < High)>>
{
	template <typename... T>
	static decltype(auto) apply(int n, T&&... t)
	{
		if (n < Mid)
			return _rvisit_at<R, Low, Mid - 1>::apply(
			    n, std::forward<T>(t)...);
		else if (n == Mid)
			return _rvisit_at<R, Mid, Mid>::apply(
			    n, std::forward<T>(t)...);
		else
			return _rvisit_at<R, Mid + 1, High>::apply(
			    n, std::forward<T>(t)...);
	}
};

template <typename R = void, typename Raw, typename F>
inline decltype(auto) rvisit_at(int n, F&& f, Raw&& tp)
{
	constexpr int m = std::decay_t<Raw>::size;
	return _rvisit_at<R, 0, m - 1>::apply(n, std::forward<F>(f),
	                                      std::forward<Raw>(tp));
}

template <typename T, typename...>
struct car
{
	using type = T;
};

template <typename... T>
using car_t = typename car<T...>::type;

template <bool all_trivial_dtor, typename... T>
struct variant_layout;

template <typename... T>
struct variant_layout<true, T...>
{
	int index;
	variant_storage<T...> data;
};

template <typename... T>
struct variant_layout<false, T...>
{
	~variant_layout()
	{
		rvisit_at(index, [](auto&& a) { a.~auto(); }, data);
	}

	int index = 0;
	variant_storage<T...> data;
};

}

template <typename... T>
struct oneof
{
	static_assert(detail::and_v<detail::can_be_alternative_v<T>...>,
	              "an alternative type must not be cv-qualified, "
	              "reference, function, or array of known bound");

private:
	static constexpr bool trivial = detail::and_v<
	    is_trivially_destructible_v<detail::variant_internal_t<T>>...>;

	static constexpr bool move_storage = detail::and_v<
	    is_nothrow_move_constructible_v<detail::variant_internal_t<T>>...>;

	static constexpr bool copy_storage = detail::and_v<
	    is_nothrow_copy_constructible_v<detail::variant_internal_t<T>>...>;

	static constexpr bool move_through = detail::and_v<
	    is_nothrow_move_assignable_v<detail::variant_internal_t<T>>...>;

	static_assert(move_storage, "library is broken, please report bug");

public:
	constexpr oneof() = default;

	oneof(oneof const& other)
	{
		detail::rvisit_at(
		    rep_.index = other.rep_.index,
		    [&](auto&& ra) { new (&rep_.data) auto(ra); },
		    other.rep_.data);
	}

	oneof(oneof&& other) noexcept
	{
		detail::rvisit_at(
		    rep_.index = other.rep_.index,
		    [&](auto&& ra) { new (&rep_.data) auto(std::move(ra)); },
		    other.rep_.data);
	}

	template <typename A, disable_capturing<oneof, A> = 0,
	          typename E = detail::first_self_construct_t<
	              A, detail::variant_element_t<T>...>>
	oneof(A&& a)
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		using type = detail::choose_alternative_t<i, oneof>;

		new (&rep_.data) type(std::forward<A>(a));
		rep_.index = i;
	}

	template <typename A, disable_capturing<oneof, A> = 0,
	          enable_if_t<not detail::any_self_construct_v<
	                          A, detail::variant_element_t<T>...>,
	                      int> = 0,
	          typename E = detail::first_conv_construct_t<
	              A, detail::variant_element_t<T>...>>
	explicit oneof(A&& a)
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		using type = detail::choose_alternative_t<i, oneof>;

		new (&rep_.data) type(std::forward<A>(a));
		rep_.index = i;
	}

	oneof& operator=(oneof const& other)
	{
		if (not trivial and rep_.index == other.rep_.index)
		{
			detail::rvisit_at(other.rep_.index,
			                  [&](auto&& ra) {
				                  detail::get_like(rep_.data,
				                                   ra) = ra;
				          },
			                  other.rep_.data);
		}
		else if (copy_storage)
		{
			if (&other != this)
			{
				this->~oneof();
				return *new (this) oneof{ other };
			}
		}
		else
		{
			detail::rvisit_at(other.rep_.index,
			                  [&](auto&& ra) {
				                  auto tmp = ra;
				                  this->~oneof();
				                  new (&rep_.data) auto(
				                      std::move(tmp));
				          },
			                  other.rep_.data);
			rep_.index = other.rep_.index;
		}

		return *this;
	}

	oneof& operator=(oneof&& other) noexcept(move_through)
	{
		if (not trivial and rep_.index == other.rep_.index)
		{
			detail::rvisit_at(other.rep_.index,
			                  [&](auto&& ra) {
				                  detail::get_like(rep_.data,
				                                   ra) =
				                      std::move(ra);
				          },
			                  other.rep_.data);

			return *this;
		}
		else
		{
			this->~oneof();
			return *new (this) oneof{ std::move(other) };
		}
	}

	template <typename A, disable_capturing<oneof, A> = 0>
	oneof& operator=(A&& a)
	{
		emplace<std::decay_t<A>>(std::forward<A>(a));
		return *this;
	}

	template <typename E, typename... Args>
	E& emplace(Args&&... args)
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		using type = detail::choose_alternative_t<i, oneof>;

		if (is_nothrow_constructible_v<type, Args...>)
		{
			this->~oneof();
			auto p =
			    new (&rep_.data) type(std::forward<Args>(args)...);
			rep_.index = i;
			return *p;
		}
		else
		{
			type tmp(std::forward<Args>(args)...);
			this->~oneof();
			auto p = new (&rep_.data) type(std::move(tmp));
			rep_.index = i;
			return *p;
		}
	}

	template <typename R = void, typename... F>
	decltype(auto) match(F&&... f) &
	{
		return detail::rvisit_at<R>(
		    rep_.index, overload(std::forward<F>(f)...), rep_.data);
	}

	template <typename R = void, typename... F>
	decltype(auto) match(F&&... f) const &
	{
		return detail::rvisit_at<R>(
		    rep_.index, overload(std::forward<F>(f)...), rep_.data);
	}

	template <typename R = void, typename... F>
	decltype(auto) match(F&&... f) &&
	{
		return detail::rvisit_at<R>(rep_.index,
		                            overload(std::forward<F>(f)...),
		                            std::move(rep_.data));
	}

	template <typename R = void, typename... F>
	decltype(auto) match(F&&... f) const &&
	{
		return detail::rvisit_at<R>(rep_.index,
		                            overload(std::forward<F>(f)...),
		                            std::move(rep_.data));
	}

	template <typename E>
	constexpr bool is() const noexcept
	{
		return rep_.index == detail::find_alternative_v<E, oneof>;
	}

	constexpr int which() const noexcept
	{
		return rep_.index;
	}

	template <typename E>
	constexpr auto get() & -> E&
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		if (i != rep_.index)
			throw bad_variant_access{};

		return rep_.data.rget(detail::index_c<i>);
	}

	template <typename E>
	constexpr auto get() const & -> E const&
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		if (i != rep_.index)
			throw bad_variant_access{};

		return rep_.data.rget(detail::index_c<i>);
	}

	template <typename E>
	constexpr decltype(auto) get() &&
	{
		return std::move(get<E>());
	}

	template <typename E>
	constexpr decltype(auto) get() const &&
	{
		return std::move(get<E>());
	}

	template <typename E>
	auto maybe() & -> E*
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		if (i != rep_.index)
			return nullptr;

		return std::addressof(
		    static_cast<E&>(rep_.data.rget(detail::index_c<i>)));
	}

	template <typename E>
	auto maybe() const & -> E const*
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		if (i != rep_.index)
			return nullptr;

		return std::addressof(
		    static_cast<E const&>(rep_.data.rget(detail::index_c<i>)));
	}

	template <typename E>
	auto maybe() && = delete;

	template <typename E>
	auto maybe() const && = delete;

	friend bool operator==(oneof const& v, oneof const& w)
	{
		if (v.which() != w.which())
			return false;
		else
			return v.match([&](auto&& x) {
				return x == detail::get_like(w.rep_.data, x);
			});
	}

	friend bool operator!=(oneof const& v, oneof const& w)
	{
		if (v.which() != w.which())
			return true;
		else
			return v.match([&](auto&& x) {
				return x != detail::get_like(w.rep_.data, x);
			});
	}

	friend bool operator<(oneof const& v, oneof const& w)
	{
		if (v.which() < w.which())
			return true;
		else if (v.which() > w.which())
			return false;
		else
			return v.match([&](auto&& x) {
				return x < detail::get_like(w.rep_.data, x);
			});
	}

	friend bool operator<=(oneof const& v, oneof const& w)
	{
		if (v.which() < w.which())
			return true;
		else if (v.which() > w.which())
			return false;
		else
			return v.match([&](auto&& x) {
				return x <= detail::get_like(w.rep_.data, x);
			});
	}

	friend bool operator>(oneof const& v, oneof const& w)
	{
		if (v.which() > w.which())
			return true;
		else if (v.which() < w.which())
			return false;
		else
			return v.match([&](auto&& x) {
				return x > detail::get_like(w.rep_.data, x);
			});
	}

	friend bool operator>=(oneof const& v, oneof const& w)
	{
		if (v.which() > w.which())
			return true;
		else if (v.which() < w.which())
			return false;
		else
			return v.match([&](auto&& x) {
				return x >= detail::get_like(w.rep_.data, x);
			});
	}

private:
	detail::variant_layout<trivial, detail::variant_internal_t<T>...> rep_;
};

template <>
struct oneof<>
{
	oneof() = delete;
};

}
