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

namespace stdex
{

#if defined(_MSC_VER)
using std::is_same_v;
using std::is_nothrow_default_constructible_v;
using std::is_nothrow_move_constructible_v;
using std::is_nothrow_move_assignable_v;
using std::is_default_constructible_v;
using std::is_trivially_default_constructible_v;
using std::is_trivially_destructible_v;
#else
using std::experimental::is_same_v;
using std::experimental::is_nothrow_default_constructible_v;
using std::experimental::is_nothrow_move_constructible_v;
using std::experimental::is_nothrow_move_assignable_v;
using std::experimental::is_default_constructible_v;
using std::experimental::is_trivially_default_constructible_v;
using std::experimental::is_trivially_destructible_v;
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

	indirection(T const& v) : p_(new T(v)) {}
	indirection(T&& v) : p_(new T(std::move(v))) {}

	indirection& operator=(T const&) = delete;
	indirection& operator=(T&&) = delete;

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
                   is_nothrow_move_assignable_v<T>>>
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
	static constexpr int value = directing_v<T, Xs...>;
};

template <typename T>
struct directing<T>
{
	static_assert(alignof(T) == 0, "type not found");
};

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
	constexpr auto& rget(index_t<0>) noexcept { return u_; }
	constexpr auto& rget(index_t<0>) const noexcept { return u_; }

	T u_;
};

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

	template <typename E>
	auto get() & -> detail::variant_element_t<E>&
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		if (i != rep_.index)
			throw bad_variant_access{};

		return rep_.data.rget(detail::index_c<i>);
	}

	template <typename E>
	auto get() const & -> detail::variant_element_t<E> const&
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		if (i != rep_.index)
			throw bad_variant_access{};

		return rep_.data.rget(detail::index_c<i>);
	}

	template <typename E>
	decltype(auto) get() &&
	{
		return std::move(get<E>());
	}

	template <typename E>
	decltype(auto) get() const &&
	{
		return std::move(get<E>());
	}

private:
	using first_type = detail::variant_internal_t<detail::car_t<T...>>;

	detail::variant_layout<detail::and_v<is_trivially_destructible_v<
	                           detail::variant_internal_t<T>>...>,
	                       detail::variant_internal_t<T>...>
	    rep_;
};

template <>
struct oneof<>
{
	oneof() = delete;
};

}
