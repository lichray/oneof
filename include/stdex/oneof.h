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
using std::is_nothrow_move_assignable_v;
#else
using std::experimental::is_same_v;
using std::experimental::is_nothrow_move_assignable_v;
#endif

using std::enable_if_t;

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
	indirection() : p_(new T) {}

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
};

template <typename T>
struct variant_internal<T[]>
{
	using type = indirection<T>;
};

template <typename T>
using variant_internal_t = typename variant_internal<T>::type;

}

template <typename... T>
struct oneof
{
	static_assert(detail::and_v<detail::can_be_alternative_v<T>...>,
	              "an alternative type must not be cv-qualified, "
	              "reference, function, or array of known bound");
};

}