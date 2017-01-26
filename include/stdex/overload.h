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

#include <utility>

namespace stdex
{

template <typename T, typename U>
using disable_capturing =
    std::enable_if_t<!std::is_base_of<T, std::remove_reference_t<U>>::value,
                     int>;

namespace detail
{

template <typename... Fs>
struct overloaded;

template <typename F, typename... Fs>
struct overloaded<F, Fs...> : F, overloaded<Fs...>::type
{
	using type = overloaded;
	using base = typename overloaded<Fs...>::type;

	template <typename T, typename... Ts,
	          disable_capturing<overloaded, T> = 0>
	constexpr overloaded(T&& head, Ts&&... tail)
	    : F(std::forward<T>(head)), base(std::forward<Ts>(tail)...)
	{
	}

	using F::operator();
	using base::operator();
};

template <typename F>
struct overloaded<F> : F
{
	template <typename T, disable_capturing<overloaded, T> = 0>
	constexpr overloaded(T&& v) : F(std::forward<T>(v))
	{
	}

	using type = F;
	using F::operator();
};

template <typename T>
struct is_overload_call_wrapper_impl : std::false_type
{
};

template <typename... Ts>
struct is_overload_call_wrapper_impl<overloaded<Ts...>> : std::true_type
{
};

template <typename T>
struct is_overload_call_wrapper
    : is_overload_call_wrapper_impl<std::decay_t<T>>
{
};

}

template <typename... Fs>
constexpr auto overload(Fs&&... x)
{
	return detail::overloaded<std::decay_t<Fs>...>(std::forward<Fs>(x)...);
}

}
