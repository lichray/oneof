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
namespace detail
{

template <class... Fs>
struct overloaded;

template <class F1, class... Fs>
struct overloaded<F1, Fs...> : F1, overloaded<Fs...>::type
{
	typedef overloaded type;

	constexpr overloaded(F1&& head, Fs&&... tail)
	    : F1(std::forward<F1>(head)),
	      overloaded<Fs...>::type(std::forward<Fs>(tail)...)
	{
	}

	using F1::operator();
	using overloaded<Fs...>::type::operator();
};

template <class F>
struct overloaded<F> : F
{
	typedef F type;
	using F::operator();
};

}

template <class... Fs>
constexpr
typename detail::overloaded<Fs...>::type overload(Fs... x)
{
	return detail::overloaded<Fs...>(x...);
}

}
