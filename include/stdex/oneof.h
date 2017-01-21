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

namespace stdex
{

#if defined(_MSC_VER)
using std::is_same_v;
#else
using std::experimental::is_same_v;
#endif

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

}

template <typename... T>
struct oneof
{
	static_assert(detail::and_v<detail::can_be_alternative_v<T>...>,
	              "an alternative type must not be cv-qualified, "
	              "reference, function, or array of known bound");
};

}
