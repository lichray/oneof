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
#include <ciso646>
#include <type_traits>
#else
#include <experimental/type_traits>
#endif
#include <exception>
#include <new>
#include <limits>
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
using std::is_move_constructible_v;
using std::is_trivially_default_constructible_v;
using std::is_trivially_destructible_v;
using std::is_trivially_copyable_v;
using std::is_lvalue_reference_v;
using std::is_integral_v;
using std::is_floating_point_v;
using std::is_signed_v;
using std::is_unsigned_v;
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
using std::experimental::is_move_constructible_v;
using std::experimental::is_trivially_default_constructible_v;
using std::experimental::is_trivially_destructible_v;
using std::experimental::is_trivially_copyable_v;
using std::experimental::is_lvalue_reference_v;
using std::experimental::is_integral_v;
using std::experimental::is_floating_point_v;
using std::experimental::is_signed_v;
using std::experimental::is_unsigned_v;
#endif

using std::enable_if_t;
using std::is_nothrow_copy_constructible;
using std::is_nothrow_move_assignable;
using std::is_trivially_copyable;
using std::is_trivially_destructible;
using std::is_copy_constructible;
using std::is_move_constructible;
using std::is_copy_assignable;
using std::is_move_assignable;

template <typename... T>
struct oneof;

struct bad_variant_access : std::exception
{
};

template <bool V>
using bool_constant = std::integral_constant<bool, V>;

struct bottom;

template <bool V>
struct copy_constructible
{
};

template <>
struct copy_constructible<false>
{
	copy_constructible() = default;
	copy_constructible(copy_constructible const&) = delete;
	copy_constructible(copy_constructible&&) = default;
	copy_constructible& operator=(copy_constructible const&) = default;
	copy_constructible& operator=(copy_constructible&&) = default;
};

template <bool V>
struct move_constructible
{
};

template <>
struct move_constructible<false>
{
	move_constructible() = default;
	move_constructible(move_constructible&&) = delete;
	move_constructible& operator=(move_constructible&&) = default;
};

template <bool V>
struct copy_assignable
{
};

template <>
struct copy_assignable<false>
{
	copy_assignable() = default;
	copy_assignable(copy_assignable const&) = default;
	copy_assignable(copy_assignable&&) = default;
	copy_assignable& operator=(copy_assignable const&) = delete;
	copy_assignable& operator=(copy_assignable&&) = default;
};

template <bool V>
struct move_assignable
{
};

template <>
struct move_assignable<false>
{
	move_assignable() = default;
	move_assignable(move_assignable&&) = default;
	move_assignable& operator=(move_assignable&&) = delete;
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

template <template <typename> class... F>
struct both
{
	template <typename T>
	using call = bool_constant<and_v<F<T>::value...>>;
};

template <template <typename> class... F>
struct either
{
	template <typename T>
	using call = bool_constant<or_v<F<T>::value...>>;
};

#if !(defined(_MSC_VER) && _HAS_CXX17)
namespace adl
{
using std::swap;

struct is_nothrow_swappable_impl
{
	template <typename T>
	static auto test(int)
	    -> bool_constant<noexcept(swap(std::declval<T&>(),
	                                   std::declval<T&>()))>;

	template <typename>
	static auto test(...) -> std::false_type;
};

template <typename T, typename = void>
constexpr bool is_swappable_impl = false;

template <typename T>
constexpr bool is_swappable_impl<T, decltype(void(swap(std::declval<T&>(),
                                                       std::declval<T&>())))> =
    true;
}
#endif

template <typename T, typename = void>
constexpr bool is_unscoped_enum_v = false;

template <typename T>
constexpr bool is_unscoped_enum_v<T, enable_if_t<std::is_enum<T>::value>> =
    is_convertible_v<T, std::underlying_type_t<T>>;

template <typename T, typename V, typename = void>
constexpr bool cannot_represent_v = (std::numeric_limits<T>::min() <
                                     std::numeric_limits<V>::min()) or
                                    (std::numeric_limits<T>::max() >
                                     std::numeric_limits<V>::max());

template <typename V>
constexpr bool cannot_represent_v<bool, V> = false;

template <typename T, typename V>
constexpr bool cannot_represent_v<
    T, V, enable_if_t<is_signed_v<T> and is_unsigned_v<V>>> = true;

template <typename T, typename V>
constexpr bool
    cannot_represent_v<T, V,
                       enable_if_t<is_unsigned_v<T> and is_signed_v<V> and
                                   not is_same_v<T, bool>>> =
        (std::numeric_limits<T>::max() >
         std::make_unsigned_t<V>(std::numeric_limits<V>::max()));

template <typename From, typename To, typename = void>
constexpr bool is_narrowing = false;

template <typename From, typename To>
constexpr bool is_narrowing<
    From, To, enable_if_t<is_floating_point_v<From> and is_integral_v<To>>> =
    true;

template <>
constexpr bool is_narrowing<long double, double> = true;

template <>
constexpr bool is_narrowing<long double, float> = true;

template <>
constexpr bool is_narrowing<double, float> = true;

template <typename From, typename To>
constexpr bool is_narrowing<
    From, To,
    enable_if_t<or_v<is_integral_v<From>, is_unscoped_enum_v<From>> and
                is_floating_point_v<To>>> = true;

template <typename From, typename To>
constexpr bool is_narrowing<
    From, To, enable_if_t<is_integral_v<From> and is_integral_v<To>>> =
    cannot_represent_v<From, To>;

template <typename From, typename To>
constexpr bool is_narrowing<
    From, To, enable_if_t<is_unscoped_enum_v<From> and is_integral_v<To>>> =
    cannot_represent_v<std::underlying_type_t<From>, To>;

template <typename From>
constexpr bool is_narrowing<
    From, bool,
    enable_if_t<either<std::is_pointer, std::is_null_pointer,
                       std::is_member_pointer>::call<From>::value>> = true;

template <typename From, typename To>
constexpr bool is_non_narrowing_convertible_v =
    is_convertible_v<From, To> and
    not is_narrowing<std::decay_t<From>, std::decay_t<To>>;

template <typename T, typename... Ts>
struct overloaded_ctor : overloaded_ctor<Ts...>
{
	using overloaded_ctor<Ts...>::test;

	template <typename U>
	static enable_if_t<is_non_narrowing_convertible_v<U, T>, T> test(T);
};

template <typename T>
struct overloaded_ctor<T>
{
	template <typename U>
	static enable_if_t<is_non_narrowing_convertible_v<U, T>, T> test(T);
};

template <typename T>
struct is_decayed : std::is_same<std::decay_t<T>, T>
{
};

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
		return *::new ((void*)this) indirection{ std::move(other) };
	}

	indirection(indirection const& other) : indirection(other.get()) {}

	indirection& operator=(indirection const& other)
	{
		if (p_)
			get() = other.get();
		else
		{
			indirection tmp{ other };
			swap(*this, tmp);
		}

		return *this;
	}

	template <typename A, typename... As,
	          disable_capturing<indirection, A> = 0>
	explicit indirection(A&& a, As&&... as)
	    : p_(new T(std::forward<A>(a), std::forward<As>(as)...))
	{
	}

	~indirection() { delete p_; }

	friend void swap(indirection& x, indirection& y) noexcept
	{
		std::swap(x.p_, y.p_);
	}

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
                   (is_nothrow_move_constructible_v<T> or
                    not is_move_constructible_v<T>)>>
{
	using type = T;
};

template <typename T>
struct variant_internal
{
	using type = typename variant_internal_impl<T>::type;
	using element_type = T;
	using model_type = T;
};

template <typename T>
struct variant_internal<T[]>
{
	using type = indirection<T>;
	using element_type = T;
	using model_type = type;
};

template <typename T>
struct variant_unwrap_internal
{
	using type = T;
};

template <typename T>
struct variant_unwrap_internal<indirection<T>>
{
	using type = T;
};

template <typename T>
struct variant_unwrap_internal<indirection<T> const>
{
	using type = T const;
};

template <typename T>
using variant_internal_t = typename variant_internal<T>::type;

template <typename T>
using variant_element_t = typename variant_internal<T>::element_type;

template <typename T>
using variant_model_t = typename variant_internal<T>::model_type;

template <typename T>
using variant_unwrap_internal_t = typename variant_unwrap_internal<T>::type;

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

template <typename... T>
using variant_ctor = overloaded_ctor<variant_element_t<T>...>;

template <typename A, typename... T>
using element_to_construct_t =
    decltype(variant_ctor<T...>::template test<A>(std::declval<A>()));

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
	return v.rget(index_c<directing_v<X, Ts...>>);
}

template <typename X, typename... Ts>
decltype(auto) rget(variant_storage<Ts...> const& v)
{
	return v.rget(index_c<directing_v<X, Ts...>>);
}

template <typename X, typename... Ts>
auto uget(variant_storage<Ts...>& v) -> X&
{
	constexpr int i = directing_v<X, variant_unwrap_internal_t<Ts>...>;
	return v.rget(index_c<i>);
}

template <typename X, typename... Ts>
auto uget(variant_storage<Ts...> const& v) -> X const&
{
	constexpr int i = directing_v<X, variant_unwrap_internal_t<Ts>...>;
	return v.rget(index_c<i>);
}

template <int I, typename S>
decltype(auto) xget(S&& s, std::true_type)
{
	using btype = std::remove_reference_t<decltype(s.rget(index_c<I>))>;
	using cv_etype = variant_unwrap_internal_t<btype>;
	using rtype = std::conditional_t<is_lvalue_reference_v<S>, cv_etype&,
	                                 cv_etype&&>;
	return static_cast<rtype>(static_cast<cv_etype&>(s.rget(index_c<I>)));
}

template <int I, typename S>
decltype(auto) xget(S&& s, std::false_type)
{
	return s.rget(index_c<I>);
}

template <typename S, typename A>
decltype(auto) gut_like(S&& s, A const&)
{
	return detail::rget<A>(std::forward<S>(s));
}

template <typename S, typename A>
decltype(auto) get_like(S&& s, A const&)
{
	return detail::uget<A>(std::forward<S>(s));
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

template <int Low, int High, int Mid>
struct _rvisit_at<bottom, Low, High, Mid, enable_if_t<(Low > High)>>
{
	template <typename... T>
	[[noreturn]] static auto apply(int, T&&... t) -> decltype(
	    _rvisit_at<bottom, Mid, Mid, Mid>::apply(0, std::forward<T>(t)...))
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
	template <typename Raw, typename F>
	static R apply(int n, F&& f, Raw&& tp)
	{
		return std::forward<F>(f)(detail::xget<Mid>(
		    std::forward<Raw>(tp), is_overload_call_wrapper<F>()));
	}
};

template <int Mid>
struct _rvisit_at<void, Mid, Mid, Mid>
{
	template <typename Raw, typename F>
	static void apply(int n, F&& f, Raw&& tp)
	{
		return void(std::forward<F>(f)(detail::xget<Mid>(
		    std::forward<Raw>(tp), is_overload_call_wrapper<F>())));
	}
};

template <int Mid>
struct _rvisit_at<bottom, Mid, Mid, Mid>
{
	template <typename Raw, typename F>
	static decltype(auto) apply(int n, F&& f, Raw&& tp)
	{
		return std::forward<F>(f)(detail::xget<Mid>(
		    std::forward<Raw>(tp), is_overload_call_wrapper<F>()));
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

template <typename R = bottom, typename Raw, typename F>
inline decltype(auto) rvisit_at(int n, F&& f, Raw&& tp)
{
	constexpr int m = std::decay_t<Raw>::size;
	return _rvisit_at<R, 0, m - 1>::apply(n, std::forward<F>(f),
	                                      std::forward<Raw>(tp));
}

template <bool all_trivial_dtor, typename... T>
struct variant_layout;

template <typename... T>
struct variant_layout<true, T...>
{
	static constexpr bool trivial = true;

	int index;
	variant_storage<T...> data;
};

template <typename... T>
struct variant_layout<false, T...>
{
	static constexpr bool trivial = false;

	struct dtor
	{
		template <typename A>
		void operator()(A const& a) const
		{
			a.~A();
		}
	};

	~variant_layout() { rvisit_at(index, dtor(), data); }

	int index = 0;
	variant_storage<T...> data;
};

template <template <typename...> class F, typename... T>
constexpr bool alternatives_satisfy =
    and_v<F<variant_internal_t<T>>::value...>;

template <template <typename...> class F, typename... T>
constexpr bool alternatives_featuring = and_v<F<variant_model_t<T>>::value...>;

template <typename... T>
using variant_layout_t =
    variant_layout<alternatives_satisfy<is_trivially_destructible, T...>,
                   variant_internal_t<T>...>;

template <bool all_trivially_copyable, typename... T>
struct oneof_rep;

template <typename... T>
struct oneof_rep<true, T...> : variant_layout_t<T...>
{
};

template <typename... T>
struct oneof_rep<false, T...> : variant_layout_t<T...>
{
	constexpr oneof_rep() = default;

	oneof_rep(oneof_rep const& other)
	{
		rvisit_at(
		    this->index = other.index,
		    [&](auto&& ra) { ::new ((void*)&this->data) auto(ra); },
		    other.data);
	}

	oneof_rep(oneof_rep&& other) noexcept
	{
		rvisit_at(this->index = other.index,
		          [&](auto&& ra) {
			          ::new ((void*)&this->data) auto(
			              std::move(ra));
			  },
		          other.data);
	}

	oneof_rep& operator=(oneof_rep const& other)
	{
		if (not this->trivial and this->index == other.index)
		{
			rvisit_at(
			    other.index,
			    [&](auto&& ra) { gut_like(this->data, ra) = ra; },
			    other.data);
		}
		else if (alternatives_satisfy<is_nothrow_copy_constructible,
		                              T...>)
		{
			if (&other != this)
			{
				this->~oneof_rep();
				return *::new ((void*)this) oneof_rep{ other };
			}
		}
		else
		{
			rvisit_at(other.index,
			          [&](auto&& ra) {
				          auto tmp = ra;
				          this->~oneof_rep();
				          ::new ((void*)&this->data) auto(
				              std::move(tmp));
				  },
			          other.data);
			this->index = other.index;
		}

		return *this;
	}

	oneof_rep& operator=(oneof_rep&& other) noexcept(
	    alternatives_satisfy<is_nothrow_move_assignable, T...>)
	{
		if (not this->trivial and this->index == other.index)
		{
			rvisit_at(other.index,
			          [&](auto&& ra) {
				          gut_like(this->data, ra) =
				              std::move(ra);
				  },
			          other.data);

			return *this;
		}
		else
		{
			this->~oneof_rep();
			return *::new ((void*)this)
			    oneof_rep{ std::move(other) };
		}
	}
};

template <typename... T>
using oneof_rep_t =
    oneof_rep<alternatives_satisfy<is_trivially_copyable, T...>, T...>;

}

#if defined(_MSC_VER) && _HAS_CXX17

using std::is_nothrow_swappable_v;
using std::is_nothrow_swappable;
using std::is_swappable_v;
using std::is_swappable;

#else

template <typename T>
constexpr bool is_nothrow_swappable_v =
    decltype(detail::adl::is_nothrow_swappable_impl::test<T>(0))::value;

template <typename T>
struct is_nothrow_swappable : bool_constant<is_nothrow_swappable_v<T>>
{
};

template <typename T>
constexpr bool is_swappable_v = detail::adl::is_swappable_impl<T>;

template <typename T>
struct is_swappable : bool_constant<is_swappable_v<T>>
{
};

#endif

template <typename... T>
struct oneof
    : private copy_constructible<
          detail::alternatives_featuring<is_copy_constructible, T...>>,
      private move_constructible<
          detail::alternatives_featuring<is_move_constructible, T...>>,
      private copy_assignable<detail::alternatives_featuring<
          detail::both<is_copy_constructible, is_copy_assignable>::call,
          T...>>,
      private move_assignable<detail::alternatives_featuring<
          detail::both<is_move_constructible, is_move_assignable>::call, T...>>
{
	static_assert(detail::alternatives_featuring<detail::is_decayed, T...>,
	              "an alternative type must not be cv-qualified, "
	              "reference, function, or array of known bound");

	oneof() = default;
	oneof(oneof const& other) = default;
	oneof(oneof&& other) = default;
	oneof& operator=(oneof const& other) = default;
	oneof& operator=(oneof&& other) = default;

	template <typename A, disable_capturing<oneof, A> = 0,
	          typename E = detail::element_to_construct_t<A, T...>>
	oneof(A&& a)
	{
		constexpr int i = detail::find_alternative_v<E, oneof>;
		using type = detail::choose_alternative_t<i, oneof>;

		::new ((void*)&rep_.data) type(std::forward<A>(a));
		rep_.index = i;
	}

	template <typename A, disable_capturing<oneof, A> = 0,
	          typename E = detail::element_to_construct_t<A, T...>>
	oneof& operator=(A&& a)
	{
		emplace<E>(std::forward<A>(a));
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
			auto p = ::new ((void*)&rep_.data)
			    type(std::forward<Args>(args)...);
			rep_.index = i;
			return *p;
		}
		else
		{
			type tmp(std::forward<Args>(args)...);
			this->~oneof();
			auto p =
			    ::new ((void*)&rep_.data) type(std::move(tmp));
			rep_.index = i;
			return *p;
		}
	}

	template <typename R = bottom, typename... F>
	decltype(auto) match(F&&... f) &
	{
		return detail::rvisit_at<R>(
		    rep_.index, overload(std::forward<F>(f)...), rep_.data);
	}

	template <typename R = bottom, typename... F>
	decltype(auto) match(F&&... f) const &
	{
		return detail::rvisit_at<R>(
		    rep_.index, overload(std::forward<F>(f)...), rep_.data);
	}

	template <typename R = bottom, typename... F>
	decltype(auto) match(F&&... f) &&
	{
		return detail::rvisit_at<R>(rep_.index,
		                            overload(std::forward<F>(f)...),
		                            std::move(rep_.data));
	}

	template <typename R = bottom, typename... F>
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
			return v.match<bool>([&](auto&& x) {
				return x == detail::get_like(w.rep_.data, x);
			});
	}

	friend bool operator!=(oneof const& v, oneof const& w)
	{
		if (v.which() != w.which())
			return true;
		else
			return v.match<bool>([&](auto&& x) {
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
			return v.match<bool>([&](auto&& x) {
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
			return v.match<bool>([&](auto&& x) {
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
			return v.match<bool>([&](auto&& x) {
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
			return v.match<bool>([&](auto&& x) {
				return x >= detail::get_like(w.rep_.data, x);
			});
	}

	template <bool swappable = detail::alternatives_featuring<
	              detail::both<is_move_constructible, is_move_assignable,
	                           is_swappable>::call,
	              T...>>
	friend enable_if_t<swappable> swap(oneof& v, oneof& w) noexcept(
	    noexcept(std::swap(v, w)) and
	    detail::alternatives_satisfy<is_nothrow_swappable, T...>)
	{
		if (v.which() == w.which())
			detail::rvisit_at(
			    v.rep_.index,
			    [&](auto&& x) {
				    using std::swap;
				    swap(x, detail::gut_like(w.rep_.data, x));
			    },
			    v.rep_.data);
		else
			std::swap(v, w);
	}

private:
	detail::oneof_rep_t<T...> rep_;
};

template <>
struct oneof<>
{
	oneof() = delete;
	oneof(oneof const&) = delete;
	oneof& operator=(oneof const&) = delete;
};

}
