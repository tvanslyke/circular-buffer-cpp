#ifndef TIM_CIRCULAR_BUFFER_HPP
#define TIM_CIRCULAR_BUFFER_HPP
/**
 * @mainpage CircularBuffer Docs
 * 
 * # Circular Buffer Documentation
 * <a href="https://github.com/tvanslyke/circular-buffer-cpp">Main Project Page</a>
 * 
 * tim::CircularBuffer Is a sequence container that encapsulates a resizable ring/circular buffer.
 *
 * ## Quick Reference
 * * tim::CircularBuffer - A resizable circular buffer.
 */


#include <utility>
#include <memory>
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <type_traits>
#include <optional>
#include <iterator>
#include <stdexcept>
#include <array>
#include <climits>
#include <limits>
#include <functional>

/**
 * @def TIM_CICULAR_BUFFER_NO_USE_INTRINSICS
 * Prevents the implementation from using compiler intrinsics for optimization.
 * Currently intrinsics are only used for turning assertions into assumptions.
 */
#if defined(TIM_CICULAR_BUFFER_NO_USE_INTRINSICS) && defined(_MSC_VER)
#include <immintrin.h>
#endif

#if !defined(TIM_CICULAR_BUFFER_NO_USE_INTRINSICS) && defined(NDEBUG)
# if defined(__clang__)
#  define TIM_CIRCULAR_BUFFER_ASSERT(x) __builtin_assume(x)
# elif defined(__GNUC__)
#  define TIM_CIRCULAR_BUFFER_ASSERT(x) if(x) {} else { __builtin_unreachable(); }
# elif defined(_MSC_VER)
#  define TIM_CIRCULAR_BUFFER_ASSERT(x) __assume(x)
# else
#  define TIM_CIRCULAR_BUFFER_ASSERT(x) assert(x)
# endif
#else
# define TIM_CIRCULAR_BUFFER_ASSERT(x) assert(x)
#endif

#if !defined(TIM_CICULAR_BUFFER_NO_USE_INTRINSICS) && defined(NDEBUG)
# if defined(__GNUC__)
#  define TIM_CIRCULAR_BUFFER_ASSERT_UNREACHABLE() __builtin_unreachable();
# elif defined(_MSC_VER)
#  define TIM_CIRCULAR_BUFFER_ASSERT_UNREACHABLE() __assume(0)
# else
#  define TIM_CIRCULAR_BUFFER_ASSERT_UNREACHABLE() assert(!"Unreachable path reached.")
# endif
#else
# define TIM_CIRCULAR_BUFFER_ASSERT_UNREACHABLE() assert(!"Unreachable path reached.")
#endif


namespace tim {

inline namespace circular_buffer {

template <class T, class Allocator = std::allocator<T>>
struct CircularBuffer;

/// @cond
namespace detail {

template <class It, class Dest, class Pred>
constexpr Dest do_remove_copy_if(It first, It last, Dest dest, Pred pred)
{
	while (first != last)
	{
		if (!pred(*first))
		{
			*dest++ = std::move(*first);
		}
		++first;
	}
	return dest;
}

template <class It, class Dest, class Pred>
constexpr Dest do_remove_copy_if_backward(It first, It last, Dest dest_last, Pred pred)
{
	while (first != last)
	{
		--last;
		if (!pred(*last))
		{
			--dest_last;
			*dest_last = std::move(*last);
		}
	}
	return dest_last;
}


template <class It, class Pred>
constexpr std::pair<It, It> bidirectional_remove_if(It first, It last, Pred pred)
{
	first = std::find_if(first, last, std::not_fn(pred));
	if (first == last)
	{
		return { first, first };
	}
	last = std::find_if(std::make_reverse_iterator(last), std::make_reverse_iterator(first), std::not_fn(pred)).base();
	// It can't be the case that first == last because we would've returned in the above if() block.
	TIM_CIRCULAR_BUFFER_ASSERT(first < last);
	if (std::next(first) == last)
	{
		return { first, last };
	}
	auto head_pos = std::find_if(std::next(first), last, pred);
	if (head_pos == last)
	{
		return { first, last };
	}
	TIM_CIRCULAR_BUFFER_ASSERT(head_pos < last);
	auto tail_search_end = std::next(head_pos);
	auto tail_pos = std::find_if(std::make_reverse_iterator(last), std::make_reverse_iterator(tail_search_end), pred).base();
	auto head_count = head_pos - first;
	auto tail_count = last - tail_pos;
	if (tail_pos == tail_search_end)
	{
		if (head_count > tail_count)
		{
			last = std::move(tail_pos, last, head_pos);
		}
		else
		{
			first = std::move_backward(first, head_pos, tail_search_end);
		}
	}
	else if (head_count > tail_count)
	{
		auto p = do_remove_copy_if(std::next(head_pos), std::prev(tail_pos), head_pos, pred);
		last = std::move(tail_pos, last, p);
	}
	else
	{
		auto p = do_remove_copy_if_backward(std::next(head_pos), std::prev(tail_pos), tail_pos, pred);
		first = std::move_backward(first, head_pos, p);
	}
	return { first, last };
}


template <
	class It,
	class SizeType,
	std::enable_if_t<
		std::is_same_v<
			std::bidirectional_iterator_tag,
			typename std::iterator_traits<It>::iterator_category
		>,
		bool
	> = false
>
constexpr It get_position_in_range(It first, It last, SizeType count, SizeType index) {
	TIM_CIRCULAR_BUFFER_ASSERT(count >= index);
	auto dist_from_end = count - index;
	if(index < dist_from_end) {
		return std::next(first, index);
	} else {
		return std::prev(last, dist_from_end);
	}
}

template <
	class It,
	class SizeType,
	std::enable_if_t<
		!std::is_same_v<
			std::bidirectional_iterator_tag,
			typename std::iterator_traits<It>::iterator_category
		>,
		bool
	> = false
>
constexpr It get_position_in_range(It first, It last, SizeType count, SizeType index) {
	TIM_CIRCULAR_BUFFER_ASSERT(count >= index);
	return std::next(first, index);
}

template <class It>
struct SimpleRange {
	using value_type = typename std::iterator_traits<It>::value_type;
	
	SimpleRange() = default;

	constexpr SimpleRange(It first, It last):
		begin_(first),
		end_(last)
	{
		
	}

	constexpr It begin() const {
		return begin_;
	}

	constexpr It end() const {
		return end_;
	}

	constexpr std::reverse_iterator<It> rbegin() const {
		return std::make_reverse_iterator(end());
	}

	constexpr std::reverse_iterator<It> rend() const {
		return std::make_reverse_iterator(begin());
	}

	friend constexpr It begin(const SimpleRange& range) {
		return range.begin();
	}

	friend constexpr It end(const SimpleRange& range) {
		return range.end();
	}

	friend constexpr std::reverse_iterator<It> rbegin(const SimpleRange& range) {
		return std::make_reverse_iterator(range.end());
	}

	friend constexpr std::reverse_iterator<It> rend(const SimpleRange& range) {
		return std::make_reverse_iterator(range.begin());
	}

	constexpr SimpleRange<std::move_iterator<It>> as_moving_range() const {
		return SimpleRange<std::move_iterator<It>>(std::make_move_iterator(begin_), std::make_move_iterator(end_));
	}

private:
	It begin_;
	It end_;
};

template <class It>
SimpleRange(It, It) -> SimpleRange<It>;

template <class C>
constexpr decltype(auto) reversed(C&& container) {
	using std::rbegin;
	using std::rend;
	return SimpleRange(rbegin(std::forward<C>(container)), rend(std::forward<C>(container)));
}

template <class T>
struct SingleValueIterator
{
	using value_type        = T;
	using reference         = const T&;
	using pointer           = const T*;
	using difference_type   = std::ptrdiff_t;
	using iterator_category = std::random_access_iterator_tag;


	SingleValueIterator() = default;
	SingleValueIterator(const SingleValueIterator&) = default;
	SingleValueIterator(SingleValueIterator&&) = default;

	SingleValueIterator& operator=(const SingleValueIterator&) = default;
	SingleValueIterator& operator=(SingleValueIterator&&) = default;

	constexpr SingleValueIterator(const T* value, std::size_t pos):
		value_(value),
		pos_(pos)
	{

	}
	
	constexpr SingleValueIterator& operator++() {
		++pos_;
		return *this;
	}

	constexpr SingleValueIterator operator++(int) {
		auto cpy = *this;
		++*this;
		return cpy;
	}

	constexpr SingleValueIterator& operator--() {
		TIM_CIRCULAR_BUFFER_ASSERT(pos_ != 0);
		--pos_;
		return *this;
	}

	constexpr SingleValueIterator operator--(int) {
		auto cpy = *this;
		--(*this);
		return cpy;
	}

	constexpr reference operator*() const {
		return *value_;
	}

	constexpr pointer operator->() const {
		return value_;
	}

	constexpr SingleValueIterator& operator+=(difference_type rhs) {
		pos_ += rhs;
		return *this;
	}

	constexpr SingleValueIterator& operator-=(difference_type rhs) {
		TIM_CIRCULAR_BUFFER_ASSERT(pos_ >= rhs);
		pos_ -= rhs;
		return *this;
	}

	friend constexpr SingleValueIterator operator+(SingleValueIterator lhs, difference_type rhs) {
		lhs += rhs;
		return lhs;
	}

	friend constexpr SingleValueIterator operator+(difference_type lhs, SingleValueIterator rhs) {
		rhs.pos_ += lhs;
		return rhs;
	}

	friend constexpr SingleValueIterator operator-(SingleValueIterator lhs, difference_type rhs) {
		lhs -= rhs;
		return lhs;
	}

	friend constexpr difference_type operator-(SingleValueIterator lhs, SingleValueIterator rhs) {
		return lhs.pos_ - rhs.pos_;
	}

	constexpr reference operator[](difference_type ofs) const {
		return *value_;
	}

	friend constexpr bool operator==(SingleValueIterator lhs, SingleValueIterator rhs) {
		return lhs.pos_ == rhs.pos_ && lhs.value_ == rhs.value_;
	}
	
	friend constexpr bool operator!=(SingleValueIterator lhs, SingleValueIterator rhs) {
		return not (lhs == rhs);
	}

	friend constexpr bool operator<(SingleValueIterator lhs, SingleValueIterator rhs) {
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.value_ == rhs.value_);
		return lhs.pos_ < rhs.pos_;
	}

	friend constexpr bool operator<=(SingleValueIterator lhs, SingleValueIterator rhs) {
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.value_ == rhs.value_);
		return lhs.pos_ <= rhs.pos_;
	}
	
	friend constexpr bool operator>(SingleValueIterator lhs, SingleValueIterator rhs) {
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.value_ == rhs.value_);
		return lhs.pos_ > rhs.pos_;
	}
	
	friend constexpr bool operator>=(SingleValueIterator lhs, SingleValueIterator rhs) {
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.value_ == rhs.value_);
		return lhs.pos_ >= rhs.pos_;
	}

private:
	const T* value_ = nullptr;
	difference_type pos_ = 0;
};

template <class T>
struct DefaultValueIterator
{
	using value_type        = T;
	using reference         = const T&;
	using pointer           = const T*;
	using difference_type   = std::ptrdiff_t;
	using iterator_category = std::random_access_iterator_tag;


	DefaultValueIterator() = default;
	DefaultValueIterator(const DefaultValueIterator&) = default;
	DefaultValueIterator(DefaultValueIterator&&) = default;

	DefaultValueIterator& operator=(const DefaultValueIterator&) = default;
	DefaultValueIterator& operator=(DefaultValueIterator&&) = default;

	explicit constexpr DefaultValueIterator(std::size_t pos):
		pos_(pos)
	{

	}
	
	constexpr DefaultValueIterator& operator++() {
		++pos_;
		return *this;
	}

	constexpr DefaultValueIterator operator++(int) {
		auto cpy = *this;
		++*this;
		return cpy;
	}

	constexpr DefaultValueIterator& operator--() {
		TIM_CIRCULAR_BUFFER_ASSERT(pos_ != 0);
		--pos_;
		return *this;
	}

	constexpr DefaultValueIterator operator--(int) {
		auto cpy = *this;
		--(*this);
		return cpy;
	}

	constexpr value_type operator*() const {
		return value_type();
	}

	constexpr std::optional<value_type> operator->() const {
		return std::optional<value_type>(std::in_place);
	}

	constexpr DefaultValueIterator& operator+=(difference_type rhs) {
		pos_ += rhs;
		return *this;
	}

	constexpr DefaultValueIterator& operator-=(difference_type rhs) {
		TIM_CIRCULAR_BUFFER_ASSERT(pos_ >= rhs);
		pos_ -= rhs;
		return *this;
	}

	friend constexpr DefaultValueIterator operator+(DefaultValueIterator lhs, difference_type rhs) {
		lhs += rhs;
		return lhs;
	}

	friend constexpr DefaultValueIterator operator+(difference_type lhs, DefaultValueIterator rhs) {
		rhs.pos_ += lhs;
		return rhs;
	}

	friend constexpr DefaultValueIterator operator-(DefaultValueIterator lhs, difference_type rhs) {
		lhs -= rhs;
		return lhs;
	}

	friend constexpr difference_type operator-(DefaultValueIterator lhs, DefaultValueIterator rhs) {
		return lhs.pos_ - rhs.pos_;
	}

	constexpr value_type operator[](difference_type ofs) const {
		return value_type();
	}

	friend constexpr bool operator==(DefaultValueIterator lhs, DefaultValueIterator rhs) {
		return lhs.pos_ == rhs.pos_;
	}
	
	friend constexpr bool operator!=(DefaultValueIterator lhs, DefaultValueIterator rhs) {
		return not (lhs == rhs);
	}

	friend constexpr bool operator<(DefaultValueIterator lhs, DefaultValueIterator rhs) {
		return lhs.pos_ < rhs.pos_;
	}

	friend constexpr bool operator<=(DefaultValueIterator lhs, DefaultValueIterator rhs) {
		return lhs.pos_ <= rhs.pos_;
	}
	
	friend constexpr bool operator>(DefaultValueIterator lhs, DefaultValueIterator rhs) {
		return lhs.pos_ > rhs.pos_;
	}
	
	friend constexpr bool operator>=(DefaultValueIterator lhs, DefaultValueIterator rhs) {
		return lhs.pos_ >= rhs.pos_;
	}

private:
	difference_type pos_ = 0;
};



template <class T>
constexpr auto make_single_value_range(const T& value, std::size_t count)
	-> SimpleRange<SingleValueIterator<T>>
{
	return SimpleRange<SingleValueIterator<T>>(
		SingleValueIterator<T>(std::addressof(value), 0),
		SingleValueIterator<T>(std::addressof(value), static_cast<std::ptrdiff_t>(count))
	);
}


namespace traits {

namespace detail {
template <class T>
constexpr decltype(auto) begin_proxy(T&& v)
{
	using std::begin;
	return begin(v);
}

} /* namespace detail */

template <class C>
struct iterator_type {
	template <class T>
	struct trait { using type = T; };


	template <class Range, class Iter = std::decay_t<decltype(detail::begin_proxy(std::declval<Range&>()))>>
	static constexpr auto helper(int) -> trait<Iter> {

	}

	template <class Range>
	static constexpr auto helper(...) -> trait<void> {

	}

	using type = typename decltype(helper<C>(0))::type;
};

} /* namespace traits */

template <class Int, class ... Args>
constexpr Int add_modulo(Int base, Int value, Args ... args) {
	auto do_add = [base, &value](auto arg) {
		if(arg >= base) {
			arg %= base;
		}
		if(base - value <= arg) {
			value = arg - (base - value);
		}
	};
	(..., do_add(args));
	// std::apply(do_add, std::make_tuple(args ...));
	return value;
}


template <class It>
constexpr It get_point_in_iterator_range(It first, It last, typename std::iterator_traits<It>::difference_type idx, typename std::iterator_traits<It>::difference_type count)
{
	using cat = typename std::iterator_traits<It>::iterator_category;
	if constexpr (std::is_same_v<cat, std::bidirectional_iterator_tag>)
	{
		if (idx <= count - idx)
		{
			return std::next(first, idx);
		}
		else
		{
			return std::prev(last, count - idx);
		}
	}
	else
	{
		return std::next(first, idx);
	}
}

template <class It, class Dest>
constexpr auto copy_ranges(It first, It last, Dest dest)
	-> std::pair<Dest, std::optional<typename traits::iterator_type<typename std::iterator_traits<Dest>::value_type>::type>>
{
	using std::begin;
	using std::end;

	if(first == last) {
		return std::make_pair(dest, std::nullopt);
	}
	auto dest_pos = dest->begin();
	auto dest_stop = dest->end();
	auto src_pos = first->begin();
	auto src_stop = first->end();
	for(;;)
	{
		auto src_sz = src_stop - src_pos;
		auto dest_sz = dest_stop - dest_pos;
		if(src_sz <= dest_sz)
		{
			dest_pos = std::copy(src_pos, src_stop, dest_pos);
			TIM_CIRCULAR_BUFFER_ASSERT(dest_stop - dest_pos == dest_sz - src_sz);
			++first;
			if(first == last)
			{
				TIM_CIRCULAR_BUFFER_ASSERT(dest_pos == dest_stop);
				return std::make_pair(std::next(dest), std::nullopt);
			}
			src_pos = first->begin();
			src_stop = first->end();
			if(dest_pos == dest_stop)
			{
				++dest;
				dest_pos = dest->begin();
				dest_stop = dest->end();
			}
		} else {
			auto mid = src_pos + dest_sz;
			TIM_CIRCULAR_BUFFER_ASSERT(mid != src_stop);
			dest_pos = std::copy(src_pos, mid, dest_pos);
			TIM_CIRCULAR_BUFFER_ASSERT(dest_pos == dest_stop);
			src_pos = mid;
			++dest;
			dest_pos = dest->begin();
			dest_stop = dest->end();
		}
	}
}

template <class It, class Dest>
constexpr auto copy_ranges_backward(It first, It last, Dest dest)
	-> std::pair<Dest, std::optional<typename traits::iterator_type<typename std::iterator_traits<Dest>::value_type>::type>>
{
	using std::begin;
	using std::end;

	if(first == last) {
		return std::make_pair(dest, std::nullopt);
	}
	--last;
	--dest;
	auto dest_pos = dest->begin();
	auto dest_stop = dest->end();
	auto src_pos = last->begin();
	auto src_stop = last->end();
	for(;;) {
		auto src_sz = src_stop - src_pos;
		auto dest_sz = dest_stop - dest_pos;
		if(src_sz <= dest_sz) {
			dest_stop = std::copy_backward(src_pos, src_stop, dest_stop);
			TIM_CIRCULAR_BUFFER_ASSERT(dest_stop - dest_pos == dest_sz - src_sz);
			if(first == last) {
				return std::make_pair(dest, dest_stop);
			}
			--last;
			src_pos = last->begin();
			src_stop = last->end();
		} else {
			auto mid = src_stop - dest_sz;
			dest_stop = std::copy_backward(mid, src_stop, dest_stop);
			TIM_CIRCULAR_BUFFER_ASSERT(dest_pos == dest_stop);
			TIM_CIRCULAR_BUFFER_ASSERT(src_pos != src_stop);
			src_stop = mid;
			--dest;
			dest_pos = dest->begin();
			dest_stop = dest->end();
		}
	}
}


template <class T, class ... U>
struct is_one_of : std::bool_constant<(std::is_same_v<T, U> || ...)> {};

template <class T, class ... U>
inline constexpr bool is_one_of_v = is_one_of<T, U...>::value;

template <class A, class T, bool = std::is_trivially_destructible_v<T>>
struct is_trivially_allocator_destructible;

template <class A, class T>
struct is_trivially_allocator_destructible<A, T, false>: std::false_type {};

template <class A, class T>
struct is_trivially_allocator_destructible<A, T, true>: std::false_type {
private:
	template <class Al, class U, class = decltype(std::declval<Al&>().destroy((std::declval<U*>())))>
	static constexpr auto helper(int)
		-> std::bool_constant<std::is_same_v<A, std::allocator<T>>>
	{
		return {};
	}

	template <class Al, class U>
	static constexpr std::true_type helper(...) {
		return {};
	}

public:
	static constexpr const bool value = helper<A, T>(0).value;
};



template <
	class A,
	class T,
	class = decltype(std::allocator_traits<A>::construct(std::declval<A&>(), std::declval<T*>(), std::declval<const T&>()))
>
constexpr std::true_type test_is_copy_insertable(int) { return std::true_type{}; }

template <class A, class T>
constexpr std::false_type test_is_copy_insertable(...) { return std::false_type{}; }

template <class A, class T>
struct is_copy_insertible: decltype(test_is_copy_insertable<A, T>()) {};

template <class A, class T>
inline constexpr bool is_copy_insertible_v = is_copy_insertible<A, T>::value;

template <
	class A,
	class T,
	class = decltype(std::allocator_traits<A>::construct(std::declval<A&>(), std::declval<T*>(), std::declval<T&&>()))
>
constexpr std::true_type test_is_move_insertable(int) { return std::true_type{}; }

template <class A, class T>
constexpr std::false_type test_is_move_insertable(...) { return std::false_type{}; }

template <class A, class T>
struct is_move_insertible: decltype(test_is_move_insertable<A, T>()) {};

template <class A, class T>
inline constexpr bool is_move_insertible_v = is_move_insertible<A, T>::value;

template <class It>
struct is_move_iterator : std::false_type {};

template <class It>
struct is_move_iterator<std::move_iterator<It>> : std::true_type {};

template <class It>
inline constexpr bool is_move_iterator_v = is_move_iterator<It>::value;

template <class T>
struct ManualScopeGuard {

	~ManualScopeGuard() {
		if(active) {
			action();
		}
	}

	T action;
	bool active;
};

template <class T>
ManualScopeGuard<std::decay_t<T>> make_manual_scope_guard(T&& action) {
	return ManualScopeGuard<std::decay_t<T>>{std::forward<T>(action), true};
}

template <class T>
inline constexpr std::size_t bit_count = static_cast<std::size_t>(CHAR_BIT) * sizeof(T);

template <class Allocator, bool = std::is_empty_v<Allocator> && !std::is_final_v<Allocator>>
struct AllocatorBase;

template <class Allocator>
struct AllocatorBase<Allocator, true>:
	private Allocator
{
	AllocatorBase() = default;
	AllocatorBase(const AllocatorBase&) = default;
	AllocatorBase(AllocatorBase&&) = default;
	AllocatorBase& operator=(const AllocatorBase&) = default;
	AllocatorBase& operator=(AllocatorBase&&) = default;

	constexpr AllocatorBase(const Allocator& a):
		Allocator(a)
	{
		
	}
	
	constexpr AllocatorBase(Allocator&& a):
		Allocator(std::move(a))
	{
		
	}

	constexpr const Allocator& alloc() const {
		return static_cast<const Allocator&>(*this);
	}

	constexpr Allocator& alloc() {
		return static_cast<Allocator&>(*this);
	}

};

template <class Allocator>
struct AllocatorBase<Allocator, false> {

	AllocatorBase() = default;
	AllocatorBase(const AllocatorBase&) = default;
	AllocatorBase(AllocatorBase&&) = default;
	AllocatorBase& operator=(const AllocatorBase&) = default;
	AllocatorBase& operator=(AllocatorBase&&) = default;

	constexpr AllocatorBase(const Allocator& a):
		alloc_(a)
	{
		
	}
	
	constexpr AllocatorBase(Allocator&& a):
		alloc_(std::move(a))
	{
		
	}

	constexpr const Allocator& alloc() const {
		return alloc_;
	}

	constexpr Allocator& alloc() {
		return alloc_;
	}
private:
	Allocator alloc_;
};


template <class Allocator>
class AllocatorRef {
private:
	using alloc_traits = std::allocator_traits<Allocator>;
public:
	using pointer            = typename alloc_traits::pointer;
	using const_pointer      = typename alloc_traits::const_pointer;
	using void_pointer       = typename alloc_traits::void_pointer;
	using const_void_pointer = typename alloc_traits::const_void_pointer;
	using value_type         = typename alloc_traits::value_type;
	using size_type          = typename alloc_traits::size_type;
	using difference_type    = typename alloc_traits::difference_type;

	using is_always_equal                        = typename alloc_traits::is_always_equal;
	using propagate_on_container_swap            = typename alloc_traits::propagate_on_container_swap;
	using propagate_on_container_move_assignment = typename alloc_traits::propagate_on_container_move_assignment;
	using propagate_on_container_copy_assignment = typename alloc_traits::propagate_on_container_copy_assignment;
	

	template <class U>
	struct rebind {
		using other = typename alloc_traits::template rebind_alloc<U>::other;
	};

	AllocatorRef() = delete;
	AllocatorRef(const AllocatorRef&) = default;
	AllocatorRef(AllocatorRef&&) = default;
	AllocatorRef& operator=(const AllocatorRef&) = default;
	AllocatorRef& operator=(AllocatorRef&&) = default;

	constexpr AllocatorRef(Allocator& a):
		ref_(std::addressof(a))
	{
		
	}

	explicit constexpr operator Allocator&() const {
		return *ref_;
	}

	constexpr AllocatorRef select_on_container_copy_construction() const {
		return *this;
	}

	[[nodiscard]]
	constexpr pointer allocate(size_type sz) const {
		return alloc_traits::allocate(*ref_, sz);
	}

	constexpr void deallocate(pointer p, size_type sz) const {
		return alloc_traits::deallocate(*ref_, p, sz);
	}

	template <
		class T,
		class ... Args,
		class = decltype(alloc_traits::construct(std::declval<Allocator&>(), std::declval<T*>(), std::declval<Args&&>()...))
	>
	constexpr void construct(T* ptr, Args&& ... args) const {
		alloc_traits::construct(*ref_, ptr, std::forward<Args>(args)...);
	}

	template <class P, class = decltype(alloc_traits::destroy(std::declval<Allocator&>(), std::declval<P>()))>
	constexpr auto destroy(P ptr) const
		-> decltype(alloc_traits::destroy(std::declval<Allocator&>(), std::declval<P>()))
	{
		return alloc_traits::destroy(*ref_, ptr);
	}

	constexpr size_type max_size() const {
		return alloc_traits::max_size(*ref_);
	}

	friend constexpr bool operator==(AllocatorRef lhs, AllocatorRef rhs) noexcept {
		return is_always_equal::value || (lhs.ref_ == rhs.ref_) || (*lhs.ref_ == *rhs.ref_);
	}

	friend constexpr bool operator!=(AllocatorRef lhs, AllocatorRef rhs) noexcept {
		return !(lhs == rhs);
	}

private:
	Allocator* ref_;
}; 


template <bool IsConst, class T, class Allocator>
struct CircularBufferIteratorBase
{
protected:
	using size_type = typename std::allocator_traits<Allocator>::size_type;

	static constexpr size_type tag_mask = ~(~size_type(0) >> 1);
	using difference_type   = typename std::allocator_traits<Allocator>::difference_type;
	using iterator_category = std::random_access_iterator_tag;
	using reference         = std::conditional_t<IsConst, const T&, T&>;
	using pointer           = std::conditional_t<IsConst, typename std::allocator_traits<Allocator>::const_pointer, typename std::allocator_traits<Allocator>::pointer>;
	using value_type        = T;


	constexpr CircularBufferIteratorBase(pointer data, size_type cap, size_type index, bool is_wrapped):
		data_(data),
		cap_(cap),
		tagged_index_(index)
	{
		if(is_wrapped) {
			set_has_wrapped(true);
		}
		_assert_invariants();
	}

	constexpr CircularBufferIteratorBase(pointer data, size_type cap, size_type tagged_index):
		data_(data),
		cap_(cap),
		tagged_index_(tagged_index)
	{
		_assert_invariants();
	}


	constexpr void _assert_invariants() const
	{
		TIM_CIRCULAR_BUFFER_ASSERT((cap_ & tag_mask) == 0u);
		TIM_CIRCULAR_BUFFER_ASSERT((cap_ == 0) ? (tagged_index_ == 0) : ((tagged_index_ & ~tag_mask) < cap_ && data_));
		
		if((tagged_index_ & tag_mask) != 0u)
		{
			TIM_CIRCULAR_BUFFER_ASSERT(cap_ != 0);
			TIM_CIRCULAR_BUFFER_ASSERT(tagged_index_ > cap_);
			TIM_CIRCULAR_BUFFER_ASSERT((tagged_index_ & ~tag_mask) < cap_);
			TIM_CIRCULAR_BUFFER_ASSERT(data_);
		}
	}

public:
	constexpr CircularBufferIteratorBase() = default;
	constexpr CircularBufferIteratorBase(const CircularBufferIteratorBase&) = default;
	constexpr CircularBufferIteratorBase(CircularBufferIteratorBase&&) = default;

	constexpr CircularBufferIteratorBase& operator=(const CircularBufferIteratorBase&) = default;
	constexpr CircularBufferIteratorBase& operator=(CircularBufferIteratorBase&&) = default;


protected:
	constexpr pointer get_pointer() const {
		_assert_invariants();
		return data_ + get_index();
	}

	constexpr value_type* get_raw_pointer() const {
		_assert_invariants();
		return std::addressof(data_[get_index()]);
	}
	
	constexpr void incr()
	{
		_assert_invariants();
		++tagged_index_;
		// Don't need to check for the tag; If it's equal to capacity it's untagged.
		if (tagged_index_ == cap_) [[unlikely]]
		{
			TIM_CIRCULAR_BUFFER_ASSERT((tagged_index_ & tag_mask) == 0ul && "Buffer capacity is too large.");
			tagged_index_ = tag_mask;
		}
		_assert_invariants();
	}
	
	constexpr void decr()
	{
		_assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(!(tagged_index_ == 0u) && "Trying to wrap around the beginning of the buffer, but the iterator isn't marked as wrapped (invalid iterator).");
		if (tagged_index_ != tag_mask) [[likely]]
		{
			--tagged_index_;
		}
		else [[unlikely]]
		{
			// wrap around to the back of the buffer.
			tagged_index_ = cap_ - 1;
		}
		_assert_invariants();
	}

	constexpr void add(size_type count)
	{
		_assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(count <= cap_);
		size_type summed = tagged_index_ + count;
		TIM_CIRCULAR_BUFFER_ASSERT(summed >= tagged_index_ || summed >= count);
		if(summed < cap_ || get_has_wrapped())
		{
			tagged_index_ = summed;
			_assert_invariants();
			return;
		}
		if(cap_ != 0u)
		{
			TIM_CIRCULAR_BUFFER_ASSERT(count != 0u);
			tagged_index_ = (summed - cap_) | tag_mask;
		}
		else
		{
			TIM_CIRCULAR_BUFFER_ASSERT(tagged_index_ == 0u);
			TIM_CIRCULAR_BUFFER_ASSERT(count == 0u);
		}
		_assert_invariants();
	}

	constexpr void subtract(size_type count) {
		_assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(count <= cap_);
		if (!get_has_wrapped()) {
			TIM_CIRCULAR_BUFFER_ASSERT(count <= get_index());
			tagged_index_ -= count;
			_assert_invariants();
			return;
		}
		const auto idx = get_index();
		if(idx >= count) {
			tagged_index_ -= count;
		} else {
			tagged_index_ = cap_ - (count - idx);
		}
		_assert_invariants();
	}

	constexpr bool get_has_wrapped() const {
		return (tagged_index_ & tag_mask) != 0u;
	}

	constexpr void set_has_wrapped(bool value) {
		_assert_invariants();
		if(value) {
			tagged_index_ |= tag_mask;
		} else {
			tagged_index_ &= ~tag_mask;
		}
		_assert_invariants();
	}

	constexpr size_type get_index() const {
		_assert_invariants();
		return tagged_index_ & ~tag_mask;
	}

	constexpr void set_index(size_type new_value) {
		_assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT((new_value & tag_mask) == 0u);
		tagged_index_ = new_value | (tagged_index_ & tag_mask);
		_assert_invariants();
	}

	template <class, class>
	friend struct CircularBuffer;
	friend struct CircularBufferIterator;
	pointer data_ = nullptr;
	size_type cap_ = 0;
	size_type tagged_index_ = 0;
};


} /* namespace detail */
/// @endcond

template <class T, class Allocator>
struct CircularBufferIterator;
template <class T, class Allocator>
struct ConstCircularBufferIterator;

/**
 * @brief
 * A mutable LegacyRandomAccessIterator for CircularBuffer<T, Allocator>.
 */
template <class T, class Allocator>
struct CircularBufferIterator: public detail::CircularBufferIteratorBase<false, T, Allocator> {
private:
	using base_type = detail::CircularBufferIteratorBase<false, T, Allocator>;
	template <class, class>
	friend struct CircularBuffer;

	template <class, class>
	friend struct CircularBufferIterator;

	friend struct ConstCircularBufferIterator<T, Allocator>;
	using size_type = typename base_type::size_type;

	using base_type::data_;
	using base_type::cap_;
	using base_type::tagged_index_;
	using base_type::get_pointer;
	using base_type::get_raw_pointer;
	using base_type::incr;
	using base_type::decr;
	using base_type::add;
	using base_type::subtract;
	using base_type::get_has_wrapped;
	using base_type::set_has_wrapped;
	using base_type::get_index;
	using base_type::set_index;
	using base_type::_assert_invariants;

public:
	using difference_type   = typename base_type::difference_type;
	using iterator_category = typename base_type::iterator_category;
	using reference	        = typename base_type::reference;
	using pointer           = typename base_type::pointer;
	using value_type        = typename base_type::value_type;


private:
	constexpr CircularBufferIterator(pointer data, size_type cap, size_type index, bool is_wrapped):
		base_type(data, cap, index, is_wrapped)
	{

	}

	constexpr CircularBufferIterator(const CircularBufferIterator<T, detail::AllocatorRef<Allocator>>& other) :
		base_type(other.data_, other.cap_, other.tagged_index_)
	{
		
	}

public:
	constexpr CircularBufferIterator() = default;
	constexpr CircularBufferIterator(const CircularBufferIterator&) = default;
	constexpr CircularBufferIterator(CircularBufferIterator&&) = default;

	constexpr CircularBufferIterator& operator=(const CircularBufferIterator&) = default;
	constexpr CircularBufferIterator& operator=(CircularBufferIterator&&) = default;

	constexpr CircularBufferIterator& operator++() {
		TIM_CIRCULAR_BUFFER_ASSERT(data_);
		incr();
		return *this;
	}

	constexpr CircularBufferIterator operator++(int) {
		auto cpy = *this;
		++*this;
		return cpy;
	}

	constexpr CircularBufferIterator& operator--() {
		TIM_CIRCULAR_BUFFER_ASSERT(data_);
		decr();
		return *this;
	}

	constexpr CircularBufferIterator operator--(int) {
		auto cpy = *this;
		--(*this);
		return cpy;
	}

	constexpr reference operator*() const {
		return *get_pointer();
	}

	constexpr pointer operator->() const {
		return get_pointer();
	}

	constexpr CircularBufferIterator& operator+=(difference_type rhs) {
		if(rhs >= 0) {
			add(rhs);
		} else {
			subtract(static_cast<size_type>(-(rhs + 1) + 1));
		}
		return *this;
	}

	constexpr CircularBufferIterator& operator-=(difference_type rhs) {
		if(rhs <= 0) {
			add(static_cast<size_type>(-(rhs + 1) + 1));
		} else {
			subtract(rhs);
		}
		return *this;
	}

	friend constexpr CircularBufferIterator operator+(CircularBufferIterator lhs, difference_type rhs) {
		lhs += rhs;
		return lhs;
	}

	friend constexpr CircularBufferIterator operator+(difference_type lhs, CircularBufferIterator rhs) {
		rhs += lhs;
		return rhs;
	}

	friend constexpr CircularBufferIterator operator-(CircularBufferIterator lhs, difference_type rhs) {
		lhs -= rhs;
		return lhs;
	}

	friend constexpr difference_type operator-(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == lhs.data_);
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.cap_ == lhs.cap_);
		if(lhs.get_has_wrapped()) {
			if(rhs.get_has_wrapped()) {
				return lhs.tagged_index_ - rhs.tagged_index_;
			} else {
				return lhs.get_index() + (rhs.cap_ - rhs.tagged_index_);
			}
		} else {
			if(rhs.get_has_wrapped()) {
				return -static_cast<difference_type>(rhs.get_index() + (lhs.cap_ - lhs.tagged_index_));
			} else {
				return lhs.tagged_index_ - rhs.tagged_index_;
			}
		}
	}

	constexpr reference operator[](difference_type ofs) const {
		return *(*this + ofs);
	}

	friend constexpr bool operator==(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		if(lhs.data_ == rhs.data_) {
			TIM_CIRCULAR_BUFFER_ASSERT(lhs.cap_ == rhs.cap_);
			return lhs.tagged_index_ == rhs.tagged_index_;
		} else {
			return false;
		}
	}
	
	friend constexpr bool operator!=(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		return not (lhs == rhs);
	}

	friend constexpr bool operator<(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ < rhs.tagged_index_;
	}

	friend constexpr bool operator<=(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ <= rhs.tagged_index_;
	}
	
	friend constexpr bool operator>(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ > rhs.tagged_index_;
	}
	
	friend constexpr bool operator>=(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ >= rhs.tagged_index_;
	}

};

/**
 * @brief
 * A const LegacyRandomAccessIterator for CircularBuffer<T, Allocator>.
 */
template <class T, class Allocator>
struct ConstCircularBufferIterator: public detail::CircularBufferIteratorBase<true, T, Allocator>
{
private:
	using base_type = detail::CircularBufferIteratorBase<true, T, Allocator>;
	template <class, class>
	friend struct CircularBuffer;

	template <class, class>
	friend struct ConstCircularBufferIterator;

	friend struct CircularBufferIterator<T, Allocator>;
	using size_type = typename base_type::size_type;

	using base_type::data_;
	using base_type::cap_;
	using base_type::tagged_index_;
	using base_type::get_pointer;
	using base_type::get_raw_pointer;
	using base_type::incr;
	using base_type::decr;
	using base_type::add;
	using base_type::subtract;
	using base_type::get_has_wrapped;
	using base_type::set_has_wrapped;
	using base_type::get_index;
	using base_type::set_index;
	using base_type::_assert_invariants;


public:
	using difference_type   = typename base_type::difference_type;
	using iterator_category = typename base_type::iterator_category;
	using reference	        = typename base_type::reference;
	using pointer           = typename base_type::pointer;
	using value_type        = typename base_type::value_type;


private:
	constexpr ConstCircularBufferIterator(pointer data, size_type cap, size_type index, bool is_wrapped):
		base_type(data, cap, index, is_wrapped)
	{
		
	}

	constexpr ConstCircularBufferIterator(const ConstCircularBufferIterator<T, detail::AllocatorRef<Allocator>>& other) :
		base_type(other.data_, other.cap_, other.tagged_index_)
	{
		
	}

public:
	constexpr ConstCircularBufferIterator() = default;
	constexpr ConstCircularBufferIterator(const ConstCircularBufferIterator&) = default;
	constexpr ConstCircularBufferIterator(ConstCircularBufferIterator&&) = default;

	constexpr ConstCircularBufferIterator& operator=(const ConstCircularBufferIterator&) = default;
	constexpr ConstCircularBufferIterator& operator=(ConstCircularBufferIterator&&) = default;
	
	constexpr ConstCircularBufferIterator(const CircularBufferIterator<T, Allocator>& other) :
		base_type(other.data_, other.cap_, other.tagged_index_)
	{

	}

	constexpr ConstCircularBufferIterator& operator++() {
		TIM_CIRCULAR_BUFFER_ASSERT(data_);
		incr();
		return *this;
	}

	constexpr ConstCircularBufferIterator operator++(int) {
		auto cpy = *this;
		++*this;
		return cpy;
	}

	constexpr ConstCircularBufferIterator& operator--() {
		TIM_CIRCULAR_BUFFER_ASSERT(data_);
		decr();
		return *this;
	}

	constexpr ConstCircularBufferIterator operator--(int) {
		auto cpy = *this;
		--(*this);
		return cpy;
	}

	constexpr reference operator*() const {
		return data_[get_index()];
	}

	constexpr pointer operator->() const {
		return get_pointer();
	}

	constexpr ConstCircularBufferIterator& operator+=(difference_type rhs) {
		if(rhs >= 0) {
			add(rhs);
		} else {
			subtract(static_cast<size_type>(-(rhs + 1) + 1));
		}
		return *this;
	}

	constexpr ConstCircularBufferIterator& operator-=(difference_type rhs) {
		if(rhs <= 0) {
			add(static_cast<size_type>(-(rhs + 1) + 1));
		} else {
			subtract(rhs);
		}
		return *this;
	}

	friend constexpr ConstCircularBufferIterator operator+(ConstCircularBufferIterator lhs, difference_type rhs) {
		lhs += rhs;
		return lhs;
	}

	friend constexpr ConstCircularBufferIterator operator+(difference_type lhs, ConstCircularBufferIterator rhs) {
		rhs += lhs;
		return rhs;
	}

	friend constexpr ConstCircularBufferIterator operator-(ConstCircularBufferIterator lhs, difference_type rhs) {
		lhs -= rhs;
		return lhs;
	}

	friend constexpr difference_type operator-(ConstCircularBufferIterator lhs, ConstCircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == lhs.data_);
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.cap_ == lhs.cap_);
		if(lhs.get_has_wrapped()) {
			if(rhs.get_has_wrapped()) {
				return lhs.tagged_index_ - rhs.tagged_index_;
			} else {
				return lhs.get_index() + (rhs.cap_ - rhs.tagged_index_);
			}
		} else {
			if(rhs.get_has_wrapped()) {
				return -static_cast<difference_type>(rhs.get_index() + (lhs.cap_ - lhs.tagged_index_));
			} else {
				return lhs.tagged_index_ - rhs.tagged_index_;
			}
		}
	}

	constexpr reference operator[](difference_type ofs) const {
		return *(*this + ofs);
	}

	friend constexpr bool operator==(ConstCircularBufferIterator lhs, ConstCircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		if(lhs.data_ == rhs.data_) {
			TIM_CIRCULAR_BUFFER_ASSERT(lhs.cap_ == rhs.cap_);
			return lhs.tagged_index_ == rhs.tagged_index_;
		} else {
			return false;
		}
	}
	
	friend constexpr bool operator!=(ConstCircularBufferIterator lhs, ConstCircularBufferIterator rhs) {
		return not (lhs == rhs);
	}

	friend constexpr bool operator<(ConstCircularBufferIterator lhs, ConstCircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ < rhs.tagged_index_;
	}

	friend constexpr bool operator<=(ConstCircularBufferIterator lhs, ConstCircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ <= rhs.tagged_index_;
	}
	
	friend constexpr bool operator>(ConstCircularBufferIterator lhs, ConstCircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ > rhs.tagged_index_;
	}
	
	friend constexpr bool operator>=(ConstCircularBufferIterator lhs, ConstCircularBufferIterator rhs) {
		lhs._assert_invariants();
		rhs._assert_invariants();
		TIM_CIRCULAR_BUFFER_ASSERT(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ >= rhs.tagged_index_;
	}
};

namespace tags {

/** Tag type used to indicate that the layout of a CircularBuffer should be preserved when making a copy. */
struct PreserveBufferLayout {};
/** Tag object used to indicate that the layout of a CircularBuffer should be preserved when making a copy. */
inline constexpr auto preserve_layout = PreserveBufferLayout{};

/** Tag type used to indicate that the capacity of a CircularBuffer should be preserved when making a copy. */
struct PreserveBufferCapacity {};
/** Tag object used to indicate that the capacity of a CircularBuffer should be preserved when making a copy. */
inline constexpr auto preserve_capacity = PreserveBufferCapacity{};

/** Tag type used to indicate that the layout of a CircularBuffer need not be preserved when making a copy. */
struct Optimized{};
/** Tag object used to indicate that the layout of a CircularBuffer need not be preserved when making a copy. */
inline constexpr auto optimized = Optimized{};

} /* namespace tags */


/** Simple type used to describe the layout of a circular buffer. */
template <class SizeType>
struct BufferShape
{
	/** Capacity of the buffer. */
	SizeType capacity = 0;
	/** Physical index into the buffer of logical index 0 (begin_index()). */
	SizeType start = 0;

	/** Defaulted comparison operators. */
	friend constexpr auto operator<=>(const BufferShape& lhs, const BufferShape& rhs) = default;
};

/// @cond
namespace detail {

struct ReserveTag {};
static constexpr ReserveTag reserve_tag = {};

struct ShapeTag {};
static constexpr ShapeTag shape_tag = {};

}
/// @endcond

/**
 * @brief
 * A sequence container that encapsulates a resizable ring/circular buffer.
 * 
 * This type is similar to std::vector, supporting nearly every member function that std::vector has (with near identical semantics),
 * except that the CircularBuffer supports additional operations and does not guarantee that its elements are stored contiguously.
 * 
 * The elements are stored in a contiguous underlying "physical" buffer but instead of storage always beginning at the start of the
 * "physical" buffer, as is done with std::vector, instead the actual "logical" position of the beginning of the element storage may
 * be at some offset into the physical buffer.
 * Additionally, the "logical" end of the element storage may wrap around to the beginning of the "physical" buffer if the needed.
 * This flexibility in the start and end of the buffer allows for elements to be more efficiently moved around in storage when performing
 * certain operations.
 * For example inserting elements near the front of a std::vector's storage, say at index 2, would require moving all elements after  
 * index 2, whereas with CircularBuffer, only those 2 elements preceding the element at index 2 would need to be moved to make room for
 * the insertion (assuming no reallocation takes place).
 * This flexibility also allows for more efficient implementations of other operations, such as erasure (including a faster 'erase_if()'),
 * prepending elements, condition.
 * This efficiency comes at the price of CircularBuffer being larger than an efficiently-implemented std::vector by additional pointer-sized 
 * member, as well as slightly more costly traversal of the stored elements (e.g. "fat" iterators that are about the size of 3 pointers).
 * 
 * CircularBuffer efficiently support additional operations that std::vector does not, such as push_front()/pop_front(), and "shifting" of
 * logical elements' positions in the physical storage.
 * 
 * @tparam T         The type of the elements.
 * @tparam Allocator An allocator that is used to acquire/release memory and to construct/destroy the elements in that memory.
 *                   The type must meet the requirements of Allocator.
 *                   The program is ill-formed (since C++20) if Allocator::value_type is not the same as T.
 * 
 * @note
 * The requirements that are imposed on the elements depend on the actual operations performed on the container.
 * Generally, it is required that element type meets the requirements of Erasable, but many member functions impose stricter requirements.
 * This container (but not its members) can be instantiated with an incomplete element type if the allocator satisfies the allocator completeness requirements.
 * @note
 * CircularBuffer is an <a href="https://en.cppreference.com/w/cpp/named_req/AllocatorAwareContainer">AllocatorAwareContainer</a>.
 */
template <class T, class Allocator>
struct CircularBuffer : private detail::AllocatorBase<Allocator>
{
private:
	using alloc_traits = std::allocator_traits<Allocator>;
	using base_type = detail::AllocatorBase<Allocator>;
	using base_type::alloc;

public:
	using value_type             = T;
	using allocator_type         = Allocator;
	using size_type              = typename alloc_traits::size_type;
	using difference_type        = typename alloc_traits::difference_type;
	using reference              = value_type&;
	using const_reference        = const value_type&;
	using pointer                = typename alloc_traits::pointer;
	using const_pointer          = typename alloc_traits::const_pointer;
	using iterator               = CircularBufferIterator<T, Allocator>;
	using const_iterator         = ConstCircularBufferIterator<T, Allocator>;
	using reverse_iterator       = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;
	using shape_type             = BufferShape<size_type>;

	static_assert(std::is_same_v<T, typename std::allocator_traits<Allocator>::value_type>,
		"Allocator's value_type doesn't match the container value_type in CircularBuffer<T, Allocator>.");

private:
	using temporary_buffer_allocator_type = std::conditional_t<
		alloc_traits::is_always_equal::value&& std::is_empty_v<Allocator>,
		Allocator,
		detail::AllocatorRef<Allocator>>;
	using temporary_buffer_type = CircularBuffer<T, temporary_buffer_allocator_type>;
	struct TrivialRangeGuard {
		TrivialRangeGuard() = default;
		TrivialRangeGuard(const TrivialRangeGuard&) = delete;
		TrivialRangeGuard(TrivialRangeGuard&&) = default;
		TrivialRangeGuard& operator=(const TrivialRangeGuard&) = delete;
		TrivialRangeGuard& operator=(TrivialRangeGuard&&) = default;

		constexpr TrivialRangeGuard(Allocator* a, pointer start, pointer stop) :
			alloc(a),
			start(start),
			stop(stop)
		{

		}

		Allocator* alloc = nullptr;
		pointer start = nullptr;
		pointer stop = nullptr;
	};

	struct NonTrivialRangeGuard {
		NonTrivialRangeGuard() = default;
		NonTrivialRangeGuard(const NonTrivialRangeGuard&) = delete;
		constexpr NonTrivialRangeGuard(NonTrivialRangeGuard&& other) noexcept :
			alloc(std::exchange(other.alloc, nullptr)),
			start(other.start),
			stop(other.stop)
		{

		}

		constexpr NonTrivialRangeGuard(Allocator* a, pointer start, pointer stop) :
			alloc(a),
			start(start),
			stop(stop)
		{

		}

		constexpr NonTrivialRangeGuard& operator=(const NonTrivialRangeGuard&) = delete;
		constexpr NonTrivialRangeGuard& operator=(NonTrivialRangeGuard&& other) noexcept {
			NonTrivialRangeGuard tmp(std::move(*this));
			alloc = std::exchange(other.alloc, nullptr);
			start = other.start;
			stop = other.stop;
		}

		constexpr ~NonTrivialRangeGuard() {
			if (!alloc) {
				return;
			}
			for (pointer p = start; p < stop; ++p) {
				alloc_traits::destroy(*alloc, std::addressof(*p));
			}
		}

		Allocator* alloc = nullptr;
		pointer start = nullptr;
		pointer stop = nullptr;
	};

	template <class U>
	using RangeGuard = std::conditional_t<
		detail::is_trivially_allocator_destructible<Allocator, U>::value,
		TrivialRangeGuard,
		NonTrivialRangeGuard
	>;

	template <class Ptr>
	struct SplitRange {
		SplitRange() = delete;
		SplitRange(const SplitRange&) = default;
		SplitRange(SplitRange&&) = default;

		SplitRange& operator=(const SplitRange&) = default;
		SplitRange& operator=(SplitRange&&) = default;

		constexpr SplitRange(Ptr p, size_type sz) :
			begin_(p),
			size_(sz)
		{

		}

		constexpr Ptr begin() const {
			return begin_;
		}

		constexpr Ptr end() const {
			return begin_ + size();
		}

		constexpr std::reverse_iterator<Ptr> rbegin() const {
			return std::make_reverse_iterator(end());
		}

		constexpr std::reverse_iterator<Ptr> rend() const {
			return std::make_reverse_iterator(begin());
		}

		constexpr size_type size() const {
			return size_;
		}

		constexpr SplitRange<std::move_iterator<Ptr>> as_moving_range() const {
			return SplitRange<std::move_iterator<Ptr>>(std::make_move_iterator(begin_), size_);
		}


	private:
		Ptr begin_;
		size_type size_;
	};

	template <class Ptr>
	struct SplitRanges {
		using value_type = typename std::iterator_traits<Ptr>::value_type;

		constexpr SplitRanges() :
			ranges_{
				SplitRange<Ptr>{Ptr{}, 0u},
				SplitRange<Ptr>{Ptr{}, 0u}
		}
		{

		}

		explicit constexpr SplitRanges(SplitRange<Ptr> r) :
			ranges_{ r, SplitRange<Ptr>{Ptr{}, 0u} }
		{

		}

		constexpr SplitRanges(SplitRange<Ptr> r1, SplitRange<Ptr> r2) :
			ranges_{ r1, r2 }
		{

		}

		constexpr SplitRanges(std::pair<Ptr, Ptr> first_range, std::pair<Ptr, Ptr> second_range) :
			ranges_{
				SplitRange<Ptr>(first_range.first, first_range.second - first_range.first),
				SplitRange<Ptr>(second_range.first, first_range.second - first_range.first)
		}
		{

		}

		constexpr const SplitRange<Ptr>* begin() const {
			return &ranges_[0];
		}

		constexpr SplitRange<Ptr>* begin() {
			return &ranges_[0];
		}

		constexpr const SplitRange<Ptr>* end() const {
			return begin() + size();
		}

		constexpr SplitRange<Ptr>* end() {
			return begin() + size();
		}

		constexpr std::reverse_iterator<const SplitRange<Ptr>*> rbegin() const {
			return std::make_reverse_iterator(end());
		}

		constexpr std::reverse_iterator<SplitRange<Ptr>*> rbegin() {
			return std::make_reverse_iterator(end());
		}

		constexpr std::reverse_iterator<const SplitRange<Ptr>*> rend() const {
			return std::make_reverse_iterator(begin());
		}

		constexpr std::reverse_iterator<SplitRange<Ptr>*> rend() {
			return std::make_reverse_iterator(begin());
		}

		constexpr size_type size() const {
			TIM_CIRCULAR_BUFFER_ASSERT(ranges_[0].size() == 0 ? ranges_[1].size() == 0 : true);
			return size_type(ranges_[0].size() != 0) + size_type(ranges_[1].size() != 0);
		}

		constexpr size_type total() const {
			return ranges_[0].size() + ranges_[1].size();
		}

		constexpr SplitRanges first(size_type count) const {
			if (ranges_[0].size() >= count) {
				return SplitRanges(SplitRange<Ptr>(ranges_[0].begin(), count));
			}
			return SplitRanges(
				ranges_[0],
				SplitRange<Ptr>(ranges_[1].begin(), count - ranges_[0].size())
			);
		}

		constexpr SplitRanges last(size_type count) const {
			if (ranges_.size() == 1u) {
				return SplitRanges(SplitRange<Ptr>(ranges_[0].end() - count, count));
			}
			if (ranges_[1].size() >= count) {
				return SplitRanges(SplitRange<Ptr>(ranges_[1].end() - count, count));
			}
			return SplitRanges(
				SplitRange<Ptr>(ranges_[0].end() - (count - ranges_[1].size()), count - ranges_[1].size()),
				ranges_[1]
			);
		}

		constexpr const SplitRange<Ptr>& front() const {
			TIM_CIRCULAR_BUFFER_ASSERT(size() != 0);
			return ranges_[0];
		}

		constexpr SplitRange<Ptr>& front() {
			TIM_CIRCULAR_BUFFER_ASSERT(size() != 0);
			return ranges_[0];
		}

		constexpr const SplitRange<Ptr>& back() const {
			TIM_CIRCULAR_BUFFER_ASSERT(size() >= 1);
			return ranges_[size()-1];
		}

		constexpr SplitRange<Ptr>& back() {
			TIM_CIRCULAR_BUFFER_ASSERT(size() >= 1);
			return ranges_[size()-1];
		}

		constexpr const SplitRange<Ptr>& operator[](size_type idx) const
		{
			return ranges_[idx];
		}

		constexpr SplitRange<Ptr>& operator[](size_type idx)
		{
			return ranges_[idx];
		}

		[[nodiscard]]
		constexpr bool empty() const noexcept {
			return ranges_[0].size() == 0u;
		}

		constexpr SplitRanges<std::move_iterator<Ptr>> as_moving_ranges() const {
			return SplitRanges<std::move_iterator<Ptr>>(
				SplitRange<std::move_iterator<Ptr>>(
					std::make_move_iterator(ranges_[0].begin()),
					ranges_[0].size()
					),
				SplitRange<std::move_iterator<Ptr>>(
					std::make_move_iterator(ranges_[1].begin()),
					ranges_[1].size()
					)
				);
		}

	private:
		std::array<SplitRange<Ptr>, 2> ranges_;
	};

	struct CircularBufferDeleter {
		using pointer = typename CircularBuffer::pointer;

		constexpr CircularBufferDeleter(CircularBuffer* buff, size_type n) :
			buffer(buff),
			count(n)
		{

		}

		constexpr void operator()(pointer p) const {
			buffer->deallocate(p, count);
		}

	private:
		CircularBuffer* buffer;
		size_type count;
	};

	struct DummyGuard {
		bool active = true;
	};

	auto make_manual_append_guard() {
		return detail::make_manual_scope_guard([this, initial_size = this->size()]() {
			size_type sz = this->size();
			if (sz > initial_size) {
				this->pop_back_n(sz - initial_size);
			}
		});
	}

	auto make_manual_prepend_guard() {
		return detail::make_manual_scope_guard([this, initial_size = this->size()]() {
			size_type sz = this->size();
			if (sz > initial_size) {
				this->pop_front_n(sz - initial_size);
			}
		});
	}

	constexpr auto make_manual_append_guard_if_not_nothrow_move() {
		if constexpr (
			std::is_nothrow_move_constructible_v<T>
			&& std::is_nothrow_move_assignable_v<T>
			) {
			return DummyGuard{};
		}
		else {
			return make_manual_append_guard();
		}
	}

	constexpr auto make_manual_prepend_guard_if_not_nothrow_move() {
		if constexpr (
			std::is_nothrow_move_constructible_v<T>
			&& std::is_nothrow_move_assignable_v<T>
			) {
			return DummyGuard{};
		}
		else {
			return make_manual_prepend_guard();
		}
	}

	template <bool Cond, class Fn>
	static constexpr auto make_manual_scope_guard_if(Fn&& fn) {
		if constexpr (Cond) {
			return detail::make_manual_scope_guard(std::forward<Fn>(fn));
		}
		else {
			return DummyGuard{};
		}
	}

	using safe_pointer = std::unique_ptr<T, CircularBufferDeleter>;

	safe_pointer allocate_safe(size_type count) {
		return safe_pointer(this->allocate(count), CircularBufferDeleter(this, count));
	}

	constexpr pointer allocate(size_type count) {
		return alloc_traits::allocate(this->alloc(), count);
	}

	constexpr void deallocate(pointer p, size_type count) {
		return alloc_traits::deallocate(this->alloc(), p, count);
	}

	template <class ... Args>
	constexpr void construct(T* p, Args&& ... args) {
		alloc_traits::construct(this->alloc(), p, std::forward<Args>(args)...);
	}

	template <class Pointer, class ... Args, std::enable_if_t<std::is_same_v<pointer, Pointer> && !std::is_same_v<Pointer, T*>, bool> = false>
	constexpr void construct(Pointer p, Args&& ... args) {
		construct(std::addressof(*p), std::forward<Args>(args)...);
	}

	constexpr void destroy(T* p) {
		alloc_traits::destroy(this->alloc(), p);
	}

	template <class Pointer, class ... Args, std::enable_if_t<std::is_same_v<pointer, Pointer> && !std::is_same_v<Pointer, T*>, bool> = false>
	constexpr void destroy(Pointer p) {
		destroy(std::addressof(*p));
	}

	constexpr Allocator select_on_container_copy_construction() const {
		return alloc_traits::select_on_container_copy_construction(this->alloc());
	}

	static constexpr bool propagate_on_container_copy_assignment
		= alloc_traits::propagate_on_container_copy_assignment::value;

	static constexpr bool propagate_on_container_move_assignment
		= alloc_traits::propagate_on_container_move_assignment::value;

	static constexpr bool propagate_on_container_swap
		= alloc_traits::propagate_on_container_swap::value;

	static constexpr bool is_always_equal = alloc_traits::is_always_equal::value;


	constexpr void initialize_allocation(size_type count)
	{
		data_ = this->allocate(count);
		cap_ = count;
	}


	template <class Ranges>
	constexpr void initialize_split_buffer_fast(const Ranges& ranges)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size() == 0);
		const auto ranges_count = ranges.size();
		if (ranges_count == 0)
		{
			return;
		}
		TIM_CIRCULAR_BUFFER_ASSERT(ranges.total() <= capacity());
		auto dest_pos = data_ + start_;
		const auto wrap_pos = data_ + capacity();

		for (auto&& item : ranges[0])
		{
			TIM_CIRCULAR_BUFFER_ASSERT(dest_pos != wrap_pos);
			construct(dest_pos++, std::forward<decltype(item)>(item));
			++size_;
		}
		if (ranges_count == 1)
		{
			return;
		}
		TIM_CIRCULAR_BUFFER_ASSERT(dest_pos == wrap_pos);
		dest_pos = data_;
		const auto start_pos = data_ + start_;
		for (auto&& item : ranges[1])
		{
			TIM_CIRCULAR_BUFFER_ASSERT(dest_pos != start_pos);
			construct(dest_pos++, std::forward<decltype(item)>(item));
			++size_;
		}
	}

	template <class Ranges>
	constexpr void initialize_continuous_buffer_fast(const Ranges& ranges)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size() == 0);
		TIM_CIRCULAR_BUFFER_ASSERT(ranges.total() <= capacity());
		auto dest_pos = data_ + start_;
		const auto wrap_pos = data_ + capacity();

		for (const auto& range : ranges)
		{
			for (auto pos = range.begin(); pos != range.end(); ++pos)
			{
				TIM_CIRCULAR_BUFFER_ASSERT(dest_pos != wrap_pos);
				construct(dest_pos++, *pos);
				++size_;
			}
		}
	}

	template <class Int, std::enable_if_t<std::is_signed_v<Int>, bool> = true>
	static constexpr size_type size_from_iterator_difference(Int value)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(value >= 0);
		if constexpr(static_cast<std::make_unsigned_t<Int>>(std::numeric_limits<Int>::max()) > std::numeric_limits<size_type>::max())
		{
			TIM_CIRCULAR_BUFFER_ASSERT(static_cast<std::make_unsigned_t<Int>>(value) <= std::numeric_limits<size_type>::max());
		}
		return static_cast<size_type>(value);
	}

	template <class TagType>
	static constexpr CircularBuffer do_tagged_move_construction(TagType, CircularBuffer&& other, const Allocator& alloc)
	{
		const bool do_fast_move = is_always_equal || alloc == other.alloc();
		if (do_fast_move)
		{
			CircularBuffer buf(std::move(other));
			return buf;
		}
		else
		{
			size_type cap = 0;
			size_type start = 0;
			if constexpr(std::is_same_v<tags::PreserveBufferLayout, TagType>)
			{
				cap = other.capacity();
				start = other.begin_index();
			}
			else if constexpr(std::is_same_v<tags::PreserveBufferCapacity, TagType>)
			{
				cap = other.capacity();
			}
			else
			{
				cap = other.size();
			}
			CircularBuffer buf = from_shape(shape_type{ cap, start }, alloc);
			if(start == 0)
			{
				buf.initialize_continuous_buffer_fast(other.get_ranges().as_moving_ranges());
			}
			else
			{
				for(auto r: other.get_ranges().as_moving_ranges())
				{
					buf.append_fast(r.begin(), r.end());
				}
			}
			return buf;
		}
	}

	
	constexpr CircularBuffer(detail::ReserveTag, size_type cap, const Allocator& alloc):
		base_type(alloc)
	{
		reserve_fast(cap);
	}
	
	constexpr CircularBuffer(detail::ReserveTag, size_type cap)
	{
		reserve_fast(cap);
	}
	
	constexpr CircularBuffer(detail::ShapeTag, shape_type shape, const Allocator& al=Allocator()) noexcept(std::is_nothrow_copy_constructible_v<Allocator>):
		base_type(al)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(shape.start < shape.capacity);
		initialize_allocation(shape.capacity);
		start_ = shape.start;
	}
	
public:
	
	/// @internal 
	constexpr void _assert_invariants() const
	{
		assert(size() <= capacity());
		assert(begin_index() < capacity() || (capacity() == 0 && begin_index() == 0));
		assert(size() < max_size());
		if (!data_)
		{
			assert(capacity() == 0);
			assert(size() == 0);
		}
	}
	/// @endinternal 


	/** Destroys all elements in the container and releases all memory allocated. */
	constexpr ~CircularBuffer()
	{
		if(!data_)
		{
			TIM_CIRCULAR_BUFFER_ASSERT(cap_ == 0);
			TIM_CIRCULAR_BUFFER_ASSERT(size_ == 0);
			TIM_CIRCULAR_BUFFER_ASSERT(start_ == 0);
			return;
		}
		TIM_CIRCULAR_BUFFER_ASSERT(cap_ >= size_);
		TIM_CIRCULAR_BUFFER_ASSERT(start_ < cap_ || (cap_ == 0u && start_ == 0));
		auto ranges = get_ranges();
		for(auto range: ranges)
		{
			for(auto pos = range.begin(); pos != range.end(); ++pos)
			{
				destroy(std::addressof(*pos));
			}
		}
		deallocate(data_, cap_);
	}
	/** @brief Default constructor. Constructs an empty buffer with 0 capacity. */
	constexpr CircularBuffer() = default;
	
	/**
	 * @brief Constructs an empty container with the given allocator alloc.
	 * 
	 * @param al The allocator to construct the new buffer with.
	 */
	constexpr CircularBuffer(const Allocator& al) noexcept(std::is_nothrow_copy_constructible_v<Allocator>):
		base_type(al)
	{

	}

	/**
	 * @brief
	 * Constructs a copy of 'other' with the given allocator 'al' using the method specified by 'tag'.
	 * 
	 * If 'tag' has type 'tags::PreserveBufferLayout', then new buffer will have the same 'capacity()' and 'begin_index()'
	 * as the old buffer.
	 * If 'tag' has type 'tags::PreserveBufferCapacity', then new buffer will have the same 'capacity()' as the old buffer,
	 * with unspecified 'begin_index()'.
	 * If 'tag' has type 'tags::Optimized', then new buffer will reserve an implementation-defined amount of memory.  In practice,
	 * this is only enough memory to fit the elements in 'other'.
	 * 
	 * @param tag   Tag object controlling how the copy operation is to take place; one of tags::optimized, tags::preserve_layout, or tags::preserve_capacity.
	 * @param other The circular buffer whose contents are to be copied.
	 * @param al    The allocator to construct the new buffer with.
	 */
	template <
		class TagType,
		std::enable_if_t<
			detail::is_one_of_v<
				TagType,
				tags::PreserveBufferLayout,
				tags::PreserveBufferCapacity,
				tags::Optimized
			>,
			bool
		> = true
	>
	constexpr CircularBuffer(TagType tag, const CircularBuffer& other, const Allocator& al):
		CircularBuffer(al)
	{
		(void)tag;
		size_type new_cap = 0;
		size_type new_start = 0;
		if constexpr(std::is_same_v<TagType, tags::Optimized>)
		{
			new_cap = other.size();
			new_start = 0;
		}
		else if constexpr(std::is_same_v<TagType, tags::PreserveBufferCapacity>)
		{
			new_cap = other.capacity();
			new_start = 0;
		}
		else 
		{
			// tag == tags::preserve_layout
			new_cap = other.capacity();
			new_start = other.start_;
		}
		constexpr bool could_wrap = std::is_same_v<TagType, tags::PreserveBufferLayout>;
		initialize_allocation(new_cap);
		start_ = new_start;
		auto dest_pos = data_ + new_start;
		auto ranges = other.get_ranges();
		switch(ranges.size())
		{
		case 2:
			for(const auto& item: ranges.front())
			{
				construct(dest_pos++, item);
				++size_;
			}
			if constexpr(could_wrap)
			{
				// if other's data wraps around and we're preserving the layout, then we need to wrap our storage around as well for the second range.
				TIM_CIRCULAR_BUFFER_ASSERT(dest_pos == (data_ + capacity()));
				dest_pos = data_;
			}
			[[fallthrough]];
		case 1:
			for(const auto& item: ranges.back())
			{
				construct(dest_pos++, item);
				++size_;
			}
			break;
		default:
			break;
		}
	}

	/**
	 * @brief
	 * Constructs a copy of 'other' using the method specified by 'tag'.
	 * 
	 * If 'tag' has type 'tags::PreserveBufferLayout', then new buffer will have the same 'capacity()' and 'begin_index()'
	 * as the old buffer.
	 * If 'tag' has type 'tags::PreserveBufferCapacity', then new buffer will have the same 'capacity()' as the old buffer,
	 * with unspecified 'begin_index()'.
	 * If 'tag' has type 'tags::Optimized', then new buffer will reserve only enough memory to fit the elements in 'other'.
	 * 
	 * The allocator of the new buffer is determined as if by calling 'other.get_allocator().select_on_container_copy_construction()'.
	 * 
	 * @param tag   Tag object controlling how the copy operation is to take place; one of tags::optimized, tags::preserve_layout, or tags::preserve_capacity.
	 * @param other The circular buffer whose contents are to be copied.
	 */
	template <
		class TagType,
		std::enable_if_t<
			detail::is_one_of_v<
				TagType,
				tags::PreserveBufferLayout,
				tags::PreserveBufferCapacity,
				tags::Optimized
			>,
			bool
		> = true
	>
	constexpr CircularBuffer(TagType tag, const CircularBuffer& other):
		CircularBuffer(tag, other, other.select_on_container_copy_construction())
	{
		
	}

	/**
	 * @brief
	 * Constructs a copy of 'other' with the given allocator 'al' as if by calling 'CircularBuffer(tags::optimized, other, al)'.
	 * 
	 * @param tag   Tag object controlling how the copy operation is to take place; one of tags::optimized, tags::preserve_layout, or tags::preserve_capacity.
	 * @param other The circular buffer whose contents are to be copied.
	 */
	constexpr CircularBuffer(const CircularBuffer& other, const Allocator& al):
		CircularBuffer(tags::optimized, other, al)
	{
		
	}

	/**
	 * @brief
	 * Move constructor.
	 * 
	 * Constructs the container with the contents of other using move semantics.
	 * Allocator is obtained by move-construction from the allocator belonging to other.
	 * After the move, other is guaranteed to be empty().
	 * 
	 * @param other The circular buffer whose contents are to be moved.
	 */
	constexpr CircularBuffer(CircularBuffer&& other) noexcept:
		base_type(std::move(other.alloc())),
		data_(std::exchange(other.data_, nullptr)),
		cap_(std::exchange(other.cap_, 0)),
		size_(std::exchange(other.size_, 0)),
		start_(std::exchange(other.start_, 0))
	{

	}

	/**
	 * @brief
	 * Tagged move constructor.
	 * 
	 * Constructs the container with the contents of other using move semantics.
	 * Allocator is obtained by move-construction from the allocator belonging to other.
	 * After the move, other is guaranteed to be empty().
	 * Move-constructs a copy of 'other' using the method specified by 'tag'.
	 * If 'tag' has type 'tags::PreserveBufferLayout', then new buffer will have the same 'capacity()' and 'begin_index()'
	 * as the old buffer.
	 * If 'tag' has type 'tags::PreserveBufferCapacity', then new buffer will have the same 'capacity()' as the old buffer,
	 * with unspecified 'begin_index()'.
	 * If 'tag' has type 'tags::Optimized', then new buffer will reserve only enough memory to fit the elements in 'other'.
	 * 
	 * @param tag   Tag object controlling how the copy operation is to take place; one of tags::optimized, tags::preserve_layout, or tags::preserve_capacity.
	 * @param other The circular buffer whose contents are to be moved.
	 * 
	 * @note
	 * In practice, the 'tag' argument is ignored as the 'optimized' path is the same as the layout-preserving path and no memory is allocated.
	 * @note
	 * This overload exists primarily to support perfect-forwarding in conjunction with the non-moving version.
	 */

	template <
		class TagType,
		std::enable_if_t<
			detail::is_one_of_v<
				TagType,
				tags::PreserveBufferLayout,
				tags::PreserveBufferCapacity,
				tags::Optimized
			>,
			bool
		> = true
	>
	constexpr CircularBuffer(TagType, CircularBuffer&& other):
		CircularBuffer(std::move(other))
	{
		
	}

	/**
	 * @brief
	 * Tagged move constructor with custom allocator.
	 * 
	 * Constructs the container with the contents of other using move semantics.
	 * Allocator is obtained from the parameter 'al'.
	 * After the move, other is guaranteed to be empty().
	 * Move-constructs a copy of 'other' using the method specified by 'tag'.
	 * If the passed-in allocator is equal to the allocator used by 'other' (determined by calling operator==() or if
	 * std::allocator_traits<Allocator>::is_always_equal is true), then this overload proceeds similarly to 'CircularBuffer(TagType tag, CircularBuffer&& other)'.
	 * That is, no new memory is allocated and instead the new buffer takes ownership of the memory from 'other'.
	 * Otherwise, if the allocators are not equal, then 'al' is used to initialize the new buffer's allocator, and the elements from 'other'
	 * are moved into the new storage.
	 * If 'tag' has type 'tags::PreserveBufferLayout', then new buffer will have the same 'capacity()' and 'begin_index()'
	 * as the old buffer.
	 * If 'tag' has type 'tags::PreserveBufferCapacity', then new buffer will have the same 'capacity()' as the old buffer,
	 * with unspecified 'begin_index()'.
	 * If 'tag' has type 'tags::Optimized', then new buffer will reserve only enough memory to fit the elements in 'other'.
	 * 
	 * @param tag   Tag object controlling how the copy operation is to take place; one of tags::optimized, tags::preserve_layout, or tags::preserve_capacity.
	 * @param other The circular buffer whose contents are to be moved.
	 * @param al    The allocator to use for the new buffer.
	 */
	template <
		class TagType,
		std::enable_if_t<
			detail::is_one_of_v<
				TagType,
				tags::PreserveBufferLayout,
				tags::PreserveBufferCapacity,
				tags::Optimized
			>,
			bool
		> = true
	>
	constexpr CircularBuffer(TagType tag, CircularBuffer&& other, const Allocator& al):
		CircularBuffer(do_tagged_move_construction(tag, std::move(other), al))
	{
		
	}

	/**
	 * @brief
	 * Move constructor with custom allocator.
	 * 
	 * Constructs the container with the contents of other using move semantics.
	 * Allocator is obtained from the parameter 'al'.
	 * After the move, other is guaranteed to be empty().
	 * If the passed-in allocator is equal to the allocator used by 'other' (determined by calling operator==() or if
	 * std::allocator_traits<Allocator>::is_always_equal is true), then this overload proceeds similarly to 'CircularBuffer(CircularBuffer&& other)'.
	 * That is, no new memory is allocated and instead the new buffer takes ownership of the memory from 'other'.
	 * Otherwise, if the allocators are not equal, then 'al' is used to initialize the new buffer's allocator, and the elements from 'other'
	 * are moved into the new storage.
	 * 
	 * @param other The circular buffer whose contents are to be moved.
	 * @param al    The allocator to use for the new buffer.
	 */
	constexpr CircularBuffer(CircularBuffer&& other, const Allocator& alloc) noexcept:
		CircularBuffer(do_tagged_move_construction(tags::optimized, std::move(other), alloc))
	{

	}

	/**
	 * @brief
	 * Initializer list constructor.
	 * 
	 * Constructs the container with the contents of the initializer list 'init'.
	 * 
	 * @param init The initializer list whose contents are used to initialize the buffer.
	 * @param al   The allocator to use for the new buffer.
	 */
	constexpr CircularBuffer(std::initializer_list<T> init, const Allocator& alloc = Allocator()):
		CircularBuffer(init.begin(), init.end(), alloc)
	{

	}
	
	/**
	 * @brief
	 * Constructs the container with 'count' copies of elements with value 'value'.
	 * 
	 * @param count The number of elements to construct.
	 * @param value The object whose value is used to initialize the new elements.
	 * @param al    The allocator to use for the new buffer.
	 */
	explicit CircularBuffer(size_type count, const value_type& value, const Allocator& alloc = Allocator()):
		CircularBuffer(alloc)
	{
		initialize_allocation(count);
		auto stop = data_ + count;
		for(auto p = data_; p != stop; ++p)
		{
			this->construct(std::addressof(*p), value);
			++size_;
		}
	}

	/**
	 * @brief
	 * Constructs the container with 'count' default-consrtucted elements.
	 * 
	 * @param count The number of elements to construct.
	 * @param al    The allocator to use for the new buffer.
	 */
	explicit constexpr CircularBuffer(size_type count, const Allocator& alloc = Allocator()):
		CircularBuffer(alloc)
	{
		initialize_allocation(count);
		auto stop = data_ + count;
		for(auto p = data_; p != stop; ++p)
		{
			construct(std::addressof(*p));
			++size_;
		}
	}
	
	/**
	 * @brief
	 * Constructs the container with the contents of the range [first, last).
	 * 
	 * @param first Iterator to the first element in the range to construct the buffer from.
	 * @param last  Iterator to the past-the-end element in the range to construct the buffer from.
	 * @param al    The allocator to use for the new buffer.
	 */
	template <
		class It,
		std::enable_if_t<
			std::is_convertible_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::forward_iterator_tag
			>,
			bool
		> = true 
	>
	constexpr CircularBuffer(It first, It last, const Allocator& alloc = Allocator()):
		CircularBuffer(alloc)
	{
		auto count = std::distance(first, last);
		TIM_CIRCULAR_BUFFER_ASSERT(count >= 0);
		assign_empty(first, last, static_cast<size_type>(count));
	}

	/**
	 * @brief
	 * Constructs the container with the contents of the range [first, last).
	 * 
	 * @param first Iterator to the first element in the range to construct the buffer from.
	 * @param last  Iterator to the past-the-end element in the range to construct the buffer from.
	 * @param al    The allocator to use for the new buffer.
	 */
	template <
		class It,
		std::enable_if_t<
			std::is_same_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::input_iterator_tag
			>,
			bool
		> = true 
	>
	constexpr CircularBuffer(It first, It last, const Allocator& alloc = Allocator()):
		CircularBuffer(alloc)
	{
		assign(first, last);
	}

	/**
	 * @brief
	 * Copy constructor. Constructs the container with the copy of the contents of other.
	 * 
	 * @param other CircularBuffer whose contents are to be copied.
	 * 
	 * @note
	 * The copy constructor does not necessarily preserve the layout or capacity of the 'other'.
	 * If you require this behavior, consider using the tagged copy constructor.
	 */

	constexpr CircularBuffer(const CircularBuffer& other):
		CircularBuffer(other, other.select_on_container_copy_construction())
	{

	}
	
	/**
	 * @brief
	 * Shape "constructor".
	 * 
	 * Creates an empty buffer with a predefined 'capacity()' and 'begin_index()' 
	 * 
	 * @param shape The shape object definining the initial layout of the new buffer.
	 * @param al    The allocator to use for the new buffer.
	 */
	static constexpr CircularBuffer from_shape(shape_type shape, const Allocator& al = Allocator())
	{	
		return CircularBuffer(detail::shape_tag, shape, al);
	}


	/**
	 * @brief
	 * Copy assignment operator. Replaces the contents with a copy of the contents of other.
	 * 
	 * @param other CircularBuffer whose contents are to be copied.
	 * 
	 * @note
	 * If std::allocator_traits<allocator_type>::propagate_on_container_copy_assignment::value is true,
	 * the allocator of *this is replaced by a copy of that of other. If the allocator of *this after
	 * assignment would compare unequal to its old value, the old allocator is used to deallocate the memory,
	 * then the new allocator is used to allocate it before copying the elements.
	 * Otherwise, the memory owned by *this may be reused when possible.
	 * In any case, the elements originally belong to *this may be either destroyed or replaced by element-wise copy-assignment. 
	 */
	constexpr CircularBuffer& operator=(const CircularBuffer& other)
	{
		if constexpr(propagate_on_container_copy_assignment && !is_always_equal)
		{
			if (alloc() != other.alloc())
			{
				clear_fast();
				if(data_)
				{
					deallocate(data_, capacity());
				}
				cap_ = 0;
				CircularBuffer tmp(other, other.alloc());
				alloc() = tmp.alloc();
				data_ = std::exchange(tmp.data_, nullptr);
				cap_ = std::exchange(tmp.cap_, 0);
				size_ = std::exchange(tmp.size_, 0);
				start_ = std::exchange(tmp.start_, 0);
				return *this;
			}
		}
		clear_fast();
		if(capacity() < other.size())
		{
			if(data_) {
				deallocate(data_, capacity());
				data_ = nullptr;
				cap_ = 0;
				start_ = 0;
			}
			data_ = allocate(other.size());
			cap_ = other.size();
		}
		else
		{
			start_ = 0;
		}
		initialize_continuous_buffer_fast(other.get_ranges());
		return *this;
	}

	/**
	 * @brief
	 * 
	 * @param 
	 * 
	 */
	constexpr CircularBuffer& operator=(CircularBuffer&& other) noexcept(propagate_on_container_move_assignment || is_always_equal)
	{

		if(data_)
		{
			clear_fast();
			deallocate(data_, capacity());
			cap_ = 0;
		}
		constexpr auto do_fast_move_assignment = [](CircularBuffer& self, CircularBuffer&& other)
		{
			TIM_CIRCULAR_BUFFER_ASSERT(propagate_on_container_move_assignment || is_always_equal || self.alloc() == other.alloc());

			if constexpr (propagate_on_container_move_assignment)
			{
				self.alloc() = other.alloc();
			}
			self.data_  = std::exchange(other.data_, nullptr);
			self.cap_   = std::exchange(other.cap_, 0);
			self.size_  = std::exchange(other.size_, 0);
			self.start_ = std::exchange(other.start_, 0);
		};
		if constexpr (propagate_on_container_move_assignment || is_always_equal)
		{
			do_fast_move_assignment(*this, std::move(other));
		}
		else if (alloc() == other.alloc())
		{
			do_fast_move_assignment(*this, std::move(other));
		}
		else
		{
			auto new_cap = other.size();
			data_ = allocate(new_cap);
			cap_ = new_cap;
			start_ = 0;
			initialize_continuous_buffer_fast(other.get_ranges().as_moving_ranges());
		}
		return *this;
	}

	/**
	 * @brief
	 * Replaces the contents with those identified by initializer list 'init'.
	 * 
	 * @param init The initializer list whose contents are used to initialize the buffer.
	 * 
	 */
	constexpr CircularBuffer& operator=(std::initializer_list<T> init)
	{
		assign(init);
		return *this;
	}
	/**
	 * @brief
	 * Returns a copy of *this with the layout preserved.  Shorthand for 'CircularBuffer(tags::preserve_layout, *this)'.
	 */
	constexpr CircularBuffer exact_copy() const
	{
		return CircularBuffer(tags::preserve_layout, *this);
	}


	/* Assignment */

	/**
	 * @brief
	 * Replaces the contents with count copies of value value
	 * 
	 * @param count The number of elements to assign.
	 * @param value The object to copy-initialize the new elements from.
	 */
	constexpr void assign(size_type count, const T& value)
	{
		clear_fast();
		assign_empty(count, value);
	}

	/**
	 * @brief
	 * Replaces the contents with copies of those in the range [first, last).
	 * 
	 * @param first Iterator to the first value in the range of elements to be copied.
	 * @param last  Iterator to one past the last value in the range of elements to be copied.
	 * 
	 * @note
	 * The behavior is undefined if either argument is an iterator into *this. 
	 */
	template <
		class It,
		std::enable_if_t<
			std::is_convertible_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::input_iterator_tag
			>,
			bool
		> = false
	>
	constexpr void assign(It first, It last) {
		clear_fast();
		if constexpr (std::is_convertible_v<typename std::iterator_traits<It>::iterator_category, std::forward_iterator_tag>)
		{
			assign_empty(first, last, size_from_iterator_difference(std::distance(first, last)));
		}
		else
		{
			append(first, last);
		}
	}

	/**
	 * @brief Replaces the contents with the elements from the initializer list 'init'.
	 * 
	 * @param init The initializer list whose contents are used to assign the buffer.
	 */
	constexpr void assign(std::initializer_list<T> ilist) {
		assign(ilist.begin(), ilist.end());
	}
	
	/**
	 * @brief Returns the allocator associated with the container. 
	 */
	constexpr allocator_type get_allocator() const noexcept { return alloc(); }

	/* Iterators */

	/**
	 * @brief Returns a mutable iterator to the first element of the buffer.  
	 */
	constexpr iterator begin() noexcept {
		return iterator(data_, cap_, start_, false);
	}

	/**
	 * @brief Returns a const iterator to the first element of the buffer.  
	 */
	constexpr const_iterator begin() const noexcept {
		return const_iterator(data_, cap_, start_, false);
	}

	/**
	 * @brief Returns a const iterator to the first element of the buffer.  
	 */
	constexpr const_iterator cbegin() const noexcept {
		return const_iterator(data_, cap_, start_, false);
	}


	/**
	 * @brief
	 * Returns a mutable iterator to the element following the last element of the buffer.
	 * 
	 * This element acts as a placeholder; attempting to access it results in undefined behavior. 
	 */
	constexpr iterator end() noexcept {
		if(cap_ - start_ > size_) {
			return iterator(data_, cap_, start_ + size_, false);
		} else {
			return iterator(data_, cap_, (size_ - (cap_ - start_)), size_ != 0);
		}
	}

	/**
	 * @brief
	 * Returns a const iterator to the element following the last element of the buffer.
	 * 
	 * This element acts as a placeholder; attempting to access it results in undefined behavior. 
	 */
	constexpr const_iterator end() const noexcept {
		return cend();
	}

	/**
	 * @brief
	 * Returns a const iterator to the element following the last element of the buffer.
	 * 
	 * This element acts as a placeholder; attempting to access it results in undefined behavior. 
	 */
	constexpr const_iterator cend() const noexcept {
		return const_cast<CircularBuffer*>(this)->end();
	}


	/**
	 * @brief
	 * Returns a mutable reverse iterator to the first element of the reversed buffer.
	 * 
	 * It corresponds to the last element of the non-reversed buffer.
	 * If the buffer is empty, the returned iterator is equal to rend(). 
	 */
	constexpr reverse_iterator rbegin() noexcept {
		return std::make_reverse_iterator(end());
	}

	/**
	 * @brief
	 * Returns a const reverse iterator to the first element of the reversed buffer.
	 * 
	 * It corresponds to the last element of the non-reversed buffer.
	 * If the buffer is empty, the returned iterator is equal to rend(). 
	 */
	constexpr const_reverse_iterator rbegin() const noexcept {
		return std::make_reverse_iterator(end());
	}

	/**
	 * @brief
	 * Returns a const reverse iterator to the first element of the reversed buffer.
	 * 
	 * It corresponds to the last element of the non-reversed buffer.
	 * If the buffer is empty, the returned iterator is equal to rend(). 
	 */
	constexpr const_reverse_iterator crbegin() const noexcept {
		return std::make_reverse_iterator(end());
	}


	/**
	 * @brief
	 * Returns a mutable reverse iterator to the element following the last element of the reversed buffer.
	 * 
	 * It corresponds to the element preceding the first element of the non-reversed buffer.
	 * This element acts as a placeholder, attempting to access it results in undefined behavior. 
	 */
	constexpr reverse_iterator rend() noexcept {
		return std::make_reverse_iterator(begin());
	}

	/**
	 * @brief
	 * Returns a const reverse iterator to the element following the last element of the reversed buffer.
	 * 
	 * It corresponds to the element preceding the first element of the non-reversed buffer.
	 * This element acts as a placeholder, attempting to access it results in undefined behavior. 
	 */
	constexpr const_reverse_iterator rend() const noexcept {
		return std::make_reverse_iterator(begin());
	}

	/**
	 * @brief
	 * Returns a const reverse iterator to the element following the last element of the reversed buffer.
	 * 
	 * It corresponds to the element preceding the first element of the non-reversed buffer.
	 * This element acts as a placeholder, attempting to access it results in undefined behavior. 
	 */
	constexpr const_reverse_iterator crend() const noexcept {
		return std::make_reverse_iterator(begin());
	}


	/**
	 * @brief
	 * Returns a mutable reference to the element at specified location 'index'. No bounds checking is performed. 
	 * 
	 * @param index The index into the logical buffer of the element to access.
	 */
	constexpr reference operator[](size_type index) {
		return begin()[index];
	}

	/**
	 * @brief
	 * Returns a const reference to the element at specified location 'index'. No bounds checking is performed. 
	 * 
	 * @param index The index into the logical buffer of the element to access.
	 */
	constexpr const_reference operator[](size_type index) const {
		return cbegin()[index];
	}


	/**
	 * @brief
	 * Returns a mutable reference to the element at specified location 'index', with bounds checking.
	 * 
	 * If 'index' is not within the range of the container, an exception of type std::out_of_range is thrown. 
	 * 
	 * @param index The index into the logical buffer of the element to access.
	 */
	constexpr reference at(size_type index) {
		if(index > size()) {
			throw std::out_of_range("tim::CircularBuffer::at()");
		}
		return begin()[index];
	}

	/**
	 * @brief
	 * Returns a const reference to the element at specified location 'index', with bounds checking.
	 * 
	 * If 'index' is not within the range of the container, an exception of type std::out_of_range is thrown. 
	 * 
	 * @param index The index into the logical buffer of the element to access.
	 */
	constexpr const_reference at(size_type index) const {
		if(index > size()) {
			throw std::out_of_range("tim::CircularBuffer::at()");
		}
		return begin()[index];
	}

	/**
	 * @brief
	 * Returns a const reference to the first element in the container.
	 * 
	 * Calling front on an empty container is undefined. 
	 */
	constexpr const_reference front() const { return *begin(); }

	/**
	 * @brief
	 * Returns a mutable reference to the first element in the container.
	 * 
	 * Calling front on an empty container is undefined. 
	 */
	constexpr reference front() { return *begin(); }

	/**
	 * @brief
	 * Returns a const reference to the last element in the container.
	 * 
	 * Calling back on an empty container is undefined. 
	 */
	constexpr const_reference back() const noexcept { return *rbegin(); }

	/**
	 * @brief
	 * Returns a mutable reference to the last element in the container.
	 * 
	 * Calling back on an empty container is undefined. 
	 */
	constexpr reference back() { return *rbegin(); }

	/**
	 * @brief
	 * Returns the number of elements that the container has currently allocated space for. 
	 */
	constexpr size_type capacity() const noexcept { return cap_; }

	/**
	 * @brief
	 * Returns the number of elements in the container, i.e. std::distance(begin(), end()). 
	 */
	constexpr size_type size() const noexcept{ return size_; }

	/**
	 * @brief
	 * Checks if the container has no elements, i.e. whether begin() == end().
	 * 
	 * @return true if the container is empty, false otherwise
	 */
	[[nodiscard("empty() returns whether the buffer is empty and has no side effects.")]]
	constexpr bool empty() const noexcept { return size() == 0u; }

	/**
	 * @brief
	 * Returns the maximum number of elements the container is able to hold due to system or library implementation limitations,
	 * i.e. std::distance(begin(), end()) for the largest possible container. 
	 * 
	 * @note
	 * This value typically reflects the theoretical limit on the size of the container, at most std::numeric_limits<difference_type>::max().
	 * @note
	 * At runtime, the size of the container may be limited to a value smaller than max_size() by the amount of RAM available. 
	 */
	constexpr size_type max_size() const noexcept { return std::min((~size_type(0)) >> 1u, alloc_traits::max_size(alloc())); }

	/**
	 * @brief
	 * Returns the index into the raw (physical) memory buffer of the first element (the physical index of logical index 0).
	 */
	constexpr size_type begin_index() const
	{
		return start_;
	}

	/**
	 * @brief
	 * Returns the index into the raw memory buffer of the element following the last element in the buffer. 
	 * 
	 * If the buffer is either empty or at capacity, this is equal to begin_index().
	 */
	constexpr size_type end_index() const
	{
		return end().get_index();
	}

	/**
	 * @brief
	 * Erases all elements from the container.
	 * 
	 * After this call, size() returns zero.
	 * Invalidates any references, pointers, or iterators referring to contained elements.
	 * Any past-the-end iterators are also invalidated.  
	 * Leaves the capacity() of the buffer unchanged. 
	 */
	constexpr void clear() noexcept
	{
		clear_fast();
	}

	/**
	 * @brief
	 * Increase the capacity of the buffer to a value that's greater or equal to new_cap.
	 * 
	 * If new_cap is greater than the current capacity(), new storage is allocated, otherwise the method does nothing.
	 * reserve() does not change the size of the buffer.
	 * If new_cap is greater than capacity(), all iterators, including the past-the-end iterator, and all references to the elements are invalidated.
	 * Otherwise, no iterators or references are invalidated.
	 *
	 * @param new_cap New capacity of the buffer.
	 * 
	 * @note
	 * After calls to this function, the values of begin_index() and end_index() are unspecified.
	 * @note
	 * See reserve_front(), reserve_back() and for alternatives.
	 */
	constexpr void reserve(size_type new_cap) {
		if(new_cap > max_size()) {
			throw std::length_error("CircularBuffer::reserve() new capacity is greater than the buffer's max_size()");
		}
		if(this->capacity() >= new_cap) {
			return;
		}
		this->reserve_slow(new_cap, 0);
	}

	/**
	 * @brief
	 * Increase the capacity of the buffer to a value that's greater or equal to new_cap.
	 * 
	 * If new_cap is greater than the current capacity(), new storage is allocated, otherwise the method does nothing.
	 * reserve() does not change the size of the buffer.
	 * If new_cap is greater than capacity(), all iterators, including the past-the-end iterator, and all references to the elements are invalidated.
	 * Otherwise, no iterators or references are invalidated.
	 *
	 * @param new_cap New capacity of the buffer.
	 * 
	 * @note
	 * Calls to this overload always preserve the value of end_index(); that is
	 * the extra capacity is always added on the beginning of the buffer.
	 * @note
	 * See reserve_back() and reserve() for alternatives.
	 */
	constexpr void reserve_front(size_type new_cap) {
		if (new_cap <= capacity())
		{
			return;
		}
		reserve_slow(new_cap, start_ + (new_cap - capacity()));
	}

	/**
	 * @brief
	 * Increase the capacity of the buffer to a value that's greater or equal to new_cap.
	 * 
	 * If new_cap is greater than the current capacity(), new storage is allocated, otherwise the method does nothing.
	 * reserve() does not change the size of the buffer.
	 * If new_cap is greater than capacity(), all iterators, including the past-the-end iterator, and all references to the elements are invalidated.
	 * Otherwise, no iterators or references are invalidated.
	 *
	 * @param new_cap New capacity of the buffer.
	 * 
	 * @note
	 * Calls to this overload always preserve the value of begin_index(); that is
	 * the extra capacity is always added on the end of the buffer.
	 * @note
	 * See reserve_back() and reserve() for alternatives.
	 */
	constexpr void reserve_back(size_type new_cap) {
		if (new_cap <= capacity())
		{
			return;
		}
		reserve_slow(new_cap, start_);
	}

	/**
	 * @brief
	 * Requests the removal of unused capacity.
	 * 
	 * It is a non-binding request to reduce capacity() to size().
	 * It depends on the implementation whether the request is fulfilled.
	 * If reallocation occurs, all iterators, including the past the end iterator, and all references to the elements are invalidated.
	 * If no reallocation takes place, no iterators or references are invalidated. 
	 * 
	 * @note
	 * If an exception is thrown other than by T's move constructor, there are no effects. 
	 */
	constexpr void shrink_to_fit()
	{
		if (size() == capacity())
		{
			return;
		}
		auto tmp = make_temporary_buffer(size());
		auto pos = tmp.data_;
		for (auto r : get_ranges())
		{
			for (T& value : r)
			{
				tmp.construct(pos++, std::move(value));
				++tmp.size_;
			}
		}
		commit(tmp);
	}
	
	/**
	 * @brief
	 * Inserts elements from range [first, last) before pos.
	 * Elements in the range [pos, end()) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of end_index() will change accordingly (but begin_index() will be unchanged).
	 * 
	 * This overload only participates in overload resolution if InputIt qualifies as LegacyInputIterator,
	 * but not LegacyForwardIterator.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param first Iterator to the first value in the range of elements to be inserted.
	 * @param last  Iterator to one past the last value in the range of elements to be inserted.
	 * 
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	template <
		class It,
		std::enable_if_t<
			detail::is_one_of_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::input_iterator_tag
			>,
			bool
		> = false
	>
	constexpr iterator insert_move_back(const_iterator pos, It first, It last) {
		if(first == last) {
			return as_non_const_iterator(pos);
		}
		const size_type index = pos - cbegin();
		auto initial_size = size();
		while(size() < capacity() && first != last) {
			emplace_back_fast(*first++);
		}
		size_type count_inserted = size() - initial_size;
		if(first == last) {
			std::rotate(as_non_const_iterator(pos), end() - count_inserted, end());
			return begin() + index;
		}
		auto tmp = make_temporary_buffer(grow_size(this->capacity()));
		// Write directly to the final destination position.
		tmp.start_ = count_inserted + index;
		for(;;) {
			do {
				tmp.emplace_back_fast(*first++);
			} while(first != last && tmp.capacity() - tmp.size() > this->capacity());
			if(first == last) {
				break;
			}
			tmp.reserve_back(grow_size(tmp.capacity()));
		}
		// Append the elements we initially appended onto *this.
		for(auto range: detail::reversed(as_split_ranges(end() - count_inserted, end()).as_moving_ranges()))
		{
			tmp.prepend_fast(range.begin(), range.end());
		}
		// Then append the elements from the range that were initially at or after 'pos' from *this.
		// (up to, but not including the elements we appended from [first, last)).
		for(auto range: as_split_ranges(as_non_const_iterator(pos), this->end() - count_inserted).as_moving_ranges())
		{
			tmp.append_fast(range.begin(), range.end());
		}
		// Then prepend the elements preceding 'pos' from *this.
		for(auto range: detail::reversed(as_split_ranges(begin(), as_non_const_iterator(pos)).as_moving_ranges()))
		{
			tmp.prepend_fast(range.begin(), range.end());
		}
		assert(!tmp.storage_is_split());
		commit(tmp);
		return begin() + index;
	}

	/**
	 * @brief
	 * Inserts elements from range [first, last) before pos.
	 * Elements in the range [pos, end()) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of end_index() will change accordingly (but begin_index() will be unchanged).
	 * 
	 * This overload only participates in overload resolution if InputIt qualifies as LegacyForwardIterator.
	 * The behavior is undefined if first and last are iterators into *this.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param first Iterator to the first value in the range of elements to be inserted.
	 * @param last  Iterator to one past the last value in the range of elements to be inserted.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	template <
		class It,
		std::enable_if_t<
			std::is_convertible_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::forward_iterator_tag
			>,
			bool
		> = false
	>
	constexpr iterator insert_move_back(const_iterator pos, It first, It last) {
		size_type count = size_from_iterator_difference(std::distance(first, last));
		if(count <= this->capacity() - this->size()) {
			return this->insert_move_back_fast(pos, first, last, count);
		} else {
			return this->insert_move_back_slow(pos, first, last, count);
		}
	}

	/**
	 * @brief
	 * Inserts the given element before pos.
	 * Elements in the range [pos, end()) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of end_index() will change accordingly (but begin_index() will be unchanged).
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param value The value to insert.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	template <class U, std::enable_if_t<std::is_same_v<T, std::remove_cvref_t<U>>, bool> = true>
	constexpr iterator insert_move_back(const_iterator pos, U&& value) {
		auto idx = pos - begin();
		emplace_move_back(pos, std::forward<U>(value));
		return begin() + idx;
	}

	/**
	 * @brief
	 * Inserts 'count' copies of the given element before pos.
	 * Elements in the range [pos, end()) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of end_index() will change accordingly (but begin_index() will be unchanged).
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param count The number of elements to insert.
	 * @param value The value of the inserted elements.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	constexpr iterator insert_move_back(const_iterator pos, size_type count, const T& value) {
		auto range = detail::make_single_value_range(value, count);
		return insert_move_back(pos, range.begin(), range.end());
	}

	/**
	 * @brief
	 * Inserts the elements from the initialize list before pos.
	 * Elements in the range [pos, end()) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of end_index() will change accordingly (but begin_index() will be unchanged).
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param ilist Initializer list containing the elements to be inserted.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	constexpr iterator insert_move_back(const_iterator pos, std::initializer_list<T> ilist) {
		return insert_move_back(pos, ilist.begin(), ilist.end());
	}



	/**
	 * @brief
	 * Inserts elements from range [first, last) before pos.
	 * Elements in the range [begin(), pos) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of begin_index() will change accordingly (but end_index() will be unchanged).
	 * 
	 * This overload only participates in overload resolution if InputIt qualifies as LegacyInputIterator,
	 * but not LegacyForwardIterator.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param first Iterator to the first value in the range of elements to be inserted.
	 * @param last  Iterator to one past the last value in the range of elements to be inserted.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	template <
		class It,
		std::enable_if_t<
			detail::is_one_of_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::input_iterator_tag
			>,
			bool
		> = false
	>
	constexpr iterator insert_move_front(const_iterator pos, It first, It last) {
		if(first == last) {
			return as_non_const_iterator(pos);
		}
		const size_type index = pos - cbegin();
		auto initial_size = size();
		bool split_before = storage_is_split();
		while(size() < capacity() && first != last) {
			emplace_front_fast(*first++);
		}
		bool split_after = storage_is_split();
		size_type count_inserted = size() - initial_size;
		if(!split_before && split_after)
		{
			pos = begin() + (index + count_inserted);
		}
		if(first == last) {
			std::reverse(begin(), begin() + count_inserted);
			std::rotate(begin(), begin() + count_inserted, as_non_const_iterator(pos));
			return begin() + index;
		}
		// need to reallocate to fit everything.  Put the remaning elements into a temporary buffer and then
		// copy everything over once we know how much space we need.
		auto tmp = make_temporary_buffer(grow_size(this->capacity()));
		tmp.start_ = index + count_inserted;
		// Write directly to the final destination position.
		for(;;) {
			// emplace into the buffer while there's still stuff to add, while also ensuring that there's enough space for
			// the elements already contained in '*this'
			do {
				tmp.emplace_back_fast(*first++);
			} while(first != last && (tmp.capacity() - tmp.size()) > this->capacity());
			if(first == last) {
				break;
			}
			tmp.reserve_back(grow_size(tmp.capacity()));
		}
		for(auto range: as_split_ranges(begin(), begin() + count_inserted).as_moving_ranges())
		{
			tmp.prepend_fast(range.rbegin(), range.rend());
		}
		assert((begin() + count_inserted) <=  as_non_const_iterator(pos));
		for(auto range: detail::reversed(as_split_ranges(begin() + count_inserted, as_non_const_iterator(pos)).as_moving_ranges()))
		{
			tmp.prepend_fast(range.begin(), range.end());
		}
		for(auto range: as_split_ranges(as_non_const_iterator(pos), end()).as_moving_ranges())
		{
			tmp.append_fast(range.begin(), range.end());
		}
		assert(!tmp.storage_is_split());
		commit(tmp);
		return begin() + index;
	}




	/**
	 * @brief
	 * Inserts elements from range [first, last) before pos.
	 * Elements in the range [begin(), pos) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of begin_index() will change accordingly.
	 *
	 * This overload only participates in overload resolution if InputIt qualifies as LegacyForwardIterator.
	 * The behavior is undefined if first and last are iterators into *this.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param first Iterator to the first value in the range of elements to be inserted.
	 * @param last  Iterator to one past the last value in the range of elements to be inserted.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	template <
		class It,
		std::enable_if_t<
			std::is_convertible_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::forward_iterator_tag
			>,
			bool
		> = false
	>
	constexpr iterator insert_move_front(const_iterator pos, It first, It last) {
		size_type count = size_from_iterator_difference(std::distance(first, last));
		if(count <= capacity() - size()) {
			return insert_move_front_fast(pos, first, last, count);
		} else {
			return insert_move_front_slow(pos, first, last, count);
		}
	}

	/**
	 * @brief
	 * Inserts the given element before pos.
	 * Elements in the range [begin(), pos) are moved to make room for the to-be-inserted ivalue.
	 * 
	 * Note that this means the value of begin_index() will change accordingly.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param value The value to insert.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	template <class U, std::enable_if_t<std::is_same_v<T, std::remove_cvref_t<U>>, bool> = true>
	constexpr iterator insert_move_front(const_iterator pos, U&& value) {
		auto idx = pos - begin();
		emplace_move_front(pos, std::forward<U>(value));
		return begin() + idx;
	}

	/**
	 * @brief
	 * Inserts 'count' copies of the given element before pos.
	 * Elements in the range [begin(), pos) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of begin_index() will change accordingly.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param count The number of elements to insert.
	 * @param value The value of the inserted elements.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	constexpr iterator insert_move_front(const_iterator pos, size_type count, const T& value) {
		auto range = detail::make_single_value_range(value, count);
		return insert_move_front(pos, range.begin(), range.end());
	}

	/**
	 * @brief
	 * Inserts the elements from the initialize list before pos.
	 * Elements in the range [begin(), pos) are moved to make room for the to-be-inserted range.
	 * 
	 * Note that this means the value of begin_index() will change accordingly.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param ilist Initializer list containing the elements to be inserted.
	 *
	 * @note
	 * If reallocation occurs, the values of begin_index() and end_index() are unspecified.
	 */
	constexpr iterator insert_move_front(const_iterator pos, std::initializer_list<T> ilist) {
		return insert_move_front(pos, ilist.begin(), ilist.end());
	}

	/**
	 * @brief
	 * Inserts elements from range [first, last) before pos.
	 * 
	 * If the number of elements before pos is less than the number of elements after pos, then 
	 * the new values are inserted as if by calling insert_move_front(), otherwise they are inserted
	 * as if by insert_move_back().
	 *
	 * This overload only participates in overload resolution if InputIt qualifies as LegacyInputIterator.
	 * The behavior is undefined if first and last are iterators into *this.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param first Iterator to the first value in the range of elements to be inserted.
	 * @param last  Iterator to one past the last value in the range of elements to be inserted.
	 */
	template <class It>
	constexpr iterator insert(const_iterator pos, It first, It last) {
		// Insert in which ever way involves the least data movement.
		size_type count_before = pos - begin();
		size_type count_after = size() - count_before;
		if(count_before < count_after) {
			return insert_move_front(pos, first, last);
		} else {
			return insert_move_back(pos, first, last);
		}
	}

	/**
	 * @brief
	 * Inserts the elements from the initialize list before pos.
	 * 
	 * If the number of elements before pos is less than the number of elements after pos, then 
	 * the new values are inserted as if by calling insert_move_front(), otherwise they are inserted
	 * as if by insert_move_back().
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param ilist initializer list containing the elements to be inserted.
	 */
	constexpr iterator insert(const_iterator pos, std::initializer_list<T> ilist) {
		return insert(pos, ilist.begin(), ilist.end());
	}


	/**
	 * @brief
	 * Inserts the given element before pos.
	 * 
	 * If the number of elements before pos is less than the number of elements after pos, then 
	 * the new value is inserted as if by calling insert_move_front(), otherwise they are inserted
	 * as if by insert_move_back().
	 *
	 * This overload only participates in overload resolution if InputIt qualifies as LegacyInputIterator.
	 * The behavior is undefined if first and last are iterators into *this.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param value The value to insert.
	 */
	template <class U, std::enable_if_t<std::is_same_v<T, std::remove_cvref_t<U>>, bool> = true>
	constexpr iterator insert(const_iterator pos, U&& value) {
		return emplace(pos, std::forward<U>(value));
	}

	/**
	 * @brief
	 * Inserts 'count' copies of the given element before pos.
	 * 
	 * If the number of elements before pos is less than the number of elements after pos, then 
	 * the new values are inserted as if by calling insert_move_front(), otherwise they are inserted
	 * as if by insert_move_back().
	 *
	 * This overload only participates in overload resolution if InputIt qualifies as LegacyInputIterator.
	 * The behavior is undefined if first and last are iterators into *this.
	 *
	 * @param pos   Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param count The number of elements to insert.
	 * @param value The value of the inserted elements.
	 */
	constexpr iterator insert(const_iterator pos, size_type count, const T& value) {
		auto range = detail::make_single_value_range(value, count);
		return insert(pos, range.begin(), range.end());
	}

	/**
	 * @brief
	 * Emplaces the given element before pos.
	 * 
	 * If the number of elements before pos is less than the number of elements after pos, then 
	 * the new value is emplaced as if by calling emplace_move_front(), otherwise they are inserted
	 * as if by emplace_move_back().
	 *
	 * @param pos  Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param args Arguments with which the new element is to be constructed.
	 * 
	 * @note
	 * The element is constructed through std::allocator_traits::construct, which typically uses placement-new
	 * to construct the element in-place at a location provided by the container.
	 * However, if the required location has been occupied by an existing element, the inserted element is
	 * constructed at another location at first, and then move assigned into the required location.
	 * @note
	 * The arguments args... are forwarded to the constructor as std::forward<Args>(args)....
	 * args... may directly or indirectly refer to a value in the container.
	 * @note
	 * If the new size() is greater than capacity(), or if begin_index() after calling this function is greater
	 * than before the call, all iterators and references are invalidated.
	 * Otherwise, only the iterators and references before the insertion point remain valid.
	 * The past-the-end iterator is also invalidated. 
	 */
	template <class ... Args>
	constexpr iterator emplace(const_iterator pos, Args&& ... args) {
		size_type count_before = pos - begin();
		size_type count_after = size() - count_before;
		if(count_before < count_after)
		{
			return emplace_move_front(pos, std::forward<Args>(args)...);
		}
		else
		{
			return emplace_move_back(pos, std::forward<Args>(args)...);
		}
	}


	/**
	 * @brief
	 * Emplaces the given element before pos.
	 * Elements in the range [pos, end()) are moved to make room for the to-be-inserted range.
	 *
	 * @param pos  Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param args Arguments with which the new element is to be constructed.
	 * 
	 * @note
	 * The element is constructed through std::allocator_traits::construct, which typically uses placement-new
	 * to construct the element in-place at a location provided by the container.
	 * However, if the required location has been occupied by an existing element, the inserted element is
	 * constructed at another location at first, and then move assigned into the required location.
	 * @note
	 * The arguments args... are forwarded to the constructor as std::forward<Args>(args)....
	 * args... may directly or indirectly refer to a value in the container.
	 * @note
	 * If the new size() is greater than capacity() all iterators and references are invalidated.
	 * Otherwise, only the iterators and references before the insertion point remain valid.
	 * The past-the-end iterator is also invalidated. 
	 */
	template <class ... Args>
	constexpr iterator emplace_move_back(const_iterator pos, Args&& ... args) {
		if (pos == end())
		{
			emplace_back(std::forward<Args>(args)...);
			return std::prev(end());
		}
		if(capacity() == size())
		{
			return emplace_reallocate(pos, std::forward<Args>(args)...);
		}
		emplace_back(std::move(back()));
		auto p = as_non_const_iterator(pos);
		copy_ranges_backward(
			as_moving_split_ranges(p, std::prev(end(), 2)),
			as_split_ranges(std::next(p), std::prev(end())));
		*p = value_type(std::forward<Args>(args) ...);
		return p;
	}

	/**
	 * @brief
	 * Emplaces the given element before pos.
	 * Elements in the range [begin(), pos) are moved to make room for the to-be-inserted range.
	 *
	 * @param pos  Iterator before which the content will be inserted. pos may be the end() iterator.
	 * @param args Arguments with which the new element is to be constructed.
	 * 
	 * @note
	 * The element is constructed through std::allocator_traits::construct, which typically uses placement-new
	 * to construct the element in-place at a location provided by the container.
	 * However, if the required location has been occupied by an existing element, the inserted element is
	 * constructed at another location at first, and then move assigned into the required location.
	 * @note
	 * The arguments args... are forwarded to the constructor as std::forward<Args>(args)....
	 * args... may directly or indirectly refer to a value in the container.
	 * @note
	 * If the new size() is greater than capacity(), or if begin_index() after calling this function is greater
	 * than before the call, all iterators and references are invalidated.
	 * Otherwise, only the iterators and references after the insertion point remain valid.
	 */
	template <class ... Args>
	constexpr iterator emplace_move_front(const_iterator pos, Args&& ... args) {
		if (pos == begin())
		{
			emplace_front(std::forward<Args>(args)...);
			return begin();
		}
		else if(pos == end())
		{
			emplace_back(std::forward<Args>(args)...);
		}
		else if(capacity() == size())
		{
			return emplace_reallocate(pos, std::forward<Args>(args)...);
		}
		assert(size() != 0);
		auto idx = pos - begin();
		emplace_front(std::move(front()));
		auto p = begin() + idx;
		copy_ranges(
			as_moving_split_ranges(std::next(begin(), 2), std::next(p)), 
			as_split_ranges(std::next(begin()), p));
		*p = value_type(std::forward<Args>(args)...);
		return p;
	}

	/**
	 * @brief
	 * Emplaces the given element at the past-the-end position in the buffer.
	 *
	 * @param args Arguments with which the new element is to be constructed.
	 * 
	 * @note
	 * The element is constructed through std::allocator_traits::construct, which typically uses placement-new
	 * to construct the element in-place.
	 * @note
	 * The arguments args... are forwarded to the constructor as std::forward<Args>(args)....
	 * args... may directly or indirectly refer to a value in the container.
	 * @note
	 * If the new size() is greater than capacity(), all iterators and references are invalidated.
	 * Otherwise, only the iterators and references before the insertion point remain valid.
	 * The past-the-end iterator is also invalidated. 
	 */
	template <class ... Args>
	constexpr reference emplace_back(Args&& ... args) {
		if(size() < capacity()) {
			return emplace_back_fast(std::forward<Args>(args)...);
		}
		auto tmp = make_temporary_buffer(grow_size(this->capacity()));
		auto pos = tmp.begin() + this->size();
		tmp.construct(pos.get_raw_pointer(), std::forward<Args>(args)...);
		{
			auto guard_ = detail::make_manual_scope_guard([&tmp, &pos](){
				tmp.destroy(pos.get_raw_pointer());
			});
			for (const auto& r : get_ranges().as_moving_ranges())
			{
				tmp.append_no_split(r.begin(), r.end());
			}
			guard_.active = false;
		}
		++tmp.size_;
		this->commit(tmp);
		return back();
	}

	/**
	 * @brief
	 * Emplaces the given element at the beginning of the buffer.
	 *
	 * @param args Arguments with which the new element is to be constructed.
	 * 
	 * @note
	 * The element is constructed through std::allocator_traits::construct, which typically uses placement-new
	 * to construct the element in-place.
	 * @note
	 * The arguments args... are forwarded to the constructor as std::forward<Args>(args)....
	 * args... may directly or indirectly refer to a value in the container.
	 * @note
	 * If the new size() is greater than capacity(), or if begin_index() after calling this function is greater
	 * than before the call, all iterators and references are invalidated.
	 */
	template <class ... Args>
	constexpr reference emplace_front(Args&& ... args) {
		if(size() < capacity()) {
			return emplace_front_fast(std::forward<Args>(args)...);
		}
		auto tmp = make_temporary_buffer(grow_size(this->capacity()));
		tmp.construct(tmp.data_, std::forward<Args>(args)...);
		tmp.size_ = 1;
		for(auto range: get_ranges().as_moving_ranges())
		{
			tmp.append_no_split(range.begin(), range.end());
		}
		commit(tmp);
		return front();
	}


	/**
	 * @brief
	 * Removes the element at 'pos'. 
	 * 
	 * If std::distance(begin(), pos) < std::distance(pos, end()) then the element is erased as if by calling
	 * erase_move_front(), otherwise the erasure proceeds as if by calling erase_move_back().
	 * If std::distance(begin(), pos) < std::distance(pos, end()), then iterators and references at or before the point of the erasure are invalidated,
	 * otherwise iterators and references at or after the point of the erasure are invalidated.
	 * Additionally, if begin_index() after calling this function is less than its value before calling the function, then all iterators (but not necessarily
	 * references) are invalidated.
	 * The iterator pos must be valid and dereferenceable.
	 * Thus the end() iterator (which is valid, but is not dereferenceable) cannot be used as a value for pos.
	 * 
	 * @param pos Iterator to the to-be-erased element.
	 */
	constexpr iterator erase(const_iterator pos)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(pos != end());
		size_type count_before = pos - begin();
		size_type count_after = (size() - count_before) - 1;
		if(count_before < count_after)
		{
			return erase_move_front(pos);
		}
		else
		{
			return erase_move_back(pos);
		}
	}
	
	/**
	 * @brief
	 * Removes the element at 'pos'.  
	 * Elements in the range [pos, end()) are shifted to occupy the erased space.
	 * 
	 * Iterators and references at or after the point of the erasure are invalidated.
	 * The iterator pos must be valid and dereferenceable.
	 * Thus the end() iterator (which is valid, but is not dereferenceable) cannot be used as a value for pos.
	 * 
	 * @param pos Iterator to the to-be-erased element.
	 */
	constexpr iterator erase_move_back(const_iterator pos)
	{
		auto p = as_non_const_iterator(pos);
		auto ranges = as_split_ranges(p, end());
		TIM_CIRCULAR_BUFFER_ASSERT(ranges.size() <= 2);
		TIM_CIRCULAR_BUFFER_ASSERT(ranges.size() != 0);
		std::move(std::next(ranges[0].begin()), ranges[0].end(), ranges[0].begin());
		if (ranges.size() == 1)
		{
			--size_;
			destroy(ranges[0].end() - 1);
			return p;
		}
		TIM_CIRCULAR_BUFFER_ASSERT(ranges.size() == 2);
		TIM_CIRCULAR_BUFFER_ASSERT(ranges[1].size() != 0);
		ranges[0].end()[-1] = std::move(*ranges[1].begin());
		std::move(std::next(ranges[1].begin()), ranges[1].end(), ranges[1].begin());
		--size_;
		destroy(ranges[1].end() - 1);
		return p;
	}
	
	/**
	 * @brief
	 * Removes the element at 'pos'.  
	 * Elements in the range [begin(), pos) are shifted to occupy the erased space.
	 * 
	 * Iterators and references at or before the point of the erasure are invalidated.
	 * Additionally, if begin_index() after calling this function is less than its value before calling the function, then all iterators (but not necessarily
	 * references) are invalidated.
	 * The iterator pos must be valid and dereferenceable.
	 * Thus the end() iterator (which is valid, but is not dereferenceable) cannot be used as a value for pos.
	 * 
	 * @param pos Iterator to the to-be-erased element.
	 */
	constexpr iterator erase_move_front(const_iterator pos)
	{
		auto p = as_non_const_iterator(pos);
		auto ranges = as_split_ranges(begin(), std::next(p));
		TIM_CIRCULAR_BUFFER_ASSERT(ranges.size() <= 2);
		TIM_CIRCULAR_BUFFER_ASSERT(ranges.size() != 0);
		switch (ranges.size())
		{
		case 2:
			TIM_CIRCULAR_BUFFER_ASSERT(ranges[1].size() != 0);
			TIM_CIRCULAR_BUFFER_ASSERT(ranges[0].size() != 0);
			std::move_backward(ranges[1].begin(), ranges[1].end()-1, ranges[1].end());
			*ranges[1].begin() = std::move(ranges[0].end()[-1]);
			[[fallthrough]];
		case 1:
			std::move_backward(ranges[0].begin(), ranges[0].end()-1, ranges[0].end());
			break;
		default:
			TIM_CIRCULAR_BUFFER_ASSERT_UNREACHABLE();
			break;
		}
		destroy(ranges[0].begin());
		--size_;
		auto new_start = start_ + 1;
		if (new_start == cap_)
		{
			new_start = 0;
		}
		start_ = new_start;
		return std::next(p);
	}

	/**
	 * @brief
	 * Removes the elements in the range [first, last).
	 * 
	 * If std::distance(begin(), first) < std::distance(last, end()) then the element is erased as if by calling
	 * erase_move_front(), otherwise the erasure proceeds as if by calling erase_move_back().
	 * If std::distance(begin(), first) < std::distance(last, end()), then iterators and references at or before the point of the erasure are invalidated,
	 * otherwise iterators and references at or after the point of the erasure are invalidated.
	 * Additionally, if begin_index() after calling this function is less than its value before calling the function, then all iterators (but not necessarily
	 * references) are invalidated.
	 * The iterator pos must be valid and dereferenceable.
	 * Thus the end() iterator (which is valid, but is not dereferenceable) cannot be used as a value for 'first' unless 'first == last'.
	 * 
	 * @param first Iterator to beginning of the to-be-erased erased.
	 * @param last  Iterator past the last element of the to-be-erased erased.
	 */
	constexpr iterator erase(const_iterator start, const_iterator stop)
	{
		size_type count_before = start - begin();
		size_type count_after = end() - stop;
		if(count_before < count_after)
		{
			return erase_move_front(start, stop);
		}
		else
		{
			return erase_move_back(start, stop);
		}
	}

	/**
	 * @brief
	 * Removes the elements in the range [first, last).
	 * Elements in the range [last, end()) are shifted to occupy the erased space.
	 * 
	 * Iterators and references at or after the point of the erasure are invalidated.
	 * The iterator pos must be valid and dereferenceable.
	 * Thus the end() iterator (which is valid, but is not dereferenceable) cannot be used as a value for 'first' unless 'first == last'.
	 * 
	 * @param first Iterator to beginning of the to-be-erased erased.
	 * @param last  Iterator past the last element of the to-be-erased erased.
	 */
	constexpr iterator erase_move_back(const_iterator start, const_iterator stop)
	{
		auto start_pos = as_non_const_iterator(start);
		auto stop_pos = as_non_const_iterator(stop);
		auto count_to_move = end() - stop;
		copy_ranges(as_moving_split_ranges(stop_pos, end()), as_split_ranges(start_pos, start_pos + count_to_move));
		pop_back_n(stop_pos - start_pos);
		return start_pos;
	}
	/**
	 * @brief
	 * Removes the elements in the range [first, last).
	 * Elements in the range [begin(), pos) are shifted to occupy the erased space.
	 * 
	 * If std::distance(begin(), first) < std::distance(last, end()) then the element is erased as if by calling
	 * erase_move_front(), otherwise the erasure proceeds as if by calling erase_move_back().
	 * If std::distance(begin(), first) < std::distance(last, end()), then iterators and references at or before the point of the erasure are invalidated,
	 * otherwise iterators and references at or after the point of the erasure are invalidated.
	 * Additionally, if begin_index() after calling this function is less than its value before calling the function, then all iterators (but not necessarily
	 * references) are invalidated.
	 * The iterator pos must be valid and dereferenceable.
	 * Thus the end() iterator (which is valid, but is not dereferenceable) cannot be used as a value for 'first' unless 'first == last'.
	 * 
	 * @param first Iterator to beginning of the to-be-erased erased.
	 * @param last  Iterator past the last element of the to-be-erased erased.
	 */
	constexpr iterator erase_move_front(const_iterator start, const_iterator stop)
	{
		auto start_pos = as_non_const_iterator(start);
		auto stop_pos = as_non_const_iterator(stop);
		auto count_to_move = start_pos - begin();
		copy_ranges_backward(as_moving_split_ranges(begin(), start_pos), as_split_ranges(stop_pos - count_to_move, stop_pos));
		pop_front_n(stop_pos - start_pos);
		return stop_pos;
	}

	/**
	 * @brief
	 * Erases all elements for which 'pred' returns true.
	 * 
	 * @param pred Predicate function object which returns 'true' for elements that should be erased.
	 * 
	 * @note
	 * After calling this function, the values of begin_index() and end_index() are unspecified.
	 * @note
	 * The expression pred(v) must be convertible to bool for every argument v of type (possibly const) T,
	 * regardless of value category, and must not modify v.
	 * Thus, a parameter type of T& is not allowed, nor is T unless for T a move is equivalent to a copy.
	 */
	template <class Pred>
	constexpr size_type erase_if(Pred pred)
	{
		const size_type size_at_start = size();
		auto ranges = get_ranges();
		if(ranges.size() == 2)
		{
			auto front_pos = std::find_if(ranges[0].begin(), ranges[0].end(), std::not_fn(pred));
			pop_front_n(size_from_iterator_difference(front_pos - ranges[0].begin()));
			if (front_pos == ranges[0].end())
			{
				ranges[0] = ranges[1];
				goto one_range;
			}
			auto back_pos = std::find_if(ranges[1].rbegin(), ranges[1].rend(), std::not_fn(pred));
			pop_back_n(size_from_iterator_difference(back_pos - ranges[1].rbegin()));
			if (back_pos == ranges[1].rend())
			{
				goto one_range;
			}
			auto [first, last] = detail::bidirectional_remove_if(begin(), end(), pred);
			auto front_count = first - begin();
			auto back_count = end() - last;
			pop_front_n(front_count);
			pop_back_n(back_count);
		}
		else if (ranges.size() == 1)
		{
		one_range:
			auto [first, last] = detail::bidirectional_remove_if(ranges[0].begin(), ranges[0].end(), pred);
			if (first == last)
			{
				clear_fast();
				return size_at_start;
			}
			for (auto p = ranges[0].begin(); p != first; ++p)
			{
				destroy(std::addressof(*p));
			}
			for (auto p = last; p != ranges[0].end(); ++p)
			{
				destroy(std::addressof(*p));
			}
			start_ = size_from_iterator_difference(first - data_);
			size_ = size_from_iterator_difference(last - first);
		}
		return size_at_start - size();
	}
	
	/**
	 * @brief
	 * Appends the given element value to the end of the container. 
	 * The new element is initialized as a copy of value.
	 * 
	 * @param value The value of the element to append.
	 * 
	 * @note
	 * T must meet the requirements of CopyInsertable in order to use overload.
	 * @note
	 * If the new size() is greater than capacity() then all iterators and references (including the past-the-end iterator) are invalidated.
	 * Otherwise only the past-the-end iterator is invalidated. 
	 */
	constexpr void push_back(const T& value)
	{
		emplace_back(value);
	}

	/**
	 * @brief
	 * Appends the given element value to the end of the container. 
	 * 'value' is moved into the new element.
	 * 
	 * @param value The value of the element to append.
	 * 
	 * @note
	 * T must meet the requirements of MoveInsertable in order to use overload.
	 * @note
	 * If the new size() is greater than capacity() then all iterators and references (including the past-the-end iterator) are invalidated.
	 * Otherwise only the past-the-end iterator is invalidated. 
	 * @note
	 * If an exception is thrown (which can be due to Allocator::allocate() or element copy/move constructor/assignment), this function has no effect (strong exception guarantee).
	 * @note
	 * If T's move constructor is not noexcept and T is not CopyInsertable into *this, the implementation will use the throwing move constructor.
	 * If it throws, the guarantee is waived and the effects are unspecified. 
	 */
	constexpr void push_back(T&& value)
	{
		emplace_back(std::move(value));
	}

	/**
	 * @brief
	 * Prepends the given element value to the beginning of the container. 
	 * The new element is initialized as a copy of value.
	 * 
	 * @param value The value of the element to prepend.
	 * 
	 * @note
	 * T must meet the requirements of CopyInsertable in order to use overload.
	 * @note
	 * If the new size() is greater than capacity(), or if the new begin_index() is greater than its previous value,
	 * then all iterators and references are invalidated.
	 * Otherwise no iterators are invalidated. 
	 */
	constexpr void push_front(const T& value)
	{
		emplace_front(value);
	}

	/**
	 * @brief
	 * Prepends the given element value to the beginning of the container. 
	 * The new element is initialized as a copy of value.
	 * 
	 * @param value The value of the element to prepend.
	 * 
	 * @note
	 * T must meet the requirements of CopyInsertable in order to use overload.
	 * @note
	 * If the new size() is greater than capacity(), or if the new begin_index() is greater than its previous value,
	 * then all iterators and references are invalidated.
	 * Otherwise no iterators are invalidated. 
	 * @note
	 * If an exception is thrown (which can be due to Allocator::allocate() or element copy/move constructor/assignment), this function has no effect (strong exception guarantee).
	 * @note
	 * If T's move constructor is not noexcept and T is not CopyInsertable into *this, the implementation will use the throwing move constructor.
	 * If it throws, the guarantee is waived and the effects are unspecified. 
	 */
	constexpr void push_front(T&& value)
	{
		emplace_front(std::move(value));
	}

	/**
	 * @brief
	 * Removes the last element of the container.
	 * 
	 * Calling pop_back on an empty container results in undefined behavior.
	 * Iterators and references to the last element, as well as the end() iterator, are invalidated. 
	 */
	constexpr void pop_back()
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size() != 0);
		--size_;
		destroy(end().get_pointer());
	}

	/**
	 * @brief
	 * Removes the last 'count' elements from the container.
	 * 
	 * If 'count' is zero then there are no effects.
	 * If 'count' is non-zero, then calling pop_back_n on an empty container results in undefined behavior.
	 * Additionally, if 'count' is non-zero, iterators and references to the last 'count' elements, as well as the end() iterator, are invalidated.
	 */
	constexpr void pop_back_n(size_type count)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size() >= count);
		for (const auto& r : detail::reversed(get_ranges().last(count)))
		{
			for (T& item : detail::reversed(r))
			{
				destroy(std::addressof(item));
			}
		}
		size_ -= count;
	}

	/**
	 * @brief
	 * Removes the first element of the container.
	 * 
	 * Calling pop_front on an empty container results in undefined behavior.
	 * If the new begin_index() is less than its previous value then all iterators and references are invalidated.
	 * Otherwise only iterators and references to the first element are invalidated. 
	 */
	constexpr void pop_front()
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size() != 0);
		destroy(begin().get_pointer());
		--size_;
		auto new_start = start_ + 1;
		if (new_start == capacity())
		{
			new_start = 0;
		}
		start_ = new_start;
	}

	/**
	 * @brief
	 * Removes the first 'count' elements from the container.
	 * 
	 * If 'count' is zero then there are no effects.
	 * If 'count' is non-zero, then calling pop_front_n on an empty container results in undefined behavior.
	 * If the new begin_index() is less than its previous value then all iterators and references are invalidated.
	 * Otherwise only iterators and references to the first 'count' elements are invalidated. 
	 */
	constexpr void pop_front_n(size_type count)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size() >= count);
		auto new_begin_index = compute_offset(begin_index(), count, capacity());
		for (const auto& r : get_ranges().first(count))
		{
			for (T& item : r)
			{
				destroy(std::addressof(item));
			}
		}
		size_ -= count;
		start_ = new_begin_index;
	}

	/**
	 * @brief
	 * Resizes the container to contain count elements.
	 * 
	 * If the current size is greater than count, the container is reduced to its first count elements.
	 * If the current size is less than count, additional default-inserted elements are appended.
	 * 
	 * @param count The new size of the container.
	 * 
	 * @note
	 * T must meet the requirements of MoveInsertable and DefaultInsertable in order to use overload.
	 * If an exception is thrown, this function has no effect (strong exception guarantee). 
	 * @note
	 * If T's move constructor is not noexcept and T is not CopyInsertable into *this, the throwing move constructor will be used.
	 * If it throws, the guarantee is waived and the effects are unspecified.
	 */
	constexpr void resize(size_type count)
	{
		if(count > size())
		{
			append(detail::DefaultValueIterator<T>(0), detail::DefaultValueIterator<T>(count - size()));
		}
		else if (count < size())
		{
			pop_back_n(size() - count);
		}
	}

	/**
	 * @brief
	 * Resizes the container to contain count elements.
	 * 
	 * If the current size is greater than count, the container is reduced to its first count elements.
	 * If the current size is less than count, additional copies of 'value' are appended.
	 * 
	 * @param count The new size of the container.
	 * @param value The value to initialize the new elements with.
	 * 
	 * @note
	 * T must meet the requirements of MoveInsertable and DefaultInsertable in order to use overload.
	 * @note
	 * If an exception is thrown, this function has no effect (strong exception guarantee). 
	 */
	constexpr void resize(size_type count, const T& value)
	{
		if(count > size())
		{
			append(detail::SingleValueIterator<T>(std::addressof(value), 0), detail::SingleValueIterator<T>(std::addressof(value), count-size()));
		}
		else if (count < size())
		{
			pop_back_n(size() - count);
		}
	}

	/**
	 * @brief
	 * Resizes the container to contain count elements.
	 * 
	 * If the current size is greater than count, the container is reduced to its last count elements.
	 * If the current size is less than count, additional default-inserted elements are prepended.
	 * 
	 * @param count The new size of the container.
	 * 
	 * @note
	 * T must meet the requirements of MoveInsertable and DefaultInsertable in order to use overload.
	 * If an exception is thrown, this function has no effect (strong exception guarantee). 
	 * @note
	 * If T's move constructor is not noexcept and T is not CopyInsertable into *this, the throwing move constructor will be used.
	 * If it throws, the guarantee is waived and the effects are unspecified.
	 */
	constexpr void resize_front(size_type count)
	{
		if(count > size())
		{
			prepend(detail::DefaultValueIterator<T>(0), detail::DefaultValueIterator<T>(count-size()));
		}
		else if (count < size())
		{
			pop_front_n(size() - count);
		}
	}

	/**
	 * @brief
	 * Resizes the container to contain count elements.
	 * 
	 * If the current size is greater than count, the container is reduced to its last count elements.
	 * If the current size is less than count, additional copies of 'value' are prepended.
	 * 
	 * @param count The new size of the container.
	 * @param value The value to initialize the new elements with.
	 * 
	 * @note
	 * T must meet the requirements of MoveInsertable and DefaultInsertable in order to use overload.
	 * @note
	 * If an exception is thrown, this function has no effect (strong exception guarantee). 
	 */
	constexpr void resize_front(size_type count, const T& value)
	{
		if(count > size())
		{
			prepend(detail::SingleValueIterator<T>(std::addressof(value), 0), detail::SingleValueIterator<T>(std::addressof(value), count-size()));
		}
		else if (count < size())
		{
			pop_front_n(size() - count);
		}
	}

	/**
	 * @brief
	 * Adds elements in the range [first, last) to the end of the container, as if by calling 'v.insert(v.end(), first, last)'.
	 * 
	 * @param first Iterator to the first element in the range of values to append.
	 * @param last  The past-the-end iterator for the range of values to append.
	 * 
	 * @note
	 * If an exception is thrown while copying elements in the range [first, last) to their destination location in the
	 * container, there are no effects.
	 * @note
	 * If T's move constructor is not noexcept and T is not CopyInsertable into *this, the throwing move constructor will be used.
	 * If it throws, the guarantee is waived and the effects are unspecified.
	 */
	template <
		class It,
		std::enable_if_t<
			detail::is_one_of_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::input_iterator_tag
			>,
			bool
		> = false
	>
	constexpr void append(It first, It last) {
		const size_type spare = this->capacity() - this->size();
		size_type count_inserted = 0u;
		auto guard_ = detail::make_manual_scope_guard([this, spare, &count_inserted](){
			this->pop_back_n(count_inserted);
		});
		while(first != last && count_inserted < spare) {
			this->emplace_back(*first++);
			++count_inserted;
		}
		if(count_inserted < spare) {
			TIM_CIRCULAR_BUFFER_ASSERT(first == last);
			guard_.active = false;
			return;
		}
		// Push the remainder of the iterator range into 'tmp'.
		// Once we've successfully copied everything else into 'tmp', we'll transfer over
		// all of the stuff still contained in '*this'.
		auto tmp = make_temporary_buffer(grow_size(this->capacity()));
		tmp.start_ = compute_offset(this->start_, this->size(), tmp.capacity());
		for(;;) {
			// Need to leave room for the stuff currently contained in '*this'.
			size_type size_limit = tmp.capacity() - this->size();
			while(first != last && tmp.size() < size_limit) {
				tmp.emplace_back(*first++);
			}
			if(tmp.size() < size_limit) {
				TIM_CIRCULAR_BUFFER_ASSERT(first == last);
				break;
			}
			else if (first == last)
			{
				TIM_CIRCULAR_BUFFER_ASSERT(tmp.size() == size_limit);
				break;
			}
			tmp.reserve_back(grow_size(tmp.capacity()));
		}
		for(auto range: detail::reversed(this->get_ranges())) {
			tmp.prepend_fast(
				try_make_move_iterator(range.begin()),
				try_make_move_iterator(range.end())
			);
		}
		guard_.active = false;
		this->commit(tmp);
	}

	/**
	 * @brief
	 * Adds elements in the range [first, last) to the end of the container, as if by calling 'insert(end(), first, last)'.
	 * 
	 * @param first Iterator to the first element in the range of values to append.
	 * @param last  The past-the-end iterator for the range of values to append.
	 * 
	 * @note
	 * If an exception is thrown while copying elements in the range [first, last) to their destination location in the
	 * container, there are no effects.
	 * @note
	 * If T's move constructor is not noexcept and T is not CopyInsertable into *this, the throwing move constructor will be used.
	 * If it throws, the guarantee is waived and the effects are unspecified.
	 */
	template <
		class It,
		std::enable_if_t<
			std::is_convertible_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::forward_iterator_tag
			>,
			bool
		> = false
	>
	constexpr void append(It first, It last) {
		auto count = size_from_iterator_difference(std::distance(first, last));
		if(this->capacity() - this->size() >= count) {
			append_fast(first, last, count);
			return;
		}
		auto tmp = make_temporary_buffer(size() + count);
		tmp.start_ = size();
		tmp.append_fast(first, last, count);
		for(auto range: detail::reversed(this->get_ranges().as_moving_ranges())) {
			tmp.prepend_fast(range.begin(), range.end(), range.size());
		}
		this->commit(tmp);
	}

	/**
	 * @brief
	 * Adds elements in the range [first, last) to the beginning of the container, as if by calling 'insert(begin(), first, last)'.
	 * 
	 * @param first Iterator to the first element in the range of values to prepend.
	 * @param last  The past-the-end iterator for the range of values to prepend.
	 * 
	 * @note
	 * If an exception is thrown while copying elements in the range [first, last) to their destination location in the
	 * container, there are no effects.
	 * @note
	 * If T's move constructor is not noexcept and T is not CopyInsertable into *this, the throwing move constructor will be used.
	 * If it throws, the guarantee is waived and the effects are unspecified.
	 * @note
	 * If the new size() is greater than capacity(), or if the new begin_index() is greater than its previous value, then all iterators
	 * and references are invalidated.
	 */
	template <
		class It,
		std::enable_if_t<
			detail::is_one_of_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::input_iterator_tag
			>,
			bool
		> = false
	>
	constexpr void prepend(It first, It last)
	{
		const auto spare = this->capacity() - this->size();
		size_type count_inserted = 0u;
		auto guard_ = detail::make_manual_scope_guard([this, spare, &count_inserted](){
			this->pop_front_n(count_inserted);
		});
		while(first != last && count_inserted < spare) {
			this->emplace_front(*first++);
			++count_inserted;
		}
		if(count_inserted < spare) {
			TIM_CIRCULAR_BUFFER_ASSERT(first == last);
			std::reverse(this->begin(), this->begin() + count_inserted);
			guard_.active = false;
			return;
		}
		// Push the remainder of the iterator range into 'tmp'.
		// Once we've successfully copied everything else into 'tmp', we'll transfer over
		// all of the stuff still contained in '*this'.
		auto tmp = make_temporary_buffer(grow_size(this->capacity()));
		tmp.start_ = this->start_;
		for(;;) {
			// Need to leave room for the stuff currently contained in '*this'.
			size_type size_limit = tmp.capacity() - this->size();
			while(first != last && tmp.size() < size_limit) {
				tmp.emplace_front(*first++);
			}
			if(tmp.size() < size_limit) {
				TIM_CIRCULAR_BUFFER_ASSERT(first == last);
				break;
			}
			tmp.reserve_front(grow_size(this->capacity()));
		}
		std::reverse(tmp.begin(), tmp.end());
		tmp.prepend_fast(
			try_make_move_iterator(this->rend() + count_inserted),
			try_make_move_iterator(this->rend()),
			count_inserted
		);
		size_type skip_count = count_inserted;
		for(auto range: this->get_ranges()) {
			if(range.size() <= skip_count) {
				skip_count -= range.size();
				continue;
			}
			tmp.append_fast(
				try_make_move_iterator(range.begin() + skip_count),
				try_make_move_iterator(range.end())
			);
			skip_count = 0u;
		}
		guard_.active = false;
		this->commit(tmp);
	}

	/**
	 * @brief
	 * Adds elements in the range [first, last) to the beginning of the container, as if by calling 'insert(begin(), first, last)'.
	 * 
	 * @param first Iterator to the first element in the range of values to prepend.
	 * @param last  The past-the-end iterator for the range of values to prepend.
	 * 
	 * @note
	 * If an exception is thrown while copying elements in the range [first, last) to their destination location in the
	 * container, there are no effects.
	 * @note
	 * If T's move constructor is not noexcept and T is not CopyInsertable into *this, the throwing move constructor will be used.
	 * If it throws, the guarantee is waived and the effects are unspecified.
	 * @note
	 * If the new size() is greater than capacity(), or if the new begin_index() is greater than its previous value, then all iterators
	 * and references are invalidated.
	 */
	template <
		class It,
		std::enable_if_t<
			std::is_convertible_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::forward_iterator_tag
			>,
			bool
		> = false
	>
	constexpr void prepend(It first, It last) {
		auto count = size_from_iterator_difference(std::distance(first, last));
		if(this->capacity() - this->size() >= count) {
			prepend_fast(first, last, count);
			return;
		}
		auto tmp = make_temporary_buffer(size() + count);
		tmp.start_ = this->start_;
		tmp.prepend_fast(first, last, count);
		for(auto range: this->get_ranges()) {
			tmp.append_fast(
				try_make_move_iterator(range.begin()),
				try_make_move_iterator(range.end()),
				range.size()
			);
		}
		this->commit(tmp);
	}

	/** 
	 * @brief
	 * Modifies the underlying storage such that the starting index of
	 * the buffer is shifted right by 'count', without changing the order
	 * of the elements in the buffer.
	 *
	 * Example:
	 *     Underlying storage before shifting:
	 *         [-, a, b, c, d, -, -, -, -]
	 *             ^ begin_index() == 1
	 *     Underlying storage after shifting right by 2:
	 *         [-, -, -, a, b, c, d, -, -]
	 *                   ^ begin_index() == 3
	 *
	 * @param count The number of elements to shift the buffer right by.
	 * @note If T is MoveInsertible and std::is_nothrow_move_constructible_v<T> is true,
	 * and std::is_nothrow_move_assignable_v<T> is true, this function throws only those
	 * exceptions thrown when move-constructing objects of type T using
	 * std::allocator_traits<Allocator>::construct().
	 * Otherwise if T is MoveInsertible and CopyInsertible but has a throwing move
	 * constructor or move assignment operator, elements are shifted using copy
	 * construction and copy assignment.
	 * In both of the above two cases, the strong exception guarantee is maintained.
	 * Otherwise, if T is not CopyInsertible, the strong exception gaurantee is waived;
	 * elements are shifted using move construction and move assignment, but if an
	 * exception is thrown, the buffer will be left in a valid but unspecified state.
	 * @note This function does not allocate, all data movement is done in-place.
	 * @note This function makes O(this->size()) calls to T's move/copy constructor and 
	 * assignment operator.
	 */
	constexpr void shift_right(size_type count) {
		assert(count <= capacity());
		shift_left(capacity() - count);
	}

	/** 
	 * @brief
	 * Modifies the underlying storage such that the starting index of
	 * the buffer is shifted left by 'count', without changing the order
	 * of the elements in the buffer. 
	 *
	 * Example:
	 *     Underlying storage before shifting:
	 *         [-, -, -, a, b, c, d, -, -]
	 *                   ^ begin_index() == 3
	 *     Underlying storage after shifting left by 2:
	 *         [-, a, b, c, d, -, -, -, -]
	 *             ^ begin_index() == 1
	 *
	 * @param count The number of elements to shift the buffer left by.
	 * @note If T is MoveInsertible and std::is_nothrow_move_constructible_v<T> is true,
	 * and std::is_nothrow_move_assignable_v<T> is true, this function throws only those
	 * exceptions thrown when move-constructing objects of type T using
	 * std::allocator_traits<Allocator>::construct().
	 * Otherwise if T is MoveInsertible and CopyInsertible but has a throwing move
	 * constructor or move assignment operator, elements are shifted using copy
	 * construction and copy assignment.
	 * In both of the above two cases, the strong exception guarantee is maintained.
	 * Otherwise, if T is not CopyInsertible, the strong exception gaurantee is waived;
	 * elements are shifted using move construction and move assignment, but if an
	 * exception is thrown, the buffer will be left in a valid but unspecified state.
	 * @note This function does not allocate, all data movement is done in-place.
	 * @note This function makes O(this->size()) calls to T's move/copy constructor and 
	 * assignment operator.
	 */
	constexpr void shift_left(size_type count) {
		assert(count <= capacity());
		if(count == capacity() || count == 0u) {
			return;
		}
		if(size() == capacity()) {
			std::rotate(data_, data_ + count, data_ + capacity());
			if(start_ >= count)
			{
				start_ -= count;
			}
			else
			{
				start_ = cap_ - (count - start_);
			}
			return;
		}
		size_type used = size();
		size_type spare = (capacity() - used);
		bool has_front_overlap = count < used;
		bool has_back_overlap = count > spare;
		if(has_front_overlap) {
			if(has_back_overlap) {
				shift_left_both_overlap(count);
			} else {
				shift_left_front_overlap(count);
			}
		} else {
			if(has_back_overlap) {
				shift_left_back_overlap(count);
			} else {
				shift_left_no_overlap(count);
			}
		}
	}


	/**
	 * @brief
	 * Returns a `shape_type` describing the layout of the buffer.
	 */
	constexpr shape_type buffer_shape() const { return {capacity(), begin_index()}; }

	/**
	 * @brief
	 * Exchanges the contents of the container with those of other.
	 * 
	 * Does not invoke any move, copy, or swap operations on individual elements.
	 * All iterators and references remain valid. The past-the-end iterator is invalidated. 
	 * 
	 * @param other The container to exchange the contents with.
	 * 
	 * @note
	 * If std::allocator_traits<allocator_type>::propagate_on_container_swap::value is true, then the allocators are exchanged using an unqualified call to non-member swap.
	 * Otherwise, they are not swapped (and if get_allocator() != other.get_allocator(), the behavior is undefined). 
	 */
	constexpr void swap(CircularBuffer& other) noexcept(propagate_on_container_swap || is_always_equal)
	{
		using std::swap;
		swap(data_, other.data_);
		swap(cap_, other.cap_);
		swap(size_, other.size_);
		swap(start_, other.start_);
		if constexpr (propagate_on_container_swap)
		{
			swap(alloc(), other.alloc());
		}
		else if constexpr(!is_always_equal)
		{
			TIM_CIRCULAR_BUFFER_ASSERT(alloc() == other.alloc());
		}
	}

	/**
	 * @brief
	 * Exchanges the contents of 'l' with those of 'r'.
	 * 
	 * Does not invoke any move, copy, or swap operations on individual elements.
	 * All iterators and references remain valid. The past-the-end iterator is invalidated. 
	 * 
	 * @param l The first container whose contents will be exchanged.
	 * @param r The second container whose contents will be exchanged.
	 * 
	 * @note
	 * If std::allocator_traits<allocator_type>::propagate_on_container_swap::value is true, then the allocators are exchanged using an unqualified call to non-member swap.
	 * Otherwise, they are not swapped (and if get_allocator() != other.get_allocator(), the behavior is undefined). 
	 */
	friend constexpr void swap(CircularBuffer& l, CircularBuffer& r) noexcept(propagate_on_container_swap || is_always_equal)
	{
		l.swap(r);
	}

	/**
	 * @brief
	 * Checks if the contents of lhs and rhs are equal, that is, they have the same number of elements and each element in lhs compares equal with the element in rhs at the same position.
	 * 
	 * @param l The first container whose contents will be compared.
	 * @param r The second container whose contents will be compared.
	 * 
	 * @return true if the contents of the buffers are equal, false otherwise
	 */
	friend constexpr bool operator==(const CircularBuffer& l, const CircularBuffer& r) 
	{
		if (l.size() != r.size())
		{
			return false;
		}
		auto l_ranges = l.get_ranges();
		if (l_ranges.size() == 0)
		{
			TIM_CIRCULAR_BUFFER_ASSERT(l.size() == 0);
			TIM_CIRCULAR_BUFFER_ASSERT(r.size() == 0);
			return true;
		}
		auto r_ranges = r.get_ranges();
		TIM_CIRCULAR_BUFFER_ASSERT(r_ranges.size() != 0);

		if (l_ranges[0].size() > r_ranges[0].size())
		{
			using std::swap;
			swap(l_ranges, r_ranges);
		}
		if (l_ranges[0].size() < r_ranges[0].size())
		{
			if (!std::equal(l_ranges[0].begin(), l_ranges[0].end(), r_ranges[0].begin(), r_ranges[0].begin() + l_ranges[0].size()))
			{
				return false;
			}
			if (r_ranges[1].size() == 0)
			{
				return std::equal(l_ranges[1].begin(), l_ranges[1].end(), r_ranges[0].begin() + l_ranges[0].size(), r_ranges[0].end());
			}
			auto rem = r_ranges[0].size() - l_ranges[0].size();
			if (!std::equal(l_ranges[1].begin(), l_ranges[1].begin() + rem, r_ranges[0].end() - rem, r_ranges[0].end()))
			{
				return false;
			}
			return std::equal(l_ranges[1].begin() + rem, l_ranges[1].end(), r_ranges[1].begin(), r_ranges[1].end());
		}
		else
		{
			return std::equal(l_ranges[0].begin(), l_ranges[0].end(), r_ranges[0].begin(), r_ranges[0].end())
				&& std::equal(l_ranges[1].begin(), l_ranges[1].end(), r_ranges[1].begin(), r_ranges[1].end());
		}
	}
	
	/**
	 * @brief
	 * Checks if the contents of lhs and rhs are not equal, that is, they have a different number of elements or if any element in lhs compares not equal with the element in rhs at the same position.
	 * 
	 * @param l The first container whose contents will be compared.
	 * @param r The second container whose contents will be compared.
	 * 
	 * @return false if the contents of the buffers are equal, true otherwise
	 */
	friend constexpr bool operator!=(const CircularBuffer& l, const CircularBuffer& r) = default;

private:

	template <class It>
	constexpr void insert_slow(const_iterator pos, It first, It last, size_type count) {
		auto tmp = make_temporary_buffer(size() + count);
		tmp.start_ = pos.get_index();
		tmp.append_fast(first, last, count);
		for(auto range: as_split_ranges(as_non_const_iterator(pos), end())) {
			tmp.append_fast(
				try_make_move_iterator(range.begin()),
				try_make_move_iterator(range.end())
			); 
		}
		for(auto range: detail::reversed(as_split_ranges(begin(), as_non_const_iterator(pos)))) {
			tmp.prepend_fast(
				try_make_move_iterator(range.begin()),
				try_make_move_iterator(range.end())
			);
		}
		commit(tmp);
	}

	template <class It>
	constexpr void insert_move_back_fast_2(const_iterator pos, It first, It last, size_type count) {
		if(storage_is_split()) {
			if(pos.get_has_wrapped()) {
				return insert_move_back_simple(pos, first, last, count);
			}
			pointer pos_addr = pos.get_pointer();
			pointer split_pos = data_ + cap_;
			if(split_pos - pos_addr >= count) {
				pointer first_range = pos_addr + count;
				size_type first_range_size = (split_pos - pos_addr) - count;
				pointer second_range = data_;
				size_type second_range_size = end().get_pointer() - second_range;
				if(second_range_size >= count) {
					this->append_fast(
						std::make_move_iterator(second_range + (second_range_size - count)),
						std::make_move_iterator(second_range + second_range_size),
						count
					);
					this->append_fast(
						std::make_move_iterator(second_range + (second_range_size - count)),
						std::make_move_iterator(second_range + second_range_size),
						count
					);
				} else {
					this->append_fast(
						std::make_move_iterator(first_range + (first_range_size - (count - second_range_size))),
						std::make_move_iterator(first_range + first_range_size),
						count
					);
				}
				// The inserted range won't need to be wrapped around to the front.
			} else {

			}
		} else {
			if((capacity() - (begin_index() + size()) >= count)) {
				return insert_move_back_simple(pos, first, last, count);
			}
		}
		if(count <= (pos.get_pointer() - (data_ + capacity()))) {
		} else {
			// inserted range *will* overlap with the physical end of the buffer

		}
	}

	template <class It>
	constexpr void insert_move_back_simple(const_iterator pos, It first, It last, size_type count) {
		pointer start = as_non_const_iterator(pos).get_pointer();
		pointer stop = end().get_pointer();
		size_type dist_to_end = stop - start;
		if(dist_to_end >= count) {
			// Inserted range won't overlap with the end.
			this->append_fast(
				std::make_move_iterator(stop - count),
				std::make_move_iterator(stop),
				count
			);
			std::move_backward(start, stop - count, stop);
			std::copy(first, last, as_non_const_iterator(pos));
		} else {
			It mid = detail::get_position_in_range(first, last, count, dist_to_end);
			this->append_fast(mid, last, count - dist_to_end);
			this->append_fast(
				std::make_move_iterator(start),
				std::make_move_iterator(stop),
				dist_to_end
			);
			std::copy(first, mid, as_non_const_iterator(pos));
		}
	}


	template <class ... Args>
	constexpr iterator emplace_reallocate(const_iterator pos, Args&& ... args)
	{
		auto tmp = make_temporary_buffer(grow_size(this->capacity()));
		auto idx = size_from_iterator_difference(pos - begin());
		auto p = tmp.begin() + idx;
		tmp.construct(p.get_raw_pointer(), std::forward<Args>(args)...);
		{
			auto guard_ = detail::make_manual_scope_guard([&tmp, &p](){
				tmp.destroy(p.get_raw_pointer());
			});
			tmp.initialize_continuous_buffer_fast(get_ranges().first(idx).as_moving_ranges());
			guard_.active = false;
		}
		++tmp.size_;
		for(auto range: get_ranges().last(size() - idx).as_moving_ranges())
		{
			tmp.append_no_split(range.begin(), range.end());
		}
		commit(tmp);
		return p;
	}

	template <class It>
	constexpr iterator insert_reallocate(const_iterator pos, It first, It last, size_type count)
	{
		size_type new_cap = count + size();
		TIM_CIRCULAR_BUFFER_ASSERT(new_cap > capacity());
		auto buf = make_temporary_buffer(new_cap);
		for (const auto& r : as_moving_split_ranges(begin(), as_non_const_iterator(pos)))
		{
			buf.append_no_split(r.begin(), r.end());
		}
		iterator result = buf.end();
		buf.append_no_split(first, last);
		for (const auto& r : as_moving_split_ranges(as_non_const_iterator(pos), end()))
		{
			buf.append_no_split(r.begin(), r.end());
		}
		commit(buf);
		return result;
	}



	template <class It>
	constexpr iterator insert_move_back_fast(const_iterator pos, It first, It last, size_type count) {
		TIM_CIRCULAR_BUFFER_ASSERT(count <= capacity() - size());
		if(count == 0u)
		{
			return as_non_const_iterator(pos);
		}
		size_type dest_index = pos - begin();
		if(dest_index == size()) {
			append_fast(first, last, count);
			return as_non_const_iterator(pos);
		}
		size_type end_index = dest_index + count;
		TIM_CIRCULAR_BUFFER_ASSERT(end_index <= capacity());
		if(end_index <= size()) {
			// the inserted range won't overlap with the current logical end of the buffer
			// 
			//   [x,x,x,y,y,y]
			//   [z,z] ^
			auto ranges_to_move = as_split_ranges(as_non_const_iterator(pos), end());
			auto total_to_move = ranges_to_move.total();
			//auto old_end = end();
			for (auto r : ranges_to_move.last(count).as_moving_ranges())
			{
				append_fast(r.begin(), r.end());
			}
			copy_ranges_backward(ranges_to_move.first(total_to_move - count).as_moving_ranges(), ranges_to_move.last(total_to_move - count));
			auto a = ranges_to_move;
			auto b = total_to_move;
			(void)a;
			(void)b;
		}
		else {
			// The inserted range will overlap with the current logical end of the buffer.
			// 
			//         [x,x,x,y,y,y]
			//     [z,z,z,z] ^
			// 1. First append the first part of the inserted range
			//         [x,x,x,y,y,y,z]
			//       [z,z,z] ^
			// 2. Then, append the elements that need to be moved to fit the rest of the inserted range
			//         [x,x,x,0,0,0,z,y,y,y]
			//       [z,z,z] ^
			// 2. Then, copy in the elements to insert
			//         [x,x,x,z,z,z,z,y,y,y]
			size_type overlap = end_index - size();
			It overlap_start = detail::get_point_in_iterator_range(first, last, count - overlap, count);
			auto ranges_to_move_from = get_ranges().last(count - overlap);
			append_fast(overlap_start, last, overlap);
			for (auto r : ranges_to_move_from.as_moving_ranges())
			{
				append_fast(r.begin(), r.end(), r.size());
			}
		}
		for (auto r : as_split_ranges(as_non_const_iterator(pos), as_non_const_iterator(pos) + count))
		{
			for (auto& value : r)
			{
				TIM_CIRCULAR_BUFFER_ASSERT(first != last);
				value = *first++;
			}
		}
		TIM_CIRCULAR_BUFFER_ASSERT(first == last);

		return as_non_const_iterator(pos);
	}

	template <class It>
	constexpr iterator insert_move_back_slow(const_iterator pos, It first, It last, size_type count)
	{
		return insert_reallocate(pos, first, last, count);
	}

	template <class It>
	constexpr iterator insert_move_front_slow(const_iterator pos, It first, It last, size_type count)
	{
		return insert_reallocate(pos, first, last, count);
	}

	template <class It>
	constexpr iterator insert_move_front_fast(const_iterator pos, It first, It last, size_type count) {
		TIM_CIRCULAR_BUFFER_ASSERT(count <= capacity() - size());
		if(count == 0u)
		{
			return as_non_const_iterator(pos);
		}
		size_type dest_index = pos - begin();
		if(dest_index == 0) {
			prepend_fast(first, last, count);
			return begin();
		}
		if(dest_index >= count) {
			// The index into the logical buffer is greater than the number of elements we're inserting.
			// 
			// example: dest_index=5,count=3
			//       [w,w,w,x,x,z,z,z]
			//         [y,y,y] ^ 
			// 1. Prepend [w,w,w]:
			//   [w,w,w,0,0,0,x,x,z,z,z]
			//           [y,y,y] ^ 
			// 2. Move [x,x] up to where [w,w,w] used to live.
			//   [w,w,w,x,x,0,0,0,z,z,z]
			//           [y,y,y] ^ 
			// 3. Overwrite the empty slots where [w,x,x] used to live with [y,y,y]. 
			//   [w,w,w,x,x,y,y,y,z,z,z]
			// First we prepend the elements in [w,w,w], then move [x,x] to where [w,w,w] used to live by overwriting the front two elements of [w,w,w],
			// then finally, we overwrite the storage where [w,x,x] used to live with [y,y,y].
			
			// 'to_be_moved' is the range of elements in the storage directly after the elements that we're prepending below, up to the end of the 
			// storage that [first, last) are being written into.  This is the range [x,x] in the above example.  These elements are to be moved into
			// the storage starting at the current logical start of the buffer, where we're moving elements from in the prepend operation below.
			auto to_be_moved = get_ranges().first(dest_index).last(dest_index - count);
			// 'front_ranges' is the destination range for 'to_be_moved'.  It's starts at the current logical start of the buffer and has the same length as
			// 'to_be_moved'.
			auto front_ranges = get_ranges().first(dest_index - count);

			// Prepend the first 'count' elements to make room for the 'count' elements we're inserting.
			for(auto range: detail::reversed(get_ranges().first(count))) {
				prepend(
					std::make_move_iterator(range.begin()),
					std::make_move_iterator(range.end())
				);
			}
			// Now move the elements in the logical range [dest_index, count) to the empty slots at the old logical start of the buffer (the logical start before
			// we did the prepend operation).  Those elements where moved out of the way during the prepend operation.
			copy_ranges(to_be_moved.as_moving_ranges(), front_ranges);
		} else {
			// The index into the logical buffer is less than the number of elements we're inserting.
			// 
			// example: dest_index=5,count=3
			//         [x,x,z,z,z]
			//     [y,y,y] ^ 
			// 1. Prepend [x,x,y]:
			//   [x,x,y,0,0,z,z,z]
			//       [y,y] ^ 
			// 2. Move [y,y] into [0,0][x,x,y]:
			//   [x,x,y,y,y,z,z,z]
			auto mid = detail::get_point_in_iterator_range(first, last, count - dest_index, count);
			auto ranges_to_prepend = get_ranges().first(dest_index);
			//auto dest_ranges = as_split_ranges(begin() + dest_index, begin() + (dest_index + count));
			prepend(first, mid);
			for (const auto& r : detail::reversed(ranges_to_prepend.as_moving_ranges()))
			{
				prepend(r.begin(), r.end());
			}
		}
		// Finally, write the range [first, last) into the space we just made.
		auto new_pos = begin() + dest_index;
		for (auto r: as_split_ranges(new_pos, new_pos + count))
		{
			for (T& value : r)
			{
				TIM_CIRCULAR_BUFFER_ASSERT(first != last);
				value = *first++;
			}
		}
		TIM_CIRCULAR_BUFFER_ASSERT(first == last);
		return new_pos;
	}

	constexpr void shift_left_front_overlap(size_type count) {
		size_type initial_size = size();
		{
			auto guard = make_manual_prepend_guard_if_not_nothrow_move();
			for(auto range: detail::reversed(get_ranges().first(count))) {
				prepend_fast(try_make_move_iterator(range.begin()), try_make_move_iterator(range.end()));
			}
			if constexpr(std::is_nothrow_move_assignable_v<T>) {
				guard.active = false;
			}
			try_move_ranges(
				get_ranges().last(initial_size - count),
				get_ranges().first(initial_size - count)
			);
			guard.active = false;
		}
		pop_back_n(count);
	}

	constexpr void shift_left_back_overlap(size_type count) {
		// Treat as a right shift.
		count = capacity() - count;
		size_type initial_size = size();
		{
			auto guard = make_manual_append_guard_if_not_nothrow_move();
			for(auto range: get_ranges().last(count)) {
				append_fast(try_make_move_iterator(range.begin()), try_make_move_iterator(range.end()));
			}
			if constexpr(std::is_nothrow_move_assignable_v<T>) {
				guard.active = false;
			}
			try_move_ranges(
				get_ranges().first(initial_size - count),
				get_ranges().last(initial_size - count)
			);
			guard.active = false;
		}
		pop_front_n(count);
	}

	constexpr void shift_left_both_overlap(size_type count) {
		// Treat as a right shift.
		count = capacity() - count;
		size_type initial_size = size();
		// Fill the spare ranges.
		auto initial_end = end();
		auto start = initial_end - count;
		auto stop = start + (capacity() - initial_size);
		auto guard = make_manual_append_guard_if_not_nothrow_move();
		for(auto range: as_split_ranges(start, stop)) {
			append_fast(
				try_make_move_iterator(range.begin()),
				try_make_move_iterator(range.end())
			);
		}
		if constexpr(std::is_nothrow_move_assignable_v<T>) {
			guard.active = false;
		}
		// Fill in the front.
		try_move_ranges_backward(
			as_split_ranges(begin(), start),
			as_split_ranges(stop - (start - begin()), stop)
		);
		size_type back_count = count - (capacity() - initial_size);
		TIM_CIRCULAR_BUFFER_ASSERT(back_count == size_from_iterator_difference(initial_end - stop));
		// Fill in the back.
		try_move_ranges(
			as_split_ranges(stop, initial_end),
			as_split_ranges(begin(), begin() + back_count)
		);
		guard.active = false;
		pop_front_n(count);
	}

	constexpr void shift_left_no_overlap(size_type count) {
		// Treat as a right shift.
		count = capacity() - count;
		auto new_begin = begin() + count;
		new_begin.set_has_wrapped(false);
		auto new_end = new_begin + size();
		try_move_construct_ranges(
			get_ranges(),
			as_split_ranges(new_begin, new_end)
		);
		clear_fast();
		start_ = new_begin.tagged_index_;
	}

	template <class Src, class Dest>
	static constexpr void copy_ranges(Src src, Dest dest) {
		TIM_CIRCULAR_BUFFER_ASSERT(dest.total() == src.total());
		auto [outer, inner] = detail::copy_ranges(src.begin(), src.end(), dest.begin());
		TIM_CIRCULAR_BUFFER_ASSERT(outer == dest.end());
		TIM_CIRCULAR_BUFFER_ASSERT(!inner || inner == dest.end()[-1].end());
	}

	template <class Src, class Dest>
	static constexpr void copy_ranges_backward(Src src, Dest dest) {
		TIM_CIRCULAR_BUFFER_ASSERT(dest.total() == src.total());
		auto [outer, inner] = detail::copy_ranges_backward(src.begin(), src.end(), dest.end());
		TIM_CIRCULAR_BUFFER_ASSERT(outer == dest.begin());
		TIM_CIRCULAR_BUFFER_ASSERT(!inner || *inner == dest.begin()->begin());
	}

	// template <class Src, class Dest>
	// constexpr void move_ranges(Src src, Dest dest) {
	// 	copy_ranges(
	// 		detail::SimpleRange(
	// 			std::make_move_iterator(src.begin()),
	// 			std::make_move_iterator(src.end())
	// 		),
	// 		dest
	// 	);
	// }

	// template <class Src, class Dest>
	// constexpr void move_ranges_backward(Src src, Dest dest) {
	// 	copy_ranges_backward(
	// 		detail::SimpleRange(
	// 			std::make_move_iterator(src.begin()),
	// 			std::make_move_iterator(src.end())
	// 		),
	// 		dest
	// 	);
	// }

	template <class Src, class Dest>
	static constexpr void try_move_ranges(Src src, Dest dest) {
		if constexpr (std::is_nothrow_move_constructible_v<T> || !detail::is_copy_insertible_v<Allocator, T>)
		{
			copy_ranges(src.as_moving_ranges(), dest);
		}
		else
		{
			copy_ranges(src, dest);
		}
	}

	template <class Src, class Dest>
	static constexpr void try_move_ranges_backward(Src src, Dest dest) {
		if constexpr (std::is_nothrow_move_constructible_v<T> || !detail::is_copy_insertible_v<Allocator, T>)
		{
			copy_ranges_backward(src.as_moving_ranges(), dest);
		}
		else
		{
			copy_ranges_backward(src, dest);
		}
	}

	template <class Src, class Dest>
	constexpr void try_move_construct_ranges(Src src, Dest dest)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(dest.total() == src.total());
		if(dest.size() == 1) {
			RangeGuard<T> guard_{std::addressof(alloc()), dest.begin()->begin(), dest.begin()->begin()};
			for(auto range: src) {
				auto stop = try_make_move_iterator(range.end());
				for(auto pos = try_make_move_iterator(range.begin()); pos < stop; (void)++pos, ++guard_.stop) {
					TIM_CIRCULAR_BUFFER_ASSERT(guard_.stop < dest.begin()->end());
					construct(guard_.stop, *pos);
				}
			}
			TIM_CIRCULAR_BUFFER_ASSERT(guard_.stop == dest.begin()->end());
			guard_.alloc = nullptr;
		} else {
			RangeGuard<T> guard1_{std::addressof(alloc()), dest.begin()->begin(), dest.begin()->begin()};
			TIM_CIRCULAR_BUFFER_ASSERT(src.size() == 1u);
			auto range = *src.begin();
			auto pos = try_make_move_iterator(range.begin());
			auto src_stop = try_make_move_iterator(range.end());
			for(; guard1_.stop < dest.begin()->end(); (void)++pos, ++guard1_.stop) {
				TIM_CIRCULAR_BUFFER_ASSERT(pos < src_stop);
				construct(guard1_.stop, *pos);
			}
			RangeGuard<T> guard2_{std::addressof(alloc()), dest.begin()[1].begin(), dest.begin()[1].begin()};
			for(; guard2_.stop < dest.begin()[1].end(); (void)++pos, ++guard2_.stop) {
				TIM_CIRCULAR_BUFFER_ASSERT(pos < src_stop);
				construct(guard2_.stop, *pos);
			}
			TIM_CIRCULAR_BUFFER_ASSERT(pos == src_stop);
			guard2_.alloc = nullptr;
			guard1_.alloc = nullptr;
		}
	}

	constexpr void clear_fast() {
		for(auto range: get_ranges()) {
			for(auto& elem: range) {
				destroy(std::addressof(elem));
			}
		}
		size_ = 0;
	}

	constexpr detail::AllocatorRef<Allocator> get_allocator_ref()
	{
		return detail::AllocatorRef<Allocator>(alloc());
	}

	constexpr temporary_buffer_type make_temporary_buffer()
	{
		if constexpr (alloc_traits::is_always_equal::value && std::is_empty_v<Allocator>)
		{
			return temporary_buffer_type();
		}
		else
		{
			return temporary_buffer_type(get_allocator_ref());
		}
	}

	constexpr temporary_buffer_type make_temporary_buffer(size_type cap)
	{
		if constexpr (alloc_traits::is_always_equal::value && std::is_empty_v<Allocator>)
		{
			return temporary_buffer_type(detail::reserve_tag, cap);
		}
		else
		{
			return temporary_buffer_type(detail::reserve_tag, cap, get_allocator_ref());
		}
	}

	constexpr void reserve_slow(size_type new_cap, size_type new_start) {
		auto tmp = make_temporary_buffer(new_cap);
		tmp.start_ = new_start;
		for(auto range: get_ranges()) {
			tmp.append_fast(
				try_make_move_iterator(range.begin()),
				try_make_move_iterator(range.end())
			);
		}
		commit(tmp);
	}


	constexpr void reserve_fast(size_type count) {
		data_ = allocate(count);
		cap_ = count;
	}

	template <class It>
	constexpr void append_fast(It first, It last, size_type size_hint) {
		for(auto range: get_spare_ranges()) {
			for(auto& elem: range) {
				if(size_hint == 0u) {
					return;
				}
				--size_hint;
				TIM_CIRCULAR_BUFFER_ASSERT(first != last);
				construct(std::addressof(elem), *first++);
				++size_;
			}
		}
	}

	template <class It>
	constexpr void append_fast(It first, It last) {
		for(auto range: get_spare_ranges()) {
			for(auto& elem: range) {
				if(first == last) {
					return;
				}
				construct(std::addressof(elem), *first++);
				++size_;
			}
		}
		TIM_CIRCULAR_BUFFER_ASSERT(first == last);
	}
	
	template <class It>
	constexpr It append_no_split(It first, It last) {
#ifndef NDEBUG
		if(storage_is_split())
		{
			TIM_CIRCULAR_BUFFER_ASSERT(end().get_pointer() < begin().get_pointer());
			TIM_CIRCULAR_BUFFER_ASSERT(end().get_pointer() - begin().get_pointer() >= std::distance(first, last));
		}
		else
		{
			TIM_CIRCULAR_BUFFER_ASSERT(begin().get_pointer() <= end().get_pointer());
			TIM_CIRCULAR_BUFFER_ASSERT((data_ + cap_) - end().get_pointer() >= std::distance(first, last));
		}
#endif /* NDEBUG */
		auto dest = end().get_pointer();
		while(first != last)
		{
			construct(dest++, *first++);
			++size_;
		}
		TIM_CIRCULAR_BUFFER_ASSERT(first == last);
		return first;
	}

	template <class It>
	constexpr void assign_empty(It first, It last, size_type size_hint)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size_ == 0);
		if(cap_ < size_hint)
		{
			reallocate_empty(size_hint);
		}
		start_ = 0;
		auto stop = data_ + size_hint;
		for(auto p = data_; p != stop; ++p)
		{
			TIM_CIRCULAR_BUFFER_ASSERT(first != last);
			construct(p, *first++);
			size_++;
		}
		TIM_CIRCULAR_BUFFER_ASSERT(first == last);
	}
	
	constexpr void assign_empty(size_type count, const T& value)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size_ == 0);
		if(cap_ < count)
		{
			reallocate_empty(count);
		}
		start_ = 0;
		auto stop = data_ + count;
		for(auto p = data_; p != stop; ++p)
		{
			construct(p, value);
			size_++;
		}
	}

	constexpr void reallocate_empty(size_type count)
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size_ == 0);
		release_memory_empty();
		initialize_allocation(count);
	}

	constexpr void release_memory_empty()
	{
		TIM_CIRCULAR_BUFFER_ASSERT(size_ == 0);
		if(data_)
		{
			TIM_CIRCULAR_BUFFER_ASSERT(data_);
			deallocate(data_, cap_);
			data_ = nullptr;
			cap_ = 0;
		}
	}


	template <class SrcIt, class DestIt>
	constexpr SrcIt append_and_replace_fast(SrcIt src_first, SrcIt src_last, DestIt dest_first, DestIt dest_last) {
		// TODO: Measure whether this is faster than just doing both operations in batches.

		// in the case of move iterators, we need to cast to an lvalue.
		constexpr auto cast_to_lvalue = [](typename std::iterator_traits<DestIt>::value_type&& rvalue) -> typename std::iterator_traits<DestIt>::value_type&
		{
			return *std::addressof(rvalue);
		};
		for(auto range: get_spare_ranges()) {
			for(auto& elem: range) {
				if(dest_first == dest_last) {
					return src_first;
				}
				construct(std::addressof(elem), *dest_first);
				++size_;
				cast_to_lvalue(*dest_first) = *src_first++;
				++dest_first;
			}
		}
		TIM_CIRCULAR_BUFFER_ASSERT(dest_first == dest_last);
		return src_first;
	}

	template <class It>
	constexpr void prepend_fast(It first, It last) {
		using iter_cat_type = typename std::iterator_traits<It>::iterator_category;
		static_assert(!std::is_same_v<iter_cat_type, std::input_iterator_tag>, "");
		prepend_fast(first, last, size_from_iterator_difference(std::distance(first, last)));
	}

	template <class It>
	constexpr void prepend_fast(It first, It last, size_type size_hint) {
		if(size_hint == 0)
		{
			assert(first == last);
			return;
		}
		auto ranges = get_spare_ranges().last(size_hint);
		TIM_CIRCULAR_BUFFER_ASSERT(ranges.size() >= 1u);
		TIM_CIRCULAR_BUFFER_ASSERT(size_hint <= ranges.total());
		auto front_range = *ranges.begin();
		RangeGuard<T> g1{std::addressof(alloc()), front_range.begin(), front_range.begin()};
		while(g1.stop != front_range.end())
		{
			TIM_CIRCULAR_BUFFER_ASSERT(first != last);
			construct(g1.stop++, *first++);
		}
		if(ranges.size() == 2)
		{
			auto back_range = ranges.begin()[1];
			RangeGuard<T> g2{std::addressof(alloc()), back_range.begin(), back_range.begin()};
			while(g2.stop != back_range.end())
			{
				TIM_CIRCULAR_BUFFER_ASSERT(first != last);
				construct(g2.stop++, *first++);
			}
			g2.alloc = nullptr;
		}
		else
		{
			TIM_CIRCULAR_BUFFER_ASSERT(front_range.size() == size_hint);
		}
		start_ = size_from_iterator_difference(g1.start - data_);
		size_ += size_hint;
		g1.alloc = nullptr;
	}
	
	constexpr void commit(temporary_buffer_type& other) noexcept {
		clear_fast();
		if(data_) {
			deallocate(data_, capacity());
		}
		data_  = std::exchange(other.data_, nullptr);
		cap_   = std::exchange(other.cap_, 0u);
		size_  = std::exchange(other.size_, 0u);
		start_ = std::exchange(other.start_, 0u);
	}

	constexpr void commit_empty(temporary_buffer_type& other) noexcept {
		TIM_CIRCULAR_BUFFER_ASSERT(!data_);
		TIM_CIRCULAR_BUFFER_ASSERT(size_ == 0);
		TIM_CIRCULAR_BUFFER_ASSERT(cap_ == 0);
		TIM_CIRCULAR_BUFFER_ASSERT(start_ == 0);
		data_  = std::exchange(other.data_, nullptr);
		cap_   = std::exchange(other.cap_, 0u);
		size_  = std::exchange(other.size_, 0u);
		start_ = std::exchange(other.start_, 0u);
	}

	template <class ... Args>
	constexpr reference emplace_back_fast(Args&& ... args) {
		auto ptr = end().get_raw_pointer();
		construct(end().get_raw_pointer(), std::forward<Args>(args)...);
		++size_;
		return *ptr;
	}

	template <class ... Args>
	constexpr reference emplace_front_fast(Args&& ... args) {
		auto new_start = start_ == 0 ? cap_ - 1 : start_ - 1;
		auto ptr = data_ + new_start;
		construct(ptr, std::forward<Args>(args)...);
		++size_;
		start_ = new_start;
		return *ptr;
	}

	static constexpr size_type grow_size(size_type n) {
		if(n == 0) {
			return 1u;
		}
		if(n == 1) {
			return 2u;
		}
		if((std::numeric_limits<size_type>::max() / 8) >= n) {
			// Approximate the golden ratio if it won't overflow.
			n *= 8;
			n /= 5;
			return n;
		} else {
			auto old_n = n;
			n /= 5;
			TIM_CIRCULAR_BUFFER_ASSERT(n > 0u);
			n *= 8;
			if(n > old_n)
			{
				return n;
			}
			// overflow
			return std::numeric_limits<size_type>::max();
		}
	}
	
	static constexpr std::optional<size_type> shrink_size(size_type n) {
		if(n == 0) {
			return std::nullopt;
		} else if(n == 1) {
			return 0u;
		} else if((std::numeric_limits<size_type>::max() / 5ul) >= n) {
			// Approximate the golden ratio if it won't overflow.
			n *= 5ul;
			n /= 8ul;
			return n;
		} else {
			n /= 8ul;
			n *= 5ul;
			return n;
		}
	}

	static constexpr SplitRanges<const_pointer> as_split_ranges(const_iterator first, const_iterator last) {
		TIM_CIRCULAR_BUFFER_ASSERT(first <= last);
		if(first.get_has_wrapped() || !last.get_has_wrapped()) {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>(first.get_pointer(), last.tagged_index_ - first.tagged_index_)
			);
		}
		return SplitRanges<const_pointer>(
			SplitRange<const_pointer>(first.get_pointer(), first.cap_ - first.get_index()),
			SplitRange<const_pointer>(last.data_, last.start_)
		);
	}

	static constexpr SplitRanges<pointer> as_split_ranges(iterator first, iterator last) {
		TIM_CIRCULAR_BUFFER_ASSERT(first <= last);
		if(first.get_has_wrapped() || !last.get_has_wrapped()) {
			return SplitRanges<pointer>(
				SplitRange<pointer>(first.get_pointer(), last.tagged_index_ - first.tagged_index_)
			);
		}
		return SplitRanges<pointer>(
			SplitRange<pointer>(first.get_pointer(), first.cap_ - first.get_index()),
			SplitRange<pointer>(last.data_, last.get_index())
		);
	}


	static constexpr SplitRanges<std::move_iterator<pointer>> as_moving_split_ranges(iterator first, iterator last) {
		TIM_CIRCULAR_BUFFER_ASSERT(first <= last);
		if(first.get_has_wrapped() || !last.get_has_wrapped()) {
			return SplitRanges<std::move_iterator<pointer>>(
				SplitRange<std::move_iterator<pointer>>(std::make_move_iterator(first.get_pointer()), last.tagged_index_ - first.tagged_index_)
			);
		}
		return SplitRanges<std::move_iterator<pointer>>(
			SplitRange<std::move_iterator<pointer>>(std::make_move_iterator(first.get_pointer()), first.cap_ - first.get_index()),
			SplitRange<std::move_iterator<pointer>>(std::make_move_iterator(last.data_), last.get_index())
		);
	}



	constexpr SplitRanges<const_pointer> get_spare_ranges() const {
		auto start = begin();
		auto stop = end();
		if(stop.get_has_wrapped()) {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>(stop.get_pointer(), start_.tagged_index_ - stop.get_index())
			);
			
		} else if(start.tagged_index_ == 0u) {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>(stop.get_pointer(), cap_ - stop.get_index())
			);
		} else {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>(stop.get_pointer(), cap_ - stop.get_index()),
				SplitRange<const_pointer>(data_, start.tagged_index_)
			);
		}
	}

	constexpr SplitRanges<pointer> get_spare_ranges() {
		auto start = begin();
		auto stop = end();
		if(stop.get_has_wrapped()) {
			return SplitRanges<pointer>(
				SplitRange<pointer>(stop.get_pointer(), start.tagged_index_ - stop.get_index())
			);
			
		} else if(start.tagged_index_ == 0u) {
			return SplitRanges<pointer>(
				SplitRange<pointer>(stop.get_pointer(), cap_ - stop.get_index())
			);
		} else {
			return SplitRanges<pointer>(
				SplitRange<pointer>(stop.get_pointer(), cap_ - stop.get_index()),
				SplitRange<pointer>(data_, start.tagged_index_)
			);
		}
	}

	constexpr SplitRanges<pointer> get_ranges() {
		auto start = begin();
		auto stop = end();
		if(!stop.get_has_wrapped()) {
			return SplitRanges<pointer>(
				SplitRange<pointer>(start.get_pointer(), stop.tagged_index_ - start.tagged_index_)
			);
		} else if(stop.get_index() == 0u) {
			return SplitRanges<pointer>(
				SplitRange<pointer>(start.get_pointer(), cap_ - start.tagged_index_)
			);
		} else {
			return SplitRanges<pointer>(
				SplitRange<pointer>(start.get_pointer(), cap_ - start.tagged_index_),
				SplitRange<pointer>(data_, stop.get_index())
			);
		}
	}


	constexpr SplitRanges<const_pointer> get_ranges() const {
		auto start = begin();
		auto stop = end();
		if(!stop.get_has_wrapped()) {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>(start.get_pointer(), stop.tagged_index_ - start.tagged_index_)
			);
		} else if(stop.get_index() == 0u) {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>(start.get_pointer(), cap_ - start.tagged_index_)
			);
		} else {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>(start.get_pointer(), cap_ - start.tagged_index_),
				SplitRange<const_pointer>(data_, stop.get_index())
			);
		}
	}

	template <class It>
	static constexpr auto try_make_move_iterator(It it)
		-> std::conditional_t<
			std::is_nothrow_move_constructible_v<typename std::iterator_traits<It>::value_type>
			|| !detail::is_copy_insertible_v<Allocator, typename std::iterator_traits<It>::value_type>,
			std::move_iterator<It>,
			It
		>
	{
		static_assert(std::is_same_v<typename std::iterator_traits<It>::value_type, T>);
		if constexpr(std::is_nothrow_move_constructible_v<T> || !detail::is_copy_insertible_v<Allocator, T>) {
			return std::make_move_iterator(it);
		} else {
			return it;
		}
	}

	static constexpr size_type compute_offset(size_type start, size_type count, size_type cap) {
		TIM_CIRCULAR_BUFFER_ASSERT(cap > start);
		TIM_CIRCULAR_BUFFER_ASSERT(cap >= count);
		if(cap - start > count) {
			return start + count;
		}
		return count - (cap - start);
	}

	constexpr bool storage_is_split() const {
		return (capacity() - begin_index()) < size();
	}

	static iterator as_non_const_iterator(const_iterator it)
	{
		iterator result;
		result.data_ = std::pointer_traits<pointer>::pointer_to(const_cast<reference>(*it.data_));
		result.cap_ = it.cap_;
		result.tagged_index_ = it.tagged_index_;
		return result;
	}

	template <class U, class OtherAlloc>
	friend struct CircularBuffer;

	pointer data_ = nullptr;
	size_type cap_ = 0;
	size_type size_ = 0;
	size_type start_ = 0;
};

template <class InputIt, class Alloc = std::allocator<typename std::iterator_traits<InputIt>::value_type>>
CircularBuffer(InputIt, InputIt, Alloc = Alloc()) -> CircularBuffer<typename std::iterator_traits<InputIt>::value_type, Alloc>;

/**
 * @brief
 * Erases all elements in 'b' for which 'pred' returns true, as if by calling 'b.erase_if(pred)'.
 * 
 * @param b    The container whose contents are to be erased.
 * @param pred Predicate function object which returns 'true' for elements that should be erased.
 * 
 * @note
 * After calling this function, the values of b.begin_index() and b.end_index() are unspecified.
 * @note
 * The expression pred(v) must be convertible to bool for every argument v of type (possibly const) T,
 * regardless of value category, and must not modify v.
 * Thus, a parameter type of T& is not allowed, nor is T unless for T a move is equivalent to a copy.
 */
template <class T, class A, class Pred>
constexpr auto erase_if(CircularBuffer<T, A>& b, Pred pred) -> typename CircularBuffer<T, A>::size_type
{
	return b.erase_if(pred);
}
/**
 * @brief
 * Erases all elements in 'b' which compare equal to 'value'.
 * 
 * @param b     The container whose contents are to be erased.
 * @param value The value to compare elements of 'b' to.
 * 
 * @note
 * After calling this function, the values of b.begin_index() and b.end_index() are unspecified.
 */
template <class T, class A, class U>
constexpr auto erase(CircularBuffer<T, A>& b, const U& value) -> typename CircularBuffer<T, A>::size_type
{
	return erase_if(b, [&value](const T& item) -> bool { return value == item; });
}

} /* inline namespace circular_buffer */

} /* namespace tim */


#endif /* TIM_CIRCULAR_BUFFER_HPP */
