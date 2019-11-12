#ifndef TIM_CIRCULAR_BUFFER_HPP
#define TIM_CIRCULAR_BUFFER_HPP

#include <utility>
#include <memory>
#include <algorithm>
#include <cassert>
#include <cstdint>

#if defined(TIM_CICULAR_BUFFER_NO_USE_INTRINSICS) && defined(_MSC_VER)
#include <immintrin.h>
#endif




namespace tim {

inline namespace circular_buffer {

template <class T, class Allocator = std::allocator<T>>
struct CircularBuffer;

namespace detail {

template <class It>
struct SimpleRange {
	
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
		return begin_;
	}

	constexpr std::reverse_iterator<It> rbegin() const {
		return std::make_reverse_iterator(end());
	}

	constexpr std::reverse_iterator<It> rend() const {
		return std::make_reverse_iterator(begin());
	}

	friend constexpr It begin(const SimpleRange& range) const {
		return range.begin();
	}

	friend constexpr It end(const SimpleRange& range) const {
		return range.end();
	}

	friend constexpr std::reverse_iterator<It> rbegin(const SimpleRange& range) const {
		return std::make_reverse_iterator(range.end());
	}

	friend constexpr std::reverse_iterator<It> rend(const SimpleRange& range) const {
		return std::make_reverse_iterator(range.begin());
	}

private:
	It begin_;
	It end_;
};

template <class C>
constexpr SimpleRange<typename C::const_reverse_iterator> reversed(const C& container) {
	using std::rbegin;
	using std::rend;
	return SimpleRange<typename C::const_reverse_iterator>(rbegin(container), rend(container));
}

template <class C>
constexpr SimpleRange<typename C::reverse_iterator> reversed(C& container) {
	using std::rbegin;
	using std::rend;
	return SimpleRange<typename C::reverse_iterator>(rbegin(container), rend(container));
}

template <class T, class ... U>
struct is_one_of: std::bool_constant<(std::is_same_v<T, U> || ...)> {}

template <class T, class ... U>
inline constexpr bool is_one_of_v = is_one_of<T, U...>::value;


template <class A, class T, bool = std::is_trivially_destructible_v<T>>
struct is_trivially_allocator_destructible;

template <class A, class T, bool = std::is_trivially_destructible_v<T>>
struct is_trivially_allocator_destructible<A, T, false>: std::false_type {};

template <class A, class T, bool = std::is_trivially_destructible_v<T>>
struct is_trivially_allocator_destructible<A, T, true>: std::false_type {
private:
	template <class U, class = decltype(std::declval<A&>().destroy(std::declval<T*>()))>
	static constexpr auto helper(int)
		-> std::bool_constant<std::is_same_v<A, std::allocator<T>>>
	{
		return {};
	}

	static constexpr std::true_type helper(...) {
		return {};
	}

public:
	static constexpr const bool value = helper(0).value;
};



template <
	class A,
	class T,
	class = decltype(std::allocator_traits<A>::construct(std::declval<A&>(), std::declval<T*>(), std::declval<const T&>()))
>
constexpr std::true_type test_is_copy_insertable(int) { return std::true_type{}; }

template <class A, class T>
constexpr std::true_type test_is_copy_insertable(...) { return std::false_type{}; }

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
constexpr std::true_type test_is_move_insertable(...) { return std::false_type{}; }

template <class A, class T>
struct is_move_insertible: decltype(test_is_move_insertable<A, T>()) {};

template <class A, class T>
inline constexpr bool is_move_insertible_v = is_move_insertible<A, T>::value;

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

struct NonInt{};

template <class Int, bool = std::is_integral_v<Int>>
struct next_widest_int;

template <class Int>
struct next_widest_int<Int, false> {};

template <class Int>
struct next_widest_int<Int, true> {
private:
	using signed_types = std::tuple<char, short, int, long, long long, std::intmax_t>;
	using unsigned_types = std::tuple<
		unsigned char,
		unsigned short,
		unsigned int,
		unsigned long,
		unsigned long long,
		std::uintmax_t
	>;
	template <class T>
	struct Tag {};
	
	template <class ... Types>
	static constexpr std::ptrdiff_t index(Tag<std::tuple<Types...>>) {
		constexpr std::array<std::size_t, sizeof...(Types)> sizes{sizeof(Types)...};
		for(std::size_t i = 0; i < sizes.size(); ++i) {
			if(sizes[i] > sizeof(Int)) {
				return i;
			} 
		}
		return sizes.size();
	}
	static constexpr std::size_t signed_index = index(Tag<signed_types>{});
	static constexpr std::size_t unsigned_index = index(Tag<unsigned_types>{});

public:
	using type = std::conditional_t<
		std::is_signed_v<Int>,
		std::conditional_t<
			(signed_index < std::tuple_size_v<signed_types>),
			std::tuple_element_t<signed_types, signed_index>,
			NonInt
		>,
		std::conditional_t<
			(unsigned_index < std::tuple_size_v<unsigned_types>),
			std::tuple_element_t<unsigned_types, unsigned_index>,
			NonInt
		>
	>;
};

template <class Int>
using next_widest_int_t = typename next_widest_int_t<Int>::type;

template <class Int, bool = std::is_integral_v<Int>>
struct next_shortest_int;

template <class Int>
struct next_shortest_int<Int, false> {};

template <class Int>
struct next_shortest_int<Int, true> {
private:
	using signed_types = std::tuple<char, short, int, long, long long, std::intmax_t>;
	using unsigned_types = std::tuple<
		unsigned char,
		unsigned short,
		unsigned int,
		unsigned long,
		unsigned long long,
		std::uintmax_t
	>;
	template <class T>
	struct Tag {};
	
	template <class ... Types>
	static constexpr std::ptrdiff_t index(Tag<std::tuple<Types...>>) {
		constexpr std::array<std::size_t, sizeof...(Types)> sizes{sizeof(Types)...};
		for(std::size_t i = 0; i < sizes.size(); ++i) {
			if(sizes[i] >= sizeof(Int)) {
				if(i > 0u) {
					return i - 1u;
				}
			} 
		}
		return sizes.size();
	}
	static constexpr std::size_t signed_index = index(Tag<signed_types>{});
	static constexpr std::size_t unsigned_index = index(Tag<unsigned_types>{});
public:
	using type = std::conditional_t<
		std::is_signed_v<Int>,
		std::conditional_t<
			(signed_index < std::tuple_size_v<signed_types>),
			std::tuple_element_t<signed_types, signed_index>,
			NonInt
		>,
		std::conditional_t<
			(unsigned_index < std::tuple_size_v<unsigned_types>),
			std::tuple_element_t<unsigned_types, unsigned_index>,
			NonInt
		>
	>;
};

template <class Int>
using next_shortest_int_t = typename next_shortest_int_t<Int>::type;

template <class T>
inline constexpr std::size_t bit_count = static_cast<std::size_t>(CHAR_BIT) * sizeof(T);

template <class UInt>
struct FibPair {
	UInt low;
	UInt high;
};

template <class UInt>
constexpr FibPair<UInt> get_largest_representable_fib_pair() {
	UInt low = 0;
	UInt high = 1;
	UInt next = low + high;
	while(next > high) {
		low = high;
		high = next;
		next = low + high;
	}
	return FibPair<UInt>{low, high};
}

template <class UInt>
constexpr std::pair<UInt, bool> safe_add(UInt lhs, UInt rhs) {
	UInt result = lhs + rhs;
	return {result, result < lhs};
}

template <class UInt>
constexpr std::pair<UInt, bool> safe_add(UInt lhs, UInt rhs) {
	UInt result = lhs + rhs;
	return {result, result < lhs};
}
	
template <class UInt>
constexpr std::array<UInt, 2> safe_multiply(UInt lhs, UInt rhs) {
	constexpr UInt shift = (CHAR_BIT * sizeof(UInt)) - 1;
	UInt lhs_lo = lhs & ((1ull << shift) - 1);
	UInt lhs_hi = lhs >> shift;
	UInt rhs_lo = rhs & ((1ull << shift) - 1);
	UInt rhs_hi = rhs >> shift;

	UInt lo = lhs_lo * rhs_lo;
	UInt hi = lhs_hi * rhs_hi;

	UInt mid1 = lhs_lo * rhs_hi;
	UInt mid2 = lhs_hi * rhs_lo;

	auto [mid, mid_overflowed] = safe_add(mid1, mid2);
	if(mid_overflowed) {
		auto [new_hi, overflowed] = safe_add(hi, UInt(1) << shift);
		assert(!overflowed && "Overflow shouldn't be possible here.");
		hi = new_hi;
	}
	{
		auto [new_hi, hi_overflowed] = safe_add(hi, (mid >> shift));
		assert(!hi_overflowed && "Overflow shouldn't be possible here.");
		hi = new_hi;
	}
	{
		auto [new_lo, lo_overflowed] = safe_add(lo, (mid & ((UInt(1) << shift) - 1)));
		if(lo_overflowed) {
			++hi;
			assert(hi > 0u && "Overflow shouldn't be possible here.");
		}
		lo = new_lo;
	}
	return std::array<UInt, 2>{lo, hi};
}

template <class UInt>
constexpr std::array<UInt, 2> array_lbitshift(std::array<UInt, 2> value, std::size_t shift) {
	constexpr std::size_t uint_bits = (CHAR_BIT * sizeof(UInt));
	if(shift >= uint_bits) {
		value[1] = value[0];
		value[0] = 0u;
		shift -= uint_bits;
		assert(shift < uint_bits);
	}
	value[1] <<= shift;
	value[1] |= value[0] >> (uint_bits - shift);
	value[0] <<= shift;
	return value;
}

template <class UInt>
constexpr std::array<UInt, 2> array_rbitshift(std::array<UInt, 2> value, std::size_t shift) {
	constexpr std::size_t uint_bits = bit_count<UInt>;
	if(shift >= uint_bits) {
		value[0] = value[1];
		value[1] = 0u;
		shift -= uint_bits;
		assert(shift < uint_bits);
	}
	value[0] >>= shift;
	value[0] |= value[1] << (uint_bits - shift);
	value[1] >>= shift;
	return value;
}

template <class UInt>
constexpr std::size_t find_last_set(UInt value) {
	constexpr std::size_t uint_bits = bit_count<UInt>;
	if(!value)
	{
		return uint_bits;
	}
	std::size_t shift_lo = 0u;
	std::size_t shift_hi = bit_count<UInt> - 1u;
	while(shift_hi > shift_lo) {
		std::size_t shift = shift_lo + (shift_hi - shift_lo) / 2;
		UInt v = UInt(1) << shift;
		if(v > value) {
			shift_hi = shift;
		} else {
			shift_lo = shift;
		}
	}
	assert(((UInt(1u) << shift_lo) & value) != 0u);
	assert((UInt(1u) << shift_hi) > value);
	return shift_lo;
}

template <class UInt, class HalfWord>
constexpr UInt safe_divide_helper(UInt quot, UInt rem, HalfWord numer, UInt denom) {
	constexpr std::size_t half_bits = bit_count<UInt> / 2u;
	constexpr UInt low_bits_mask = (1 << half_bits) - 1;
	constexpr auto low_bits = [](UInt v) -> half_word {
		return static_cast<half_word>(low_bits_mask & v);
	};
	constexpr auto high_bits = [](UInt v) -> half_word {
		return static_cast<half_word>((low_bits_mask << half_bits) & v);
	};
	UInt quot = 0;
	UInt rem = 0;

	half_word denom_lo = denom & ((UInt(1) << half_bits) - 1);
	while(
		(high_bits(quot) != 0u)
		|| ((static_cast<UInt>(low_bits(quot)) * denom_lo) > ((rem << half_bits) | numer))
	) {
		--quot;
		rem += denom >> half_bits;
		if(high_bits(rem) != 0u) {
			break;
		}
	}
	return quot;
}

template <class UInt>
constexpr std::optional<UInt> safe_divide(std::array<UInt, 2> numerator, UInt denominator) {
	switch(denominator)
	{
	case 0u:
		return std::nullopt;
	case 2u:
		numerator = array_rbitshift(numerator, 1);
		[[fallthrough]]
	case 1u:
		if(numerator[1] != 0u) {
			return std::nullopt;
		} else {
			return numerator[0];
		}
	default:
		break;
	}
	if(numerator[1] == 0u) {
		return numerator[0] / denominator;
	}
	if(numerator[1] >= denominator) {
		return std::nullopt;
	}
	// std::size_t numer_shift = bit_count<UInt> - (find_last_set(numerator[1]) + 1u);
	const std::size_t denom_shift = bit_count<UInt> - (find_last_set(denominator) + 1u);
	numerator = array_lbitshift(numerator, denom_shift);
	denominator <<= denom_shift;
	using half_word = std::conditional_t<
		std::is_integral_v<next_shortest_int_t<UInt>> && (sizeof(next_shortest_int_t<UInt>) * 2ul >= sizeof(UInt)),
		UInt,
		next_shortest_int_t<UInt>
	>;
	constexpr std::size_t half_bits = bit_count<UInt> / 2u;
	constexpr UInt low_bits_mask = (1 << half_bits) - 1;
	constexpr auto low_bits = [](UInt v) -> half_word {
		return static_cast<half_word>(low_bits_mask & v);
	};
	constexpr  auto high_bits = [](UInt v) -> half_word {
		return static_cast<half_word>((low_bits_mask << half_bits) & v);
	};
	const half_word denom_hi = denominator >> half_bits;
	const half_word numer0_lo = low_bits(numerator[0]);
	const half_word numer0_hi = high_bits(numerator[0]) >> half_bits;

	const half_digit quot_tmp = low_bits(safe_divide_helper(numerator[1] / denom_hi, numerator[1] % denom_hi, numer0_hi, denominator));

	const UInt numer_mid = ((numerator[1] << half_bits) | (numerator[0] >> half_bits)) - quot_hi * denominator;
	const half_digit quot_lo = low_bits(safe_divide_helper(numer_mid / denom_hi, numer_mid % denom_hi, numer0_lo, denominator));
	const half_digit quot_lo = low_bits(quot);
	return static_cast<UInt>(quot_hi) << half_bits) | quot_lo
}
 
template <class UInt>
constexpr std::optional<UInt> safe_scale_by_ratio(UInt numer, UInt denom, UInt value) {
	return safe_divide(safe_multiply(numer, value), denom);
}

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
	using alloc_traits = std::allocator_traits<A>;
public:
	using pointer            = typename alloc_traits::pointer;
	using const_pointer      = typename alloc_traits::const_pointer;
	using void_pointer       = typename alloc_traits::void_pointer;
	using const_void_pointer = typename alloc_traits::const_void_pointer;
	using value_type         = typename alloc_traits::value_type;
	using size_type          = typename alloc_traits::size_type;
	using difference_type    = typename alloc_traits::difference_type;

	using is_always_equal                        = alloc_traits::is_always_equal;
	using propagate_on_container_swap            = alloc_traits::propagate_on_container_swap;
	using propagate_on_container_move_assignment = !is_always_equal;
	using propagate_on_container_copy_assignment = !is_always_equal;
	

	template <class U>
	struct rebind {
		using other = typename alloc_traits<U>::template rebind_alloc<U>::other;
	};

	AllocatorRef() = delete;
	AllocatorRef(const AllocatorRef&) = default;
	AllocatorRef(AllocatorRef&&) = default;
	AllocatorRef& operator=(const AllocatorRef&) = default;
	AllocatorRef& operator=(AllocatorRef&&) = default;

	constexpr AllocatorRef(A& a):
		ref_(a)
	{
		
	}

	explicit constexpr operator A&() const {
		return *ref_;
	}

	constexpr AllocatorRef select_on_container_copy_construction() const {
		return *this;
	}

	[[nodiscard]]
	constexpr pointer allocate(size_type sz) const {
		return alloc_traits::allocate(*ref_, ptr);
	}

	constexpr void deallocate(pointer p, size_type sz) const {
		return alloc_traits::deallocate(*ref_, p, sz);
	}

	template <
		class T,
		class ... Args,
		class = decltype(alloc_traits::construct(std::declval<A&>(), std::declval<T*>(), std::declval<Args&&>()...))
	>
	constexpr void construct(T* ptr, Args&& ... args) const {
		return alloc_traits::construct(ptr, std::forward<Args>(args)...);
	}

	template <class P, class = decltype(alloc_traits::destroy(std::declval<A&>(), std::declval<P>()))>
	constexpr auto destroy(P ptr) const
		-> decltype(alloc_traits::destroy(std::declval<A&>(), std::declval<P>()))
	{
		return alloc_traits::destroy(*ref_, ptr);
	}

	constexpr size_type max_size() const {
		return alloc_traits::max_size(*ref_);
	}

	friend constexpr operator==(AllocatorRef lhs, AllocatorRef rhs) noexcept {
		return is_always_equal::value || (lhs.ref_ == rhs.ref_) || (*lhs.ref_ == *rhs.ref_);
	}

	friend constexpr operator!=(AllocatorRef lhs, AllocatorRef rhs) noexcept {
		return !(lhs == rhs);
	}

private:
	A* ref_;
}; 

} /* namespace detail */

template <bool IsConst, class T, class Allocator>
struct CircularBufferIterator {
private:
	using reference_type = std::conditional_t<
		IsConst,
		const CircularVector&,
		CircularVector&
	>;
	using size_type = std::size_t;
public:
	using difference_type   = std::ptrdiff_t;
	using iterator_category = std::random_access_iterator_tag;
	using reference         = std::conditional_t<IsConst, const T&, T&>;
	using pointer           = std::conditional_t<
		IsConst, 
		typename std::allocator_traits<Allocator>::const_pointer,
		typename std::allocator_traits<Allocator>::pointer
	>;
	using value_type        = T;


private:
	constexpr CircularBufferIterator(pointer data, size_type cap, size_type index, bool is_wrapped):
		data_(data),
		cap_(cap),
		tagged_index_(index)
	{
		if(is_wrapped) {
			set_has_wrapped(true);
		}
	}

public:
	CircularBufferIterator() = default;
	CircularBufferIterator(const CircularBufferIterator&) = default;
	CircularBufferIterator(CircularBufferIterator&&) = default;

	CircularBufferIterator& operator=(const CircularBufferIterator&) = default;
	CircularBufferIterator& operator=(CircularBufferIterator&&) = default;

	template <bool OtherIsConst, std::enable_if_t<(IsConst && !OtherIsConst), bool> = false>
	constexpr CircularBufferIterator(const CircularBufferIterator<OtherIsconst, T, Allocator>& other):
		data_(other.data_),
		cap_(other.cap_),
		tagged_index_(other.tagged_index_)
	{
		
	}

	constexpr CircularBufferIterator& operator++() {
		assert(data_);
		if(cap_ - get_index() > 1) {
			++tagged_index_;
		} else {
			set_index(0u);
			set_has_wrapped(true);
		}
		return *this;
	}

	constexpr CircularBufferIterator operator++(int) {
		auto cpy = *this;
		++*this;
		return cpy;
	}

	constexpr CircularBufferIterator& operator--() {
		assert(data_);
		if(get_index() == 0u) {
			assert(get_has_wrapped());
			tagged_index_ = cap_ - 1;
			assert(!get_has_wrapped());
		} else {
			--tagged_index_;
		}
		return *this;
	}

	constexpr CircularBufferIterator operator--(int) {
		auto cpy = *This;
		--(*this);
		return cpy;
	}

	constexpr reference operator*() const {
		return data_[get_index()];
	}

	constexpr pointer operator->() const {
		return get_pointer();
	}

	constexpr CircularBufferIterator& operator+=(difference_type rhs) {
		if(rhs >= 0) {
			this->add(rhs);
		} else {
			this->subtract(static_cast<size_type>(-(rhs + 1) + 1));
		}
	}

	constexpr CircularBufferIterator& operator-=(difference_type rhs) {
		if(rhs <= 0) {
			this->add(static_cast<size_type>(-(rhs + 1) + 1));
		} else {
			this->subtract(rhs);
		}
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
		assert(lhs.data_ == lhs.data_);
		assert(lhs.cap_ == lhs.cap_);
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
		return pos_ - other.pos_;
	}

	constexpr reference operator[](difference_type ofs) const {
		return *(*this + ofs);
	}

	friend constexpr bool operator==(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		if(lhs.data_ == rhs.data_) {
			assert(lhs.cap_ == rhs.cap_);
			return lhs.tagged_index_ == rhs.tagged_index_;
		} else {
			return false;
		}
	}
	
	friend constexpr bool operator!=(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		return not (lhs == rhs);
	}

	friend constexpr bool operator<(CircularBufferIterator lhs, CircularBufferIterator rhs) {
		assert(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ < rhs.tagged_index_;
	}

	friend constexpr bool operator<=(CircularBufferIterator other) {
		assert(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ <= rhs.tagged_index_;
	}
	
	friend constexpr bool operator>(CircularBufferIterator other) {
		assert(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ > rhs.tagged_index_;
	}
	
	friend constexpr bool operator>=(CircularBufferIterator other) {
		assert(lhs.data_ == rhs.data_);
		return lhs.tagged_index_ >= rhs.tagged_index_;
	}
	
private:

	constexpr pointer get_pointer() const {
		return data_ + get_index();
	}

	constexpr pointer get_raw_pointer() const {
		return std::addressof(**this);
	}

	constexpr void add(size_type count) {
		assert(count <= cap_);
		if(get_has_wrapped()) {
			assert(count <= (cap_ - get_index()));
			tagged_index_ += count;
		} else {
			size_type rem = cap_ - tagged_index_;
			if(rem > count) {
				tagged_index_ += count;
			} else {
				tagged_index_ = count - rem;
			}
		}
	}
	
	constexpr void subtract(size_type count) {
		assert(count <= cap_);
		if(get_has_wrapped()) {
			if(tagged_index_ >= count) {
				tagged_index_ -= count;
			} else {
				tagged_index_ = cap_ - (count - (tagged_index_ + 1));
			}
		} else {
			assert(count <= get_index());
			tagged_index_ -= count;
		}
	}

	constexpr bool get_has_wrapped() const {
		return (tagged_index_ & ~(~size_type(0) >> 1)) != 0u;
	}

	constexpr void set_has_wrapped(bool value) const {
		if(value) {
			tagged_index_ |= ~(~size_type(0) >> 1);
		} else {
			tagged_index_ &= (~size_type(0) >> 1);
		}
	}

	constexpr size_type get_index() const {
		return tagged_index_ & (~size_type(0) >> 1);
	}

	constexpr size_type set_index(size_type new_value) const {
		assert((new_value & ~(~size_type(0) >> 1)) == 0u);
		tagged_index_ = new_value_ | (tagged_index_ & ~(~size_type(0) >> 1));
	}

	template <class T, class Allocator>
	friend class CircularBuffer;
	pointer data_ = nullptr;
	size_type cap_ = 0;
	size_type tagged_index_ = 0;
};


template <class T, class Allocator>
struct CircularBuffer: private AllocatorBase<Allocator> {
	using size_type = std::size_t;
private:
	using alloc_traits = std::allocator_traits<Allocator>;
	using base_type = AllocatorBase<Allocator>;
	using base_type::alloc;

	using ptr_traits = std::pointer_traits<typename alloc_traits::pointer>;
public:
	using value_type             = T;
	using allocator_type         = Allocator;
	using size_type              = typename alloc_traits::size_type;
	using difference_type        = typename alloc_traits::difference_type;
	using reference	             = value_type&;
	using const_reference        = const value_type&;
	using pointer                = typename alloc_traits::pointer;
	using const_pointer          = typename alloc_traits::const_pointer;
	using iterator               = CircularBufferIterator<false, T, Allocator>;
	using const_iterator         = CircularBufferIterator<true, T, Allocator>;
	using reverse_iterator       = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>

private:
	struct TrivialRangeGuard {
		TrivialRangeGuard() = default;
		TrivialRangeGuard(const TrivialRangeGuard&) = delete;
		TrivialRangeGuard(TrivialRangeGuard&&) = delete;
		TrivialRangeGuard& operator=(const TrivialRangeGuard&) = delete;
		TrivialRangeGuard& operator=(TrivialRangeGuard&&) = delete;

		Allocator* alloc = nullptr;
		pointer start = nullptr;
		pointer stop = nullptr;
	};

	struct NonTrivialRangeGuard {
		NonTrivialRangeGuard() = default;
		NonTrivialRangeGuard(const NonTrivialRangeGuard&) = delete;
		NonTrivialRangeGuard(NonTrivialRangeGuard&&) = delete;
		NonTrivialRangeGuard& operator=(const NonTrivialRangeGuard&) = delete;
		NonTrivialRangeGuard& operator=(NonTrivialRangeGuard&&) = delete;

		~NonTrivialRangeGuard() {
			if(!alloc) {
				return;
			}
			for(pointer p = start; p < stop; ++p) {
				alloc_traits::destroy(*alloc, p);
			}
		}

		Allocator* alloc = nullptr;
		pointer start = nullptr;
		pointer stop = nullptr;
	};

	using RangeGuard = std::conditional_t<
		detail::is_trivially_allocator_destructible<Allocator, T>::value,
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

		constexpr SplitRange(Ptr p, size_type sz, bool tag = false):
			begin_(p),
			tagged_size_(sz)
		{
			assert(!get_tag());
			set_tag(tag);
		}
		
		constexpr bool get_tag() const {
			constexpr size_type mask = ~((~size_type(0)) >> 1);
			return (mask & tagged_size_) != 0ul;
		}

		constexpr void set_tag(bool value) {
			constexpr size_type mask = (~size_type(0)) >> 1;
			if(value) {
				tagged_size_ |= ~mask;
			} else {
				tagged_size_ &= mask;
			}
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
			constexpr size_type mask = ~((~size_type(0)) >> 1);
			assert((tagged_size_ & mask) == 0u);
			return tagged_size_;
		}

	private:
		Ptr begin_;
		size_type tagged_size_;
	};

	template <class Ptr>
	struct SplitRanges {

		constexpr SplitRanges():
			ranges_{
				SplitRange<Ptr>{nullptr, 0u, false},
				SplitRange<Ptr>{nullptr, 0u, true}
			}
		{
			
		}

		constexpr SplitRanges(SplitRange<Ptr> r):
			ranges_{r, SplitRange<Ptr>{nullptr, 0u, true}}
		{
			
		}

		constexpr SplitRanges(SplitRange<Ptr> r1, SplitRange<Ptr> r2):
			ranges_{r1, r2}
		{
			
		}

		constexpr SplitRanges(std::pair<Ptr, Ptr> first_range, std::pair<Ptr, Ptr> second_range):
			ranges_{
				SplitRange<Ptr>{first_range.first, first_range.second},
				SplitRange<Ptr>{second_range.first, second_range.second}
			}
		{
			ranges_[1].set_tag(false);
		}

		constexpr const SplitRange<Ptr>* begin() const {
			return &ranges_[0];
		}

		constexpr SplitRange<Ptr>* begin() {
			return &ranges_[0];
		}

		constexpr const SplitRange<Ptr>* end() const {
			return ranges_[1].get_tag() ? &ranges_[1] : &ranges_[1] + 1u;
		}

		constexpr SplitRange<Ptr>* end() {
			return ranges_[1].get_tag() ? &ranges_[1] : &ranges_[1] + 1u;
		}

		constexpr std::reverse_iterator<SplitRange<Ptr>*> rbegin() const {
			return std::make_reverse_iterator(end());
		}

		constexpr std::reverse_iterator<SplitRange<Ptr>*> rend() const {
			return std::make_reverse_iterator(begin());
		}

		constexpr size_type size() const {
			return ranges_[1].get_tag() ? 1u : 2u;
		}

		constexpr size_type total() const {
			return ranges_[0].size() + (ranges_[1].get_tag() ? ranges_[1].size() : 0ul);
		}

	private:
		std::array<SplitRange<Ptr>, 2> ranges_;
	};


	struct CircularBufferDeleter {
		using pointer = typename CircularBuffer::pointer;

		constexpr AllocatorDeleter(CircularBuffer* buff, size_type n):
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

	constexpr void destroy(T* p) {
		alloc_traits::destroy(this->alloc(), p);
	}

	constexpr size_type max_size() const {
		return alloc_traits::max_size(this->alloc());
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

	static constexpr bool is_always_equal = alloc_traits::is_always_equal;

public:


	constexpr iterator begin() {
		return iterator(data_, cap_, start_, false);
	}

	constexpr const_iterator begin() const {
		return const_iterator(data_, cap_, start_, false);
	}

	constexpr const_iterator cbegin() const {
		return const_iterator(data_, cap_, start_, false);
	}


	constexpr iterator end() {
		if(cap_ - start_ > size_) {
			return iterator(data_, cap_, start_ + size_, false);
		} else {
			return iterator(data_, cap_, (size_ - (cap_ - start_)), true);
		}
	}

	constexpr const_iterator end() const {
		if(cap_ - start_ > size_) {
			return const_iterator(data_, cap_, start_ + size_, false);
		} else {
			return const_iterator(data_, cap_, (size_ - (cap_ - start_)), true);
		}
	}

	constexpr const_iterator cend() const {
		if(cap_ - start_ > size_) {
			return const_iterator(data_, cap_, start_ + size_, false);
		} else {
			return const_iterator(data_, cap_, (size_ - (cap_ - start_)), true);
		}
	}


	constexpr reverse_iterator rbegin() {
		return std::make_reverse_iterator(end());
	}

	constexpr const_reverse_iterator rbegin() const {
		return std::make_reverse_iterator(end());
	}

	constexpr const_reverse_iterator crbegin() const {
		return std::make_reverse_iterator(end());
	}


	constexpr reverse_iterator rend() {
		return std::make_reverse_iterator(begin());
	}

	constexpr const_reverse_iterator rend() const {
		return std::make_reverse_iterator(begin());
	}

	constexpr const_reverse_iterator crbegin() const {
		return std::make_reverse_iterator(begin());
	}


	constexpr reference operator[](size_type index) {
		return cbegin()[index];
	}

	constexpr const_reference operator[](size_type index) const {
		return begin()[index];
	}


	constexpr reference at(size_type index) {
		if(index > size()) {
			throw std::out_of_range("tim::CircularBuffer::at()");
		}
		return begin()[index];
	}

	constexpr const_reference operator[](size_type index) const {
		if(index > size()) {
			throw std::out_of_range("tim::CircularBuffer::at()");
		}
		return begin()[index];
	}

	constexpr const_reference front() const { return *begin(); }
	constexpr reference front() { return *begin(); }

	constexpr const_reference back() const { return *rbegin(); }
	constexpr reference back() { return *rbegin(); }

	constexpr size_type capacity() const { return cap_; }

	constexpr size_type size() const { return size_; }

	constexpr bool empty() const { return size() == 0u; }

	constexpr size_type max_size() const { return (~size_type(0)) >> 1u; }

	/**
	 * Returns the index into the raw memory buffer of the first 
	 * element.
	 */
	constexpr size_type begin_index() const {
		return start_;
	}

	/**
	 * Returns the index into the raw memory buffer of the element
	 * following the last element in the buffer.  If the buffer is either
	 * empty or at capacity, this is equal to begin_index().
	 */
	constexpr size_type end_index() const {
		return end().get_index();
	}

	/**
	 * @brief
	 * Increase the capacity of the buffer to a value that's greater or equal to new_cap.
	 * If new_cap is greater than the current capacity(), new storage is allocated, otherwise the method does nothing.
	 * reserve() does not change the size of the buffer.
	 * If new_cap is greater than capacity(), all iterators, including the past-the-end iterator, and all references to the elements are invalidated.
	 * Otherwise, no iterators or references are invalidated.
	 *
	 * @param new_cap New capacity of the buffer.
	 * 
	 * @note
	 * Calls to this overload always preserve the value of begin_index(); that is
	 * the extra capacity is always added on the end of the buffer.  See reserve_front()
	 * and reserve(size_type, size_type) for alternatives.
	 */
	constexpr void reserve(size_type new_cap) {
		if(new_cap > max_size()) {
			throw std::length_error("CircularBuffer::reserve()");
		}
		if(this->capacity() >= new_cap) {
			return;
		}
		this->reserve_slow(new_cap, this->start_);
	}

	/**
	 * @brief
	 * Increase the capacity of the buffer to a value that's greater or equal to new_cap, and set 
	 * the buffer's begin_index() to new_begin.
	 * If new_cap is greater than the current capacity(), new storage is allocated, otherwise the method's only
	 * effect is to change the value of begin_index().
	 * reserve() does not change the size of the buffer.
	 * If new_cap is greater than capacity(), all iterators, including the past-the-end iterator, and all references to the elements are invalidated.
	 * Otherwise, no iterators or references are invalidated.
	 *
	 * @param new_cap   New capacity of the buffer.
	 * @param new_begin New value of begin_index() for the buffer.  Must be in the range [0, new_cap).
	 * 
	 * @note
	 * Calls to this overload do not preserve the value of begin_index(). The new value
	 * of begin_index() is determined .  See reserve_front() and reserve(size_type) for
	 * alternatives.
	 */
	constexpr void reserve(size_type new_cap, size_type new_begin) {
		if(new_cap > max_size()) {
			throw std::length_error("CircularBuffer::reserve()");
		}
		if(this->capacity() >= new_cap) {
			if(start_ == new_start) {
				return;
			}
			
		}
		this->reserve_slow(new_cap, new_start);
	}

	template <class ... Args>
	constexpr void emplace_back(Args&& ... args) {
		if(size() < capacity()) {
			emplace_back_fast(std::forward<Args>(args)...);
			return;
		}
		auto tmp = make_temporary_buffer();
		tmp.reserve_fast(next_size<true>(this->capacity()));
		tmp.start_ = this->start_;
		auto pos = tmp.begin() + this->size();
		this->construct(pos.get_raw_pointer(), std::forward<Args>(args)...);
		{
			auto guard_ = make_manual_scope_guard([this, &pos](){
				this->destroy(pos.get_raw_pointer());
			});
			tmp.assign(
				try_make_move_iterator(this->begin()),
				try_make_move_iterator(this->end())
			);
			gaurd_.active = false;
		}
		tmp.start_ = pos.get_index();
		this->commit(tmp);
	}

	template <class ... Args>
	constexpr void emplace_front(Args&& ... args) {
		if(size() < capacity()) {
			emplace_back_fast(std::forward<Args>(args)...);
			return;
		}
		auto tmp = make_temporary_buffer();
		tmp.reserve_fast(next_size<true>(this->capacity()));
		tmp.start_ = this->start_;
		auto pos = tmp.begin() - 1;
		this->construct(pos.get_raw_pointer(), std::forward<Args>(args)...);
		{
			auto guard_ = detail::make_manual_scope_guard([this, &pos](){
				this->destroy(pos.get_raw_pointer());
			});
			tmp.assign(
				try_make_move_iterator(this->begin()),
				try_make_move_iterator(this->end())
			);
			gaurd_.active = false;
		}
		tmp.start_ = pos.get_index();
		this->commit(tmp);
	}

	template <
		class It,
		std::enable_if_t<
			detail::is_one_of_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::input_iterator_tag,
				std::forward_iterator_tag,
				std::bidirectional_iterator_tag,
				std::random_access_iterator_tag
			>,
			bool
		> = false
	>
	constexpr void assign(It first, It last) {
		this->clear();
		this->append(first, last);
	}

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
		const std::size_t spare = this->capacity() - this->size();
		std::size_t count_inserted = 0u;
		auto guard_ = detail::make_manual_scope_guard([this, spare, &count_inserted](){
			this->pop_back_n(count_inserted);
		});
		while(first != last && count_inserted < spare) {
			this->emplace_back(*first++);
			++count_inserted;
		}
		if(count_inserted < spare) {
			assert(first == last);
			guard_.active = false;
			return;
		}
		// Push the remainder of the iterator range into 'tmp'.
		// Once we've successfully copied everything else into 'tmp', we'll transfer over
		// all of the stuff still contained in '*this'.
		auto tmp = make_temporary_buffer();
		tmp.reserve_fast(next_size(this->capacity()));
		tmp.start_ = compute_offset(this->start_, this->size(), tmp.capacity());
		for(;;) {
			// Need to leave room for the stuff currently contained in '*this'.
			std::size_t size_limit = tmp.capacity() - this->size();
			while(first != last && tmp.size() < size_limit) {
				tmp.emplace_back(*first++);
			}
			if(tmp.size() < size_limit) {
				assert(first == last);
				break;
			}
			tmp.reserve_back(next_size(this->capacity()));
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

	template <
		class It,
		std::enable_if_t<
			detail::is_one_of_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::forward_iterator_tag,
				std::bidirectional_iterator_tag,
				std::random_access_iterator_tag
			>,
			bool
		> = false
	>
	constexpr void append(It first, It last) {
		std::size_t count = std::distance(first, last);
		if(this->capacity() - this->size() >= count) {
			append_fast(first, last, count);
			return;
		}
		auto tmp = make_temporary_buffer();
		tmp.reserve_fast(this->size() + count);
		tmp.start_ = compute_offset(this->start_, this->size(), tmp.capacity());
		tmp.append_fast(first, last, count);
		for(auto range: detail::reversed(this->get_ranges())) {
			tmp.prepend_fast(
				try_make_move_iterator(range.begin()),
				try_make_move_iterator(range.end()),
				range.size()
			);
		}
		this->commit(tmp);
	}

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
	constexpr void prepend(It first, It last) {
		const std::size_t spare = this->capacity() - this->size();
		std::size_t count_inserted = 0u;
		auto guard_ = detail::make_manual_scope_guard([this, spare, &count_inserted](){
			this->pop_front_n(count_inserted);
		});
		while(first != last && count_inserted < spare) {
			this->emplace_front(*first++);
			++count_inserted;
		}
		if(count_inserted < spare) {
			assert(first == last);
			std::reverse(this->begin(), this->begin() + count_inserted);
			guard_.active = false;
			return;
		}
		// Push the remainder of the iterator range into 'tmp'.
		// Once we've successfully copied everything else into 'tmp', we'll transfer over
		// all of the stuff still contained in '*this'.
		auto tmp = make_temporary_buffer();
		tmp.reserve_fast(next_size(this->capacity()));
		tmp.start_ = this->start_;
		for(;;) {
			// Need to leave room for the stuff currently contained in '*this'.
			std::size_t size_limit = tmp.capacity() - this->size();
			while(first != last && tmp.size() < size_limit) {
				tmp.emplace_front(*first++);
			}
			if(tmp.size() < size_limit) {
				assert(first == last);
				break;
			}
			tmp.reserve_front(next_size(this->capacity()));
		}
		std::reverse(tmp.begin(), tmp.end());
		tmp.prepend_fast(
			std::make_move_iterator(this->rend() + count_inserted),
			std::make_move_iterator(this->rend()),
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

	template <
		class It,
		std::enable_if_t<
			detail::is_one_of_v<
				typename std::iterator_traits<It>::iterator_category, 
				std::forward_iterator_tag,
				std::bidirectional_iterator_tag,
				std::random_access_iterator_tag
			>,
			bool
		> = false
	>
	constexpr void prepend(It first, It last) {
		std::size_t count = std::distance(first, last);
		if(this->capacity() - this->size() >= count) {
			prepend_fast(first, last, count);
			return;
		}
		auto tmp = make_temporary_buffer();
		tmp.reserve_fast(this->size() + count);
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
	 * Modifies the underlying storage such that the new starting index
	 */
	constexpr void shift_left(size_type count) const {
		if(count >= capacity()) {
			count %= capacity();
		}
		if(count == 0u) {
			return;
		}
		if(size() == capacity()) {
			std::rotate(data_, data_ + count, data_ + capacity());
			start_ = (begin() - count).get_index();
			return;
		}
		size_type spare = (capacity() - size());
		if(count <= spare) {
			auto start = this->begin();
			this->prepend_fast(
				try_make_move_iterator(start),
				try_make_move_iterator(start + count),
				count
			);
			auto stop = std::copy(
				this->try_make_move_iterator(start + count),
				this->try_make_move_iterator(this->end()),
				start
			);
			this->pop_back_n(this->end() - stop);
		} else if(count >= size()) {
			size_type overlap = count - spare;
			auto start = this->begin();
			// fill
			this->prepend_fast(
				try_make_move_iterator(start) + overlap,
				try_make_move_iterator(start + spare) + overlap,
				spare
			);
			auto stop = std::copy(
				try_make_move_iterator(start + spare) + overlap,
				try_make_move_iterator(this->end()),
				start + overlap
			);
			this->pop_back_n(this->end() - stop);
			std::copy(
				(this->end() - overlap) - count
		}
	}

private:
	constexpr CircularBuffer<T, AllocatorRef<Allocator>> make_temporary_buffer() const noexcept {
		return CircularBuffer<T, AllocatorRef<Allocator>>(AllocatorRef<Allocator>(this->alloc()));
	}

	constexpr void reserve_slow(size_type new_cap, size_type new_start) {
		auto tmp = make_temporary_buffer();
		tmp.reserve_fast(new_cap);
		tmp.start_ = new_start;
		for(auto range: get_ranges()) {
			tmp.append_fast(
				try_make_move_iterator(range.begin()),
				try_make_move_iterator(range.end())
			);
		}
		this->clear();
		if(this->data_) {
			this->deallocate(this->data_, this->capacity());
		}
		this->data_  = std::exchange(tmp.data_, nullptr);
		this->cap_   = std::exchange(tmp.cap_, 0u);
		tmp.size_ = 0u;
		tmp.start_ = 0u;
	}

	constexpr void reserve_fast(size_type count) {
		data_ = this->allocate(count);
		cap_ = count;
	}

	template <class It>
	constexpr void append_fast(It first, It last, size_type size_hint) {
		append_fast(first, last);
	}

	template <class It>
	constexpr void append_fast(It first, It last) {
		for(auto range: get_spare_ranges()) {
			for(auto& elem: range) {
				if(first == last) {
					return;
				}
				this->construct(std::addressof(range), *first++);
				++size_;
			}
		}
	}

	template <class It>
	constexpr void prepend_fast(It first, It last) {
		using iter_cat_type = typename std::iterator_traits<It>::iterator_category;
		static_assert(!std::is_same_v<iter_cat_type, std::input_iterator_tag>, "");
		prepend_fast(first, last, std::distance(first, last));
	}

	template <class It>
	constexpr void prepend_fast(It first, It last, size_type size_hint) {
		auto ranges = get_spare_ranges();
		assert(size_hint <= ranges.total());
		if(ranges.size() == 1 || (ranges.begin()[1].size() >= size_hint)) {
			auto range = ranges.end()[-1];
			auto p = range.end() - size_hint;
			RangeGuard guard_{std::addressof(this->alloc()), p, p};
			for(std::size_t i = 0u; i < size_hint; (void)++i, ++guard_.stop) {
				assert(first != last);
				this->construct(guard_.stop, *first++);
			}
			guard_.alloc = nullptr;
			assert(first == last);
		} else {
			auto back_range = *ranges.begin();
			const size_type back_range_count = (size_hint - ranges.begin()[1].size());
			pointer p = back_range.end() - back_range_count;
			RangeGuard back_guard_{std::addressof(this->alloc()), p, p};
			for(std::size_t i = 0u; i < back_range_count; (void)++i, ++back_guard_.stop) {
				assert(first != last);
				this->construct(back_guard_.stop, *first++);
			}

			auto front_range = ranges.begin()[1];
			const size_type front_range_count = front_range.size();
			p = front_range.begin(); 
			RangeGuard front_guard_{std::addressof(this->alloc()), p, p};
			assert(front_range_count == (size_hint - back_range_size));
			for(std::size_t i = 0u; i < back_range_count; (void)++i, ++back_guard_.stop) {
				assert(first != last);
				this->construct(back_guard_.stop, *first++);
			}

			front_guard_.alloc = nullptr;
			back_guard_.alloc = nullptr;
			assert(first == last);
		}
	}

	constexpr void commit(CircularBuffer<T, AllocatorRef<Allocator>>& other) noexcept {
		this->clear();
		if(this->data_) {
			this->deallocate(this->data_, this->capacity());
		}
		this->data_  = std::exchange(other.data_, nullptr);
		this->cap_   = std::exchange(other.cap_, 0u);
		this->size_  = std::exchange(other.size_, 0u);
		this->start_ = std::exchange(other.start_, 0u);
	}

	template <class ... Args>
	constexpr void emplace_back_fast(Args&& ... args) {
		construct(this->end().get_raw_pointer(), std::forward<Args>(args)...);
		++size_;
	}

	template <bool Grow>
	static constexpr size_type next_size(size_type n) {
		// Safely multiply by the golden ratio without floating point.
		using wide_int_type = next_widest_int_t<size_type>;
		constexpr auto fib_pair = detail::get_largest_representable_fib_pair<size_type>();
		constexpr uint_type numer = Grow ? fib_pair.high : fib_pair.low;
		constexpr uint_type denom = Grow ? fib_pair.low : fib_pair.high;

		if constexpr(sizeof(wide_int_type) >= 2ull * sizeof(size_type)) {
			wide_int_type product = static_cast<wide_int_type>(n) * numer;
			wide_int_type quot = static_cast<wide_int_type>(n) / denom;
			assert(quot == static_cast<size_type>(quot) && "Overflow occurred.");
			return static_cast<size_type>(quot);
		} else {
#if defined(TIM_CICULAR_BUFFER_NO_USE_INTRINSICS)
			std::optional<size_type> result = detail::safe_scale_by_ratio(numer, denom, n);
			assert(result && "Overflow occurred.");
			return *result;
#elif defined(__GNUC__) && defined(__SIZEOF_INT128__)
			if constexpr(sizeof(unsigned __int128) >= 2ull * sizeof(size_type)) {
				unsigned __int128 product = static_cast<unsigned __int128>(n) * numer;
				unsigned __int128 quot = static_cast<unsigned __int128>(n) / denom;
				assert(quot == static_cast<size_type>(quot) && "Overflow occurred.");
				return static_cast<size_type>(quot);
			} else {
				std::optional<size_type> result = detail::safe_scale_by_ratio(numer, denom, n);
				assert(result && "Overflow occurred.");
				return *result;
			}
#elif defined(_MSC_VER)
# if _MSC_VER >= 1920
			if constexpr(sizeof(unsigned __int64) >= sizeof(size_type)) {
				unsigned __int64 product_hi = 0u;
				unsigned __int64 product_lo = _mul128(
					static_cast<unsigned __int64>(n),
					static_cast<unsigned __int64>(numer),
					&product_hi
				);
				assert(product_hi < denom && "Overflow occurred.");
				[[maybe_unused]]
				unsigned __int64 rem = 0u;
				unsigned __int128 quot = _udiv128(product_hi, product_lo, denom, &rem);
				assert(quot == static_cast<size_type>(quot) && "Overflow occurred.");
				return static_cast<size_type>(quot);
			} else {
# endif
				std::optional<size_type> result = detail::safe_scale_by_ratio(numer, denom, n);
				assert(result && "Overflow occurred.");
				return *result;
# if _MSC_VER >= 1920
			}
# endif
#endif
		}
	}

	constexpr SplitRanges<const_pointer> get_spare_ranges() const {
		auto start = begin();
		auto stop = end();
		if(stop.get_has_wrapped()) {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>{stop.get_pointer(), start_.tagged_index_ - stop.get_index()}
			);
			
		} else if(start.tagged_index_ == 0u) {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>{stop.get_pointer(), cap_ - stop.get_index()}
			);
		} else {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>{stop.get_pointer(), cap_ - stop.get_index()},
				SplitRange<const_pointer>{data_, start.tagged_index_},
			);
		}
	}

	constexpr SplitRanges<pointer> get_spare_ranges() {
		auto start = begin();
		auto stop = end();
		if(stop.get_has_wrapped()) {
			return SplitRanges<pointer>(
				SplitRange<pointer>{stop.get_pointer(), start_.tagged_index_ - stop.get_index()}
			);
			
		} else if(start.tagged_index_ == 0u) {
			return SplitRanges<pointer>(
				SplitRange<pointer>{stop.get_pointer(), cap_ - stop.get_index()}
			);
		} else {
			return SplitRanges<pointer>(
				SplitRange<pointer>{stop.get_pointer(), cap_ - stop.get_index()},
				SplitRange<pointer>{data_, start.tagged_index_},
			);
		}
	}

	constexpr SplitRanges<const_pointer> get_ranges() const {
		auto start = begin();
		auto stop = end();
		if(!stop.get_has_wrapped()) {
			return SplitRanges<const_pointer>{
				SplitRange<const_pointer>{start.get_pointer(), stop.tagged_index_ - start.tagged_index_}
			};
		} else if(stop.get_index() == 0u) {
			return SplitRanges<const_pointer>(
				SplitRange<const_pointer>{start.get_pointer(), cap_ - start.tagged_index_}
			);
		} else {
			return SplitRanges<const_pointer>{
				SplitRange<const_pointer>{start.get_pointer(), cap_ - start.tagged_index_},
				SplitRange<const_pointer>{data_, stop.get_index()}
			};
		}
	}

	template <class It>
	static constexpr auto try_make_move_iterator(It it)
		-> std::conditional_t<
			std::is_nothrow_move_constructible_v<T>
			|| !detail::is_copy_insertible_v<T>,
			std::move_iterator<It>,
			It
		>
	{
		if constexpr(std::is_nothrow_move_constructible_v<T> || !detail::is_copy_insertible_v<T>) {
			return std::make_move_iterator(it);
		} else {
			return it;
		}
	}

	static constexpr size_type compute_offset(size_type start, size_type count, size_type cap) {
		assert(cap > start);
		assert(cap >= count);
		if(cap - start > count) {
			return start + count;
		}
		return count - (cap - start);
	}

	template <class U, class OtherAlloc>
	friend class CircularBuffer;

	pointer data_;
	size_type cap_;
	size_type size_;
	size_type start_;
};

} /* inline namespace circular_buffer */

} /* namespace tim */


#endif /* TIM_CIRCULAR_BUFFER_HPP */
