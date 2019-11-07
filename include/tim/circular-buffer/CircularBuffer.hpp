#ifndef TIM_CIRCULAR_BUFFER_HPP
#define TIM_CIRCULAR_BUFFER_HPP

#include <utility>
#include <memory>
#include <algorithm>
#include <cassert>

namespace tim {

inline namespace circular_buffer {

namespace detail {

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

template <class UInt>
constexpr std::array<UInt, 2> safe_divmod(UInt numer, UInt denom) {
	UInt quot = numer / denom;
	return std::array<UInt, 2>{quot, quot * denom};
}

template <class UInt>
constexpr std::array<UInt, 2> safe_divide(std::array<UInt, 2> numerator, UInt denominator) {
}
 
template <class UInt>
constexpr std::array<UInt, 2> safe_scale_by_ratio(UInt numer, UInt denom, UInt value) {
	std::array<UInt, 2> mul = safe_multiply(numer, value);
	if(!mul[1]) {
		return {mul[0] / denom, 0u};
	}
	constexpr std::size_t uint_bits = sizeof(UInt) * CHAR_BIT;
	std::size_t mul_shift = 0;
	std::size_t denom_shift = 0;
	UInt head = 0;
	for(mul_shift = 0; mul_shift < uint_bits; ++mul_shift) {
		UInt head = mul[1] << mul_shift;
	}
	
	return std::array<UInt, 2>{quot, quot * denom};
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

	template <class ... Args>
	constexpr void emplace_back(Args&& ... args) {
		if(size() < capacity()) {
			construct(this->end().get_pointer(), std::forward<Args>(args)...);
			++size_;
			return;
		}
		
	}

private:
	template <bool Grow>
	static constexpr size_type next_size(size_type n) {
		// Safely multiply by the golden ratio without floating point.

		using uint_type = size_type;
		// Golden ratio or its inverse approximated as a fraction:
		constexpr auto fib_pair = detail::get_largest_representable_fib_pair<uint_type>();
		constexpr uint_type numer = Grow ? fib_pair.high : fib_pair.low;
		constexpr uint_type denom = Grow ? fib_pair.low : fib_pair.high;

		switch(n) {
		case 0u: return 1u;
		case 1u: return 2u;
		case 2u: return 4u;
		default:
			break;
		}

		constexpr uint_type uint_type_bits = CHAR_BIT * sizeof(uint_type);
		constexpr uint_type shift = uint_type_bits / 2u;
		
		constexpr uint_type numer_lo = numer & ((1ull << shift) - 1);
		constexpr uint_type numer_hi = numer >> shift;
		constexpr uint_type denom_lo = denom & ((1ull << shift) - 1);
		constexpr uint_type denom_hi = denom >> shift;

		uint_type sz = n;

		uint_type sz_lo = sz & (shift - 1);
		uint_type sz_hi = sz >> shift;

		bool carry = false;
		uint_type result_lo = numer_lo * sz_lo;
		uint_type result_hi = numer_hi * sz_hi;
		{
			uint_type result_mid = 0;
			uint_type lhs = numer_lo * sz_hi;
			uint_type rhs = numer_hi * sz_lo;
			result_mid = lhs + rhs;
			// Carry
			result_hi += ((result_mid < lhs) ? 1ull : 0ull) << shift;
			result
		}
		constexpr auto divmod = [shift](uint_type n, uint_type d) -> std::pair<uint_type, uint_type> {
			uint_type n_lo = (n & ((1ull << shift) - 1)) << shift;
			uint_type n_hi = (n & ~((1ull << shift) - 1));
			uint_type quot = n_hi / d;
			
		};
	}

	pointer data_;
	size_type cap_;
	size_type size_;
	size_type start_;
};

} /* inline namespace circular_buffer */

} /* namespace tim */


#endif /* TIM_CIRCULAR_BUFFER_HPP */
