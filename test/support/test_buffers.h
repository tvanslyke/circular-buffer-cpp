#ifndef TEST_BUFFERS_H
#define TEST_BUFFERS_H

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <vector>


template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_empty_buffer(const A& a = A())
{
	tim::CircularBuffer<T, A> buf(a);
	assert(buf.empty());
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_empty_buffer_with_capacity(const A& a = A())
{
	tim::CircularBuffer<T, A> buf(a);
	buf.reserve(1);
	assert(buf.size() == 0);
	assert(buf.capacity() >= 1);
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_unary_buffer(const A& a = A())
{
	tim::CircularBuffer<T, A> buf(1, a);
	assert(buf.size() == 1);
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_unary_buffer_with_back_slack(const A& a = A())
{
	tim::CircularBuffer<T, A> buf(1, a);
	buf.reserve(2);
	assert(buf.size() == 1);
	assert(buf.capacity() >= 2);
	assert(buf.begin_index() == 0);
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_unary_buffer_with_front_slack(const A& a = A())
{
	tim::CircularBuffer<T, A> buf(1, a);
	buf.reserve_front(2);
	assert(buf.size() == 1);
	assert(buf.capacity() >= 2);
	assert(buf.begin_index() == 1);
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_simple_wrapped_buffer(const A& a = A())
{
	auto buf = tim::CircularBuffer<T, A>::from_shape({3,2}, a);
	buf.resize(2);
	assert(&buf.front() > &buf.back());
	assert(buf.size() == 2);
	assert(buf.capacity() == 3);
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_simple_buffer_with_front_and_back_slack(const A& a = A())
{
	auto buf = tim::CircularBuffer<T, A>::from_shape({ 8, 2 }, a);
	buf.resize(5);
	assert(&buf.front() < &buf.back());
	assert(buf.size() == 5);
	assert(buf.capacity() == 8);
	assert(buf.begin_index() == 2);
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_large_wrapped_buffer(const A& a = A())
{
	tim::CircularBuffer<T, A> buf(100);
	buf.pop_front_n(50);
	buf.resize(75);
	assert(&buf.front() > &buf.back());
	assert(buf.size() == 75);
	assert(buf.begin_index() == 50);
	assert(buf.end_index() == 25);
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_large_full_buffer(const A& a = A())
{
	tim::CircularBuffer<T, A> buf(25, a);
	buf.shrink_to_fit();
	assert(buf.begin_index() == 0);
	assert(buf.capacity() == buf.size() ? buf.end_index() == 0 : buf.end_index() == 25);
	assert(buf.get_allocator() == a);
	return buf;
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
auto get_large_full_wrapped_buffer(const A& a = A()) -> tim::CircularBuffer<T, A>
{
	tim::CircularBuffer<T, A> buf(25, a);
	buf.shrink_to_fit();
	buf.shift_right(10);
	assert(buf.begin_index() == 10);
	assert(buf.end_index() == 10);
	assert(buf.get_allocator() == a);
	return buf;
}


template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
std::vector<std::function<tim::CircularBuffer<T, A>()>> get_test_buffer_makers(const A& a = A())
{
	return {
		std::bind(get_empty_buffer<A, T>, a),
		std::bind(get_empty_buffer_with_capacity<A, T>, a),
		std::bind(get_unary_buffer<A, T>, a),
		std::bind(get_unary_buffer_with_back_slack<A, T>, a),
		std::bind(get_unary_buffer_with_front_slack<A, T>, a),
		std::bind(get_simple_wrapped_buffer<A, T>, a),
		std::bind(get_simple_buffer_with_front_and_back_slack<A, T>, a),
		std::bind(get_large_wrapped_buffer<A, T>, a),
		std::bind(get_large_full_buffer<A, T>, a),
		std::bind(get_large_full_wrapped_buffer<A, T>, a)
	};
}

template <class A=std::allocator<int>, class T = typename std::allocator_traits<A>::value_type>
std::vector<tim::CircularBuffer<T, A>> get_test_buffers(const A& a = A())
{
	std::vector<tim::CircularBuffer<T, A>> v;
	v.reserve(10);
	v.emplace_back(get_empty_buffer(a));
	v.emplace_back(get_empty_buffer_with_capacity(a));
	v.emplace_back(get_unary_buffer(a));
	v.emplace_back(get_unary_buffer_with_back_slack(a));
	v.emplace_back(get_unary_buffer_with_front_slack(a));
	v.emplace_back(get_simple_wrapped_buffer(a));
	v.emplace_back(get_simple_buffer_with_front_and_back_slack(a));
	v.emplace_back(get_large_wrapped_buffer(a));
	v.emplace_back(get_large_full_buffer(a));
	v.emplace_back(get_large_full_wrapped_buffer(a));
	return v;
}

#endif  /* TEST_BUFFERS_H */
