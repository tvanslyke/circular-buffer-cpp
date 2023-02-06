//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// vector(vector&& c, const allocator_type& a);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include "test_macros.h"
#include "MoveOnly.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"

template <class C, class A>
void
test(C x, const C cpy, const A& a)
{
	auto s = x.size();
	auto cap = x.capacity();
	auto begin_idx = x.begin_index();
	C c(std::move(x), a);
	c._assert_invariants();
	x._assert_invariants();
	cpy._assert_invariants();
	assert(c == cpy);
	assert(c.size() == s);
	assert(c.capacity() == cap);
	assert(c.begin_index() == begin_idx);
	assert(cpy == c);
	assert(x.empty());
	assert(c.get_allocator() == a);
	x = std::move(c);
}

template <class C, class A>
void
test_optimized(C x, const C cpy, const A& a)
{
	auto s = x.size();
	auto cap = x.capacity();
	auto begin_idx = x.begin_index();
	C c(tim::tags::optimized, std::move(x), a);
	c._assert_invariants();
	x._assert_invariants();
	cpy._assert_invariants();
	assert(c == cpy);
	assert(c.size() == s);
	assert(c.capacity() == cap);
	assert(c.begin_index() == begin_idx);
	assert(cpy == c);
	assert(x.empty());
	assert(c.get_allocator() == a);
	x = std::move(c);
}

template <class C, class A>
void
test_preserve_capacity(C x, const C cpy, const A& a)
{
	auto s = x.size();
	auto cap = x.capacity();
	auto begin_idx = x.begin_index();
	C c(tim::tags::preserve_capacity, std::move(x), a);
	c._assert_invariants();
	x._assert_invariants();
	cpy._assert_invariants();
	assert(c == cpy);
	assert(c.size() == s);
	assert(c.capacity() == cap);
	assert(c.begin_index() == begin_idx);
	assert(cpy == c);
	assert(x.empty());
	assert(c.get_allocator() == a);
	x = std::move(c);
}

template <class C, class A>
void
test_preserve_layout(C x, const C cpy, const A& a)
{
	auto s = x.size();
	auto cap = x.capacity();
	auto begin_idx = x.begin_index();
	C c(tim::tags::preserve_layout, std::move(x), a);
	c._assert_invariants();
	x._assert_invariants();
	cpy._assert_invariants();
	assert(c == cpy);
	assert(c.size() == s);
	assert(c.capacity() == cap);
	assert(c.begin_index() == begin_idx);
	assert(cpy == c);
	assert(x.empty());
	assert(c.get_allocator() == a);
	x = std::move(c);
}

int main(int, char**)
{
	{
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l(test_allocator<MoveOnly>(5));
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > lo(test_allocator<MoveOnly>(5));
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l2(std::move(l), test_allocator<MoveOnly>(6));
		assert(l2 == lo);
		assert(!l.empty());
		assert(l2.get_allocator() == test_allocator<MoveOnly>(6));
		assert(is_contiguous_container_asan_correct(l2));
	}
	{
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l(test_allocator<MoveOnly>(5));
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > lo(test_allocator<MoveOnly>(5));
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l2(std::move(l), test_allocator<MoveOnly>(5));
		assert(l2 == lo);
		assert(l.empty());
		assert(l2.get_allocator() == test_allocator<MoveOnly>(5));
		assert(is_contiguous_container_asan_correct(l2));
	}
	{
		tim::CircularBuffer<MoveOnly, other_allocator<MoveOnly> > l(other_allocator<MoveOnly>(5));
		tim::CircularBuffer<MoveOnly, other_allocator<MoveOnly> > lo(other_allocator<MoveOnly>(5));
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		tim::CircularBuffer<MoveOnly, other_allocator<MoveOnly> > l2(std::move(l), other_allocator<MoveOnly>(4));
		assert(l2 == lo);
		assert(!l.empty());
		assert(l2.get_allocator() == other_allocator<MoveOnly>(4));
		assert(is_contiguous_container_asan_correct(l2));
	}
	{
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly> > l(min_allocator<MoveOnly>{});
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly> > lo(min_allocator<MoveOnly>{});
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly> > l2(std::move(l), min_allocator<MoveOnly>());
		assert(l2 == lo);
		assert(l.empty());
		assert(l2.get_allocator() == min_allocator<MoveOnly>());
		assert(is_contiguous_container_asan_correct(l2));
	}
	{
		for (auto fn: get_test_buffer_makers())
		{
			test(fn(), fn(), std::allocator<int>());
			test_optimized(fn(), fn(), std::allocator<int>());
			test_preserve_capacity(fn(), fn(), std::allocator<int>());
			test_preserve_layout(fn(), fn(), std::allocator<int>());
		}
	}
	{
		for (auto fn: get_test_buffer_makers(std::allocator<MoveOnly>{}))
		{
			test(fn(), fn(), std::allocator<MoveOnly>{});
			test_optimized(fn(), fn(), std::allocator<MoveOnly>{});
			test_preserve_capacity(fn(), fn(), std::allocator<MoveOnly>{});
			test_preserve_layout(fn(), fn(), std::allocator<MoveOnly>{});
		}
	}
	{
		for (auto fn: get_test_buffer_makers(test_allocator<int>()))
		{
			test(fn(), fn(), test_allocator<int>());
			test_optimized(fn(), fn(), test_allocator<int>());
			test_preserve_capacity(fn(), fn(), test_allocator<int>());
			test_preserve_layout(fn(), fn(), test_allocator<int>());
		}
	}
	{
		for (auto fn: get_test_buffer_makers(test_allocator<MoveOnly>()))
		{
			test(fn(), fn(), test_allocator<MoveOnly>());
			test_optimized(fn(), fn(), test_allocator<MoveOnly>());
			test_preserve_capacity(fn(), fn(), test_allocator<MoveOnly>());
			test_preserve_layout(fn(), fn(), test_allocator<MoveOnly>());
		}
	}
	{
		for (auto fn: get_test_buffer_makers(min_allocator<MoveOnly>()))
		{
			test(fn(), fn(), min_allocator<MoveOnly>());
			test_optimized(fn(), fn(), min_allocator<MoveOnly>());
			test_preserve_capacity(fn(), fn(), min_allocator<MoveOnly>());
			test_preserve_layout(fn(), fn(), min_allocator<MoveOnly>());
		}
	}
	return 0;
}
