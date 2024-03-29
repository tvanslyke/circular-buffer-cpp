//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// vector(vector&& c);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "MoveOnly.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"
#include "archetypes.h"

template <class C>
void
test(C x, const C cpy)
{
	auto s = x.size();
	auto cap = x.capacity();
	auto begin_idx = x.begin_index();
	C c(std::move(x));
	c._assert_invariants();
	x._assert_invariants();
	cpy._assert_invariants();
	assert(c == cpy);
	assert(c.size() == s);
	assert(c.capacity() == cap);
	assert(c.begin_index() == begin_idx);
	assert(cpy == c);
	assert(x.empty());
	x = std::move(c);
}

template <class C>
void
test_optimized(C x, const C cpy)
{
	auto s = x.size();
	auto cap = x.capacity();
	auto begin_idx = x.begin_index();
	C c(tim::tags::optimized, std::move(x));
	c._assert_invariants();
	x._assert_invariants();
	cpy._assert_invariants();
	assert(c == cpy);
	assert(c.size() == s);
	assert(c.capacity() == cap);
	assert(c.begin_index() == begin_idx);
	assert(cpy == c);
	assert(x.empty());
	x = std::move(c);
}

template <class C>
void
test_preserve_capacity(C x, const C cpy)
{
	auto s = x.size();
	auto cap = x.capacity();
	auto begin_idx = x.begin_index();
	C c(tim::tags::preserve_capacity, std::move(x));
	c._assert_invariants();
	x._assert_invariants();
	cpy._assert_invariants();
	assert(c == cpy);
	assert(c.size() == s);
	assert(c.capacity() == cap);
	assert(c.begin_index() == begin_idx);
	assert(cpy == c);
	assert(x.empty());
	x = std::move(c);
}

template <class C>
void
test_preserve_layout(C x, const C cpy)
{
	auto s = x.size();
	auto cap = x.capacity();
	auto begin_idx = x.begin_index();
	C c(tim::tags::preserve_layout, std::move(x));
	c._assert_invariants();
	x._assert_invariants();
	cpy._assert_invariants();
	assert(c == cpy);
	assert(c.size() == s);
	assert(c.capacity() == cap);
	assert(c.begin_index() == begin_idx);
	assert(cpy == c);
	assert(x.empty());
	x = std::move(c);
}

int main(int, char**)
{
	{
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l(test_allocator<MoveOnly>(5));
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > lo(test_allocator<MoveOnly>(5));
		l._assert_invariants();
		lo._assert_invariants();
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		l._assert_invariants();
		lo._assert_invariants();
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l2 = std::move(l);
		assert(l2 == lo);
		assert(l.empty());
		assert(l2.get_allocator() == lo.get_allocator());
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
		tim::CircularBuffer<MoveOnly, other_allocator<MoveOnly> > l2 = std::move(l);
		assert(l2 == lo);
		assert(l.empty());
		assert(l2.get_allocator() == lo.get_allocator());
		assert(is_contiguous_container_asan_correct(l2));
	}
	{
		int a1[] = {1, 3, 7, 9, 10};
		tim::CircularBuffer<int> c1(a1, a1+sizeof(a1)/sizeof(a1[0]));
		assert(is_contiguous_container_asan_correct(c1));
		tim::CircularBuffer<int>::const_iterator i = c1.begin();
		tim::CircularBuffer<int> c2 = std::move(c1);
		assert(is_contiguous_container_asan_correct(c2));
		tim::CircularBuffer<int>::iterator j = c2.erase(i);
		assert(*j == 3);
		assert(is_contiguous_container_asan_correct(c2));
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
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly> > l2 = std::move(l);
		assert(l2 == lo);
		assert(l.empty());
		assert(l2.get_allocator() == lo.get_allocator());
		assert(is_contiguous_container_asan_correct(l2));
	}
	{
		int a1[] = {1, 3, 7, 9, 10};
		tim::CircularBuffer<int, min_allocator<int>> c1(a1, a1+sizeof(a1)/sizeof(a1[0]));
		assert(is_contiguous_container_asan_correct(c1));
		tim::CircularBuffer<int, min_allocator<int>>::const_iterator i = c1.begin();
		tim::CircularBuffer<int, min_allocator<int>> c2 = std::move(c1);
		assert(is_contiguous_container_asan_correct(c2));
		tim::CircularBuffer<int, min_allocator<int>>::iterator j = c2.erase(i);
		assert(*j == 3);
		assert(is_contiguous_container_asan_correct(c2));
	}
	{
		test_alloc_base::clear();
		using Vect = tim::CircularBuffer<int, test_allocator<int> >;
		Vect v(test_allocator<int>(42, 101));
		assert(test_alloc_base::count == 1);
		assert(test_alloc_base::copied == 1);
		assert(test_alloc_base::moved == 0);
		{
			  const test_allocator<int>& a = v.get_allocator();
			  assert(a.get_data() == 42);
			  assert(a.get_id() == 101);
		}
		assert(test_alloc_base::count == 1);
		test_alloc_base::clear_ctor_counters();

		Vect v2 = std::move(v);
		assert(test_alloc_base::count == 2);
		assert(test_alloc_base::copied == 0);
		assert(test_alloc_base::moved == 1);
		{
			  const test_allocator<int>& a = v.get_allocator();
			  assert(a.get_id() == test_alloc_base::moved_value);
			  assert(a.get_data() == test_alloc_base::moved_value);
		}
		{
			  const test_allocator<int>& a = v2.get_allocator();
			  assert(a.get_id() == 101);
			  assert(a.get_data() == 42);
		}
	}

	{
		for (auto fn: get_test_buffer_makers())
		{
			test(fn(), fn());
			test_optimized(fn(), fn());
			test_preserve_capacity(fn(), fn());
			test_preserve_layout(fn(), fn());
		}
	}
	{
		for (auto fn: get_test_buffer_makers(std::allocator<MoveOnly>{}))
		{
			test(fn(), fn());
			test_optimized(fn(), fn());
			test_preserve_capacity(fn(), fn());
			test_preserve_layout(fn(), fn());
		}
	}
	{
		for (auto fn: get_test_buffer_makers(std::allocator<MoveOnly>{}))
		{
			test(fn(), fn());
			test_optimized(fn(), fn());
			test_preserve_capacity(fn(), fn());
			test_preserve_layout(fn(), fn());
		}
	}
	{
		for (auto fn: get_test_buffer_makers(test_allocator<int>()))
		{
			test(fn(), fn());
			test_optimized(fn(), fn());
			test_preserve_capacity(fn(), fn());
			test_preserve_layout(fn(), fn());
		}
	}
	{
		for (auto fn: get_test_buffer_makers(test_allocator<MoveOnly>()))
		{
			test(fn(), fn());
			test_optimized(fn(), fn());
			test_preserve_capacity(fn(), fn());
			test_preserve_layout(fn(), fn());
		}
	}
	{
		for (auto fn: get_test_buffer_makers(min_allocator<MoveOnly>()))
		{
			test(fn(), fn());
			test_optimized(fn(), fn());
			test_preserve_capacity(fn(), fn());
			test_preserve_layout(fn(), fn());
		}
	}
  return 0;
}
