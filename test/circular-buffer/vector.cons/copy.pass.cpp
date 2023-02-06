//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// vector(const vector& v);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"

template <class C>
void
test(const C& x)
{
	{
		typename C::size_type s = x.size();
		C c(x);
		c._assert_invariants();
		assert(c.size() == s);
		assert(c == x);
		assert(x.size() == s);
	}
	{
		typename C::size_type s = x.size();
		C c(tim::tags::optimized, x);
		c._assert_invariants();
		assert(c.size() == s);
		assert(c == x);
		assert(c.capacity() == c.size());
	}
	{
		typename C::size_type s = x.size();
		C c(tim::tags::preserve_capacity, x);
		c._assert_invariants();
		assert(c.size() == s);
		assert(c == x);
		assert(c.capacity() == x.capacity());
		assert(c.begin_index() == 0);
	}
	{
		typename C::size_type s = x.size();
		C c(tim::tags::preserve_layout, x);
		c._assert_invariants();
		assert(c.size() == s);
		assert(c == x);
		assert(c.capacity() == x.capacity());
		assert(c.begin_index() == x.begin_index());
	}
}



int main(int, char**)
{
	{
		int a[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 1, 0};
		int* an = a + sizeof(a)/sizeof(a[0]);
		test(tim::CircularBuffer<int>(a, an));
	}
	{
		tim::CircularBuffer<int, test_allocator<int> > v(3, 2, test_allocator<int>(5));
		tim::CircularBuffer<int, test_allocator<int> > v2 = v;
		assert(is_contiguous_container_asan_correct(v));
		assert(is_contiguous_container_asan_correct(v2));
		assert(v2 == v);
		assert(v2.get_allocator() == v.get_allocator());
		assert(is_contiguous_container_asan_correct(v));
		assert(is_contiguous_container_asan_correct(v2));
	}
	{
		tim::CircularBuffer<int, other_allocator<int> > v(3, 2, other_allocator<int>(5));
		tim::CircularBuffer<int, other_allocator<int> > v2 = v;
		assert(is_contiguous_container_asan_correct(v));
		assert(is_contiguous_container_asan_correct(v2));
		assert(v2 == v);
		assert(v2.get_allocator() == other_allocator<int>(-2));
		assert(is_contiguous_container_asan_correct(v));
		assert(is_contiguous_container_asan_correct(v2));
	}
	{
		int a[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 1, 0};
		int* an = a + sizeof(a)/sizeof(a[0]);
		test(tim::CircularBuffer<int, min_allocator<int>>(a, an));
	}
	{
		tim::CircularBuffer<int, min_allocator<int> > v(3, 2, min_allocator<int>());
		tim::CircularBuffer<int, min_allocator<int> > v2 = v;
		assert(is_contiguous_container_asan_correct(v));
		assert(is_contiguous_container_asan_correct(v2));
		assert(v2 == v);
		assert(v2.get_allocator() == v.get_allocator());
		assert(is_contiguous_container_asan_correct(v));
		assert(is_contiguous_container_asan_correct(v2));
	}
	for (const auto& buf : get_test_buffers())
	{
		test(buf);
	}

  return 0;
}
