//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// vector& operator=(const vector& c);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "test_buffers.h"
#include "Counter.h"

int main(int, char**)
{
	{
		tim::CircularBuffer<int, test_allocator<int> > l(3, 2, test_allocator<int>(5));
		tim::CircularBuffer<int, test_allocator<int> > l2(l, test_allocator<int>(3));
		l2 = l;
		assert(l2 == l);
		assert(l2.get_allocator() == test_allocator<int>(3));
	}
	{
		tim::CircularBuffer<int, other_allocator<int> > l(3, 2, other_allocator<int>(5));
		tim::CircularBuffer<int, other_allocator<int> > l2(l, other_allocator<int>(3));
		l2 = l;
		assert(l2 == l);
		assert(l2.get_allocator() == other_allocator<int>(5));
	}
	{
		tim::CircularBuffer<int, min_allocator<int> > l(3, 2, min_allocator<int>());
		tim::CircularBuffer<int, min_allocator<int> > l2(l, min_allocator<int>());
		l2 = l;
		assert(l2 == l);
		assert(l2.get_allocator() == min_allocator<int>());
	}

	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	for(const auto& src_buf : get_test_buffers(test_allocator<Counter<int>>()))
	{
		for(const auto& dest_buf: get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto src = src_buf.exact_copy();
			auto dest = dest_buf.exact_copy();
			auto cap = src.capacity();
			auto sz = src.size();
			auto begin_idx = src.begin_index();
			auto end_idx = src.end_index();
			dest = src;
			dest._assert_invariants();
			src._assert_invariants();
			assert(dest == src);
			assert(dest.size() == sz);

			assert(src.begin_index() == begin_idx);
			assert(src.end_index() == end_idx);
			assert(src.capacity() == cap);
			assert(src.size() == sz);
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);

	return 0;
}
