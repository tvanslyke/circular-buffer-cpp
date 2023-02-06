//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void assign(size_type n, const_reference v);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <algorithm>
#include <cassert>

#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_allocator.h"
#include "Counter.h"
#include "test_buffers.h"

bool is6(int x) { return x == 6; }

template <typename Vec>
void test ( Vec &v )
{
	v.assign(5, 6);
	assert(v.size() == 5);
	assert(std::all_of(v.begin(), v.end(), is6));
}

int main(int, char**)
{
	{
		typedef tim::CircularBuffer<int> V;
		V d1;
		V d2;
		d2.reserve(10);  // no reallocation during assign.
		test(d1);
		test(d2);
	}

	{
		typedef tim::CircularBuffer<int, min_allocator<int>> V;
		V d1;
		V d2;
		d2.reserve(10);  // no reallocation during assign.
		test(d1);
		test(d2);
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			for(int n = 0; n < 2 * (int)test_buf.capacity() + 1; ++n)
			{
				test_buf._assert_invariants();
				auto buf = test_buf.exact_copy();
				buf._assert_invariants();
				buf.assign(n, Counter<int>(-1));
				test_buf._assert_invariants();
				buf._assert_invariants();
				assert((int)buf.size() == n);
				assert(std::all_of(buf.begin(), buf.end(), [](const auto& c) { return c == Counter<int>(-1); }));
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	return 0;
}
