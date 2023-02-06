//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void clear() noexcept;

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"
#include "test_allocator.h"
#include "Counter.h"

int main(int, char**)
{
	{
		int a[] = {1, 2, 3};
		tim::CircularBuffer<int> c(a, a+3);
		ASSERT_NOEXCEPT(c.clear());
		c.clear();
		assert(c.empty());
		c._assert_invariants();
	}
	{
		int a[] = {1, 2, 3};
		tim::CircularBuffer<int, min_allocator<int>> c(a, a+3);
		ASSERT_NOEXCEPT(c.clear());
		c.clear();
		assert(c.empty());
		c._assert_invariants();
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf: get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			buf.clear();
			assert(buf.empty());
			buf._assert_invariants();
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);


	return 0;
}
