//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void pop_back();

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <algorithm>
#include <numeric>

#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "test_buffers.h"
#include "Counter.h"


int main(int, char**)
{
	{
		tim::CircularBuffer<int> c;
		c.push_back(1);
		assert(c.size() == 1);
		c.pop_back();
		assert(c.size() == 0);

	}
	{
		tim::CircularBuffer<int, min_allocator<int>> c;
		c.push_back(1);
		assert(c.size() == 1);
		c.pop_back();
		assert(c.size() == 0);
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> c;
		c = {0,1,2,3,4,5,6,7,8,9};
	assert(c.size() == 10);
	c.pop_back();
	assert(c.size() == 9);
	c.pop_back();
	assert(c.size() == 8);
	c.pop_back_n(2);
	assert(c.size() == 6);
	assert(c[5] == 5);
	c.pop_back_n(5);
	assert(c.size() == 1);
	assert(c[0] == 0);
	c.push_back(1);
	c.pop_back_n(2);
	assert(c.size() == 0);
	}
	{ // LWG 526
		int arr[] = {0, 1, 2, 3, 4};
		int sz = 5;
		tim::CircularBuffer<int> c(arr, arr+sz);
		while (c.size() < c.capacity())
			c.push_back(sz++);
		c.push_back(c.front());
		assert(c.back() == 0);
		for (int i = 0; i < sz; ++i)
			assert(c[i] == i);
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf: get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			auto b = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			std::iota(b.begin(), b.end(), 0);
			while(!b.empty())
			{
				auto size_before = b.size();
				b.pop_back();
				auto size_after = b.size();
				assert(size_after == size_before - 1);
				assert(std::equal(b.begin(), b.end(), buf.begin(), buf.begin() + b.size()));
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);

	return 0;
}
