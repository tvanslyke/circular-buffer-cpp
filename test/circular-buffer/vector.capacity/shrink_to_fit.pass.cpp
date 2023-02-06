//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void shrink_to_fit();

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <algorithm>
#include <numeric>

#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"
#include "Counter.h"

int main(int, char**)
{
	{
		tim::CircularBuffer<int> v(100);
		v.push_back(1);
		assert(is_contiguous_container_asan_correct(v));
		v.shrink_to_fit();
		assert(v.capacity() == 101);
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int, limited_allocator<int, 401> > v(100);
		v.push_back(1);
		assert(is_contiguous_container_asan_correct(v));
		v.shrink_to_fit();
		assert(v.capacity() == 101);
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int, limited_allocator<int, 400> > v(100);
		v.push_back(1);
		assert(is_contiguous_container_asan_correct(v));
		v.shrink_to_fit();
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v(100);
		v.push_back(1);
		assert(is_contiguous_container_asan_correct(v));
		v.shrink_to_fit();
		assert(v.capacity() == 101);
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			{
				auto buf = test_buf.exact_copy();
				buf.shrink_to_fit();
				assert(buf == test_buf);
				assert(buf.capacity() == buf.size());
			}
			{
				auto buf = test_buf.exact_copy();
				buf.reserve(buf.capacity() + 1);
				buf.shrink_to_fit();
				assert(buf == test_buf);
				assert(buf.capacity() == buf.size());
			}
			{
				auto buf = test_buf.exact_copy();
				buf.reserve(2 * buf.capacity() + 1);
				buf.shrink_to_fit();
				assert(buf == test_buf);
				assert(buf.capacity() == buf.size());
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);  

	return 0;
}
