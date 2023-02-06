//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void swap(vector& x);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <algorithm>
#include <numeric>

#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_allocator.h"
#include "test_buffers.h"
#include "Counter.h"

int main(int, char**)
{
	{
		tim::CircularBuffer<int> v1(100);
		tim::CircularBuffer<int> v2(200);
		assert(is_contiguous_container_asan_correct(v1));
		assert(is_contiguous_container_asan_correct(v2));
		v1.swap(v2);
		assert(v1.size() == 200);
		assert(v1.capacity() == 200);
		assert(is_contiguous_container_asan_correct(v1));
		assert(v2.size() == 100);
		assert(v2.capacity() == 100);
		assert(is_contiguous_container_asan_correct(v2));
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v1(100);
		tim::CircularBuffer<int, min_allocator<int>> v2(200);
		assert(is_contiguous_container_asan_correct(v1));
		assert(is_contiguous_container_asan_correct(v2));
		v1.swap(v2);
		assert(v1.size() == 200);
		assert(v1.capacity() == 200);
		assert(is_contiguous_container_asan_correct(v1));
		assert(v2.size() == 100);
		assert(v2.capacity() == 100);
		assert(is_contiguous_container_asan_correct(v2));
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_lhs : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto lhs = test_lhs.exact_copy();
			std::iota(lhs.begin(), lhs.end(), 0);
			for(const auto& test_rhs: get_test_buffers(test_allocator<Counter<int>>()))
			{
				auto rhs = test_rhs.exact_copy();
				std::iota(rhs.begin(), rhs.end(), (int)lhs.size());
				for(int i = 0; i != (int)lhs.size(); ++i)
				{
					{
						auto l = lhs.exact_copy();
						auto r = rhs.exact_copy();
						auto allocs_before = test_allocator<Counter<int>>::alloc_count;
						auto constructs_before = Counter<int>::gConstructed;
						l.swap(r);
						assert(r == lhs);
						assert(r.capacity() == lhs.capacity());
						assert(r.begin_index() == lhs.begin_index());
						assert(r.end_index() == lhs.end_index());
						assert(l == rhs);
						assert(l.capacity() == rhs.capacity());
						assert(l.begin_index() == rhs.begin_index());
						assert(l.end_index() == rhs.end_index());
						assert(allocs_before == test_allocator<Counter<int>>::alloc_count);
						assert(constructs_before == Counter<int>::gConstructed);
					}
					{
						using std::swap;
						auto l = lhs.exact_copy();
						auto r = rhs.exact_copy();
						auto allocs_before = test_allocator<Counter<int>>::alloc_count;
						auto constructs_before = Counter<int>::gConstructed;
						swap(l, r);
						assert(r == lhs);
						assert(r.capacity() == lhs.capacity());
						assert(r.begin_index() == lhs.begin_index());
						assert(r.end_index() == lhs.end_index());
						assert(l == rhs);
						assert(l.capacity() == rhs.capacity());
						assert(l.begin_index() == rhs.begin_index());
						assert(l.end_index() == rhs.end_index());
						assert(allocs_before == test_allocator<Counter<int>>::alloc_count);
						assert(constructs_before == Counter<int>::gConstructed);
					}
				}
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);


	return 0;
}
