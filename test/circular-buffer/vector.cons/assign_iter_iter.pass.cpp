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
#include <numeric>
#include <cassert>
#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_iterators.h"
#include "emplace_constructible.h"
#include "container_test_types.h"
#include "Counter.h"
#include "test_allocator.h"
#include "test_buffers.h"

void test_emplaceable_concept() {
	int arr1[] = {42};
	int arr2[] = {1, 101, 42};
	{
		using T = EmplaceConstructibleMoveableAndAssignable<int>;
		using It = forward_iterator<int*>;
		{
			tim::CircularBuffer<T> v;
			v.assign(It(arr1), It(std::end(arr1)));
			assert(v[0].value == 42);
		}
		{
			tim::CircularBuffer<T> v;
			v.assign(It(arr2), It(std::end(arr2)));
			assert(v[0].value == 1);
			assert(v[1].value == 101);
			assert(v[2].value == 42);
		}
	}
	{
		using T = EmplaceConstructibleMoveableAndAssignable<int>;
		using It = input_iterator<int*>;
		{
			tim::CircularBuffer<T> v;
			v.assign(It(arr1), It(std::end(arr1)));
			assert(v[0].copied == 0);
			assert(v[0].value == 42);
		}
		{
			tim::CircularBuffer<T> v;
			v.assign(It(arr2), It(std::end(arr2)));
			//assert(v[0].copied == 0);
			assert(v[0].value == 1);
			//assert(v[1].copied == 0);
			assert(v[1].value == 101);
			assert(v[2].copied == 0);
			assert(v[2].value == 42);
		}
	}
}



int main(int, char**)
{
	test_emplaceable_concept();

	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& src_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			for(const auto& dest_buf: get_test_buffers(test_allocator<Counter<int>>()))
			{
				auto src = src_buf.exact_copy();
				auto dest = dest_buf.exact_copy();
				std::iota(src.begin(), src.end(), 0);
				std::iota(dest.begin(), dest.end(), (int)src.size());
				dest.assign(src.begin(), src.end());
				assert(dest == src);
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& dest_buf: get_test_buffers(test_allocator<Counter<int>>()))
		{
			for(int n = 0; n < 2 * (int)dest_buf.capacity() + 1; ++n)
			{
				auto dest = dest_buf.exact_copy();
				std::vector<Counter<int>> src(n);
				std::iota(src.begin(), src.end(), 0);
				std::iota(dest.begin(), dest.end(), (int)src.size());
				dest.assign(src.begin(), src.end());
				assert(std::equal(src.begin(), src.end(), dest.begin(), dest.end()));
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	return 0;
}
