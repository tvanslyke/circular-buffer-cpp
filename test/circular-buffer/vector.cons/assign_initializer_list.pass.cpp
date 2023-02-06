//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// void assign(initializer_list<value_type> il);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"
#include "test_allocator.h"
#include "Counter.h"

template <typename Vec>
void test ( Vec &v )
{
	v.assign({3, 4, 5, 6});
	assert(v.size() == 4);
	assert(is_contiguous_container_asan_correct(v));
	assert(v[0] == 3);
	assert(v[1] == 4);
	assert(v[2] == 5);
	assert(v[3] == 6);
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
	for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
	{
		{
			auto buf = test_buf.exact_copy();
			buf = std::initializer_list<Counter<int>>{};
			assert(buf.empty());
		}
		{
			auto buf = test_buf.exact_copy();
			buf = std::initializer_list<Counter<int>>{ {-1} };
			assert(buf.size() == 1);
			buf.front() == Counter<int>(-1);
		}
		{
			auto buf = test_buf.exact_copy();
			buf = std::initializer_list<Counter<int>>{ {-1}, {-2} };
			assert(buf.size() == 2);
			buf.front() == Counter<int>(-1);
			buf.back() == Counter<int>(-2);
		}
		{
			auto buf = test_buf.exact_copy();
			buf = std::initializer_list<Counter<int>>{ {0}, {-1}, {-2}, {-3}, {-4}, {-5}, {-6}, {-7}, {-8}, {-9} };
			assert(buf.size() == 10);
			for(int i = 0; i < 10; ++i) { assert(Counter<int>(-i) == buf[i]); }
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	return 0;
}
