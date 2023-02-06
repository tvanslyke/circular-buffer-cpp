//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// iterator insert(const_iterator position, const value_type& x);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <cstddef>
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
		tim::CircularBuffer<int>::iterator i = v.insert(v.cbegin() + 10, 1);
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		assert(v[j] == 1);
		for (++j; j < 101; ++j)
			assert(v[j] == 0);
	}
	{
		tim::CircularBuffer<int> v(100);
		while(v.size() < v.capacity()) v.push_back(0); // force reallocation
		size_t sz = v.size();
		tim::CircularBuffer<int>::iterator i = v.insert(v.cbegin() + 10, 1);
		assert(v.size() == sz + 1);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		std::size_t j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		assert(v[j] == 1);
		for (++j; j < v.size(); ++j)
			assert(v[j] == 0);
	}
	{
		tim::CircularBuffer<int> v(100);
		while(v.size() < v.capacity()) v.push_back(0);
		v.pop_back(); v.pop_back(); // force no reallocation
		size_t sz = v.size();
		tim::CircularBuffer<int>::iterator i = v.insert(v.cbegin() + 10, 1);
		assert(v.size() == sz + 1);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		std::size_t j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		assert(v[j] == 1);
		for (++j; j < v.size(); ++j)
			assert(v[j] == 0);
	}
	{
		tim::CircularBuffer<int, limited_allocator<int, 300> > v(100);
		tim::CircularBuffer<int, limited_allocator<int, 300> >::iterator i = v.insert(v.cbegin() + 10, 1);
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		assert(v[j] == 1);
		for (++j; j < 101; ++j)
			assert(v[j] == 0);
	}
#if TEST_STD_VER >= 11
	{
		tim::CircularBuffer<int, min_allocator<int>> v(100);
		tim::CircularBuffer<int, min_allocator<int>>::iterator i = v.insert(v.cbegin() + 10, 1);
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		assert(v[j] == 1);
		for (++j; j < 101; ++j)
			assert(v[j] == 0);
	}
#endif
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			for(int i = 0; i < (int)buf.size(); ++i)
			{
				auto b = buf.exact_copy();
				auto pos = b.insert(b.begin() + i, Counter<int>(-1));
				assert(pos == b.begin() + i);
				assert(std::equal(buf.begin(), buf.begin() + i, b.begin(), b.begin() + i));
				assert(std::equal(buf.begin() + i, buf.end(), b.begin() + (i + 1), b.end()));
				assert(*pos == Counter<int>(-1));
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			for(int i = 0; i < (int)buf.size(); ++i)
			{
				auto b = buf.exact_copy();
				auto begin_idx = b.begin_index();
				auto end_idx = b.end_index();
				bool will_reallocate = b.size() == b.capacity();
				auto pos = b.insert_move_front(b.begin() + i, Counter<int>(-1));
				assert(pos == b.begin() + i);
				assert(std::equal(buf.begin(), buf.begin() + i, b.begin(), b.begin() + i));
				assert(std::equal(buf.begin() + i, buf.end(), b.begin() + (i + 1), b.end()));
				assert(*pos == Counter<int>(-1));
				if(!will_reallocate)
				{
					auto expected_new_begin_index = begin_idx == 0 ? (b.capacity() - 1) : begin_idx - 1;
					assert(b.begin_index() == expected_new_begin_index);
					assert(b.end_index() == end_idx);
				}
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			for(int i = 0; i < (int)buf.size(); ++i)
			{
				auto b = buf.exact_copy();
				auto begin_idx = b.begin_index();
				auto end_idx = b.end_index();
				bool will_reallocate = b.size() == b.capacity();
				auto pos = b.insert_move_back(b.begin() + i, Counter<int>(-1));
				assert(pos == b.begin() + i);
				assert(std::equal(buf.begin(), buf.begin() + i, b.begin(), b.begin() + i));
				assert(std::equal(buf.begin() + i, buf.end(), b.begin() + (i + 1), b.end()));
				assert(*pos == Counter<int>(-1));
				if(!will_reallocate)
				{
					auto expected_new_end_index = end_idx == b.capacity() - 1 ? 0: end_idx + 1;
					assert(b.begin_index() == begin_idx);
					assert(b.end_index() == expected_new_end_index);
				}
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);

	return 0;
}
