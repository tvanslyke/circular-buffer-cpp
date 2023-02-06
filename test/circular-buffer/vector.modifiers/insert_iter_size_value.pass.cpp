//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// iterator insert(const_iterator position, size_type n, const value_type& x);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <cstddef>
#include <numeric>
#include <algorithm>

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
		tim::CircularBuffer<int>::iterator i = v.insert(v.cbegin() + 10, 5, 1);
		assert(v.size() == 105);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (; j < 15; ++j)
			assert(v[j] == 1);
		for (++j; j < 105; ++j)
			assert(v[j] == 0);
	}
	{
		tim::CircularBuffer<int> v(100);
		while(v.size() < v.capacity()) v.push_back(0); // force reallocation
		size_t sz = v.size();
		tim::CircularBuffer<int>::iterator i = v.insert(v.cbegin() + 10, 5, 1);
		assert(v.size() == sz + 5);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		std::size_t j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (; j < 15; ++j)
			assert(v[j] == 1);
		for (++j; j < v.size(); ++j)
			assert(v[j] == 0);
	}
	{
		tim::CircularBuffer<int> v(100);
		v.reserve(128); // force no reallocation
		size_t sz = v.size();
		tim::CircularBuffer<int>::iterator i = v.insert(v.cbegin() + 10, 5, 1);
		assert(v.size() == sz + 5);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		std::size_t j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (; j < 15; ++j)
			assert(v[j] == 1);
		for (++j; j < v.size(); ++j)
			assert(v[j] == 0);
	}
	{
		tim::CircularBuffer<int, limited_allocator<int, 300> > v(100);
		tim::CircularBuffer<int, limited_allocator<int, 300> >::iterator i = v.insert(v.cbegin() + 10, 5, 1);
		assert(v.size() == 105);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (; j < 15; ++j)
			assert(v[j] == 1);
		for (++j; j < 105; ++j)
			assert(v[j] == 0);
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v(100);
		tim::CircularBuffer<int, min_allocator<int>>::iterator i = v.insert(v.cbegin() + 10, 5, 1);
		assert(v.size() == 105);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (; j < 15; ++j)
			assert(v[j] == 1);
		for (++j; j < 105; ++j)
			assert(v[j] == 0);
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v(100);
		tim::CircularBuffer<int, min_allocator<int>>::iterator i = v.insert(v.cbegin() + 10, 5, 1);
		assert(v.size() == 105);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (; j < 15; ++j)
			assert(v[j] == 1);
		for (++j; j < 105; ++j)
			assert(v[j] == 0);
	}
	
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type i= 0; i != buf.size(); ++i)
			{
				for(size_type count = 0; count < (2u * buf.capacity() + 1); ++count)
				{
					auto b = buf.exact_copy();
					auto p = b.insert(b.begin()+i, count, Counter<int>(-1));
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::all_of(b.begin() + i, b.begin() + (i + count), [](const Counter<int>& c) { return c.get() == -1; }));
					assert(std::equal(b.begin() + (i + count), b.end(), buf.begin() + i, buf.end()));
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
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type i= 0; i != buf.size(); ++i)
			{
				for(size_type count = 0; count < (2u * buf.capacity() + 1); ++count)
				{
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + count >= b.capacity();
					auto p = b.insert_move_front(b.begin()+i, count, Counter<int>(-1));
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::all_of(b.begin() + i, b.begin() + (i + count), [](const Counter<int>& c) { return c.get() == -1; }));
					assert(std::equal(b.begin() + (i + count), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_begin_index = begin_idx < count ? (b.capacity() - (count - begin_idx)) : begin_idx - count;
						assert(b.begin_index() == expected_new_begin_index);
						assert(b.end_index() == end_idx);
					}
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
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type i= 0; i != buf.size(); ++i)
			{
				for(size_type count = 0; count < (2u * buf.capacity() + 1); ++count)
				{
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + count >= b.capacity();
					auto p = b.insert_move_back(b.begin()+i, count, Counter<int>(-1));
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::all_of(b.begin() + i, b.begin() + (i + count), [](const Counter<int>& c) { return c.get() == -1; }));
					assert(std::equal(b.begin() + (i + count), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_end_index = (end_idx + count) % b.capacity();
						assert(b.begin_index() == begin_idx); 
						assert(b.end_index() == expected_new_end_index);
					}
				}
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);


  return 0;
}
