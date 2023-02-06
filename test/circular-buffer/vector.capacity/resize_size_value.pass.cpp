//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void resize(size_type sz, const value_type& x);

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
		v.resize(50, 1);
		assert(v.size() == 50);
		assert(v.capacity() == 100);
		assert(v == tim::CircularBuffer<int>(50));
		v.resize(200, 1);
		assert(v.size() == 200);
		assert(v.capacity() >= 200);
		assert(is_contiguous_container_asan_correct(v));
		for (unsigned i = 0; i < 50; ++i)
			assert(v[i] == 0);
		for (unsigned i = 50; i < 200; ++i)
			assert(v[i] == 1);
	}
	{
		// Add 1 for implementations that dynamically allocate a container proxy.
		tim::CircularBuffer<int, limited_allocator<int, 300 + 1> > v(100);
		v.resize(50, 1);
		assert(v.size() == 50);
		assert(v.capacity() == 100);
		v.resize(200, 1);
		assert(v.size() == 200);
		assert(v.capacity() >= 200);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v(100);
		v.resize(50, 1);
		assert(v.size() == 50);
		assert(v.capacity() == 100);
		assert(is_contiguous_container_asan_correct(v));
		assert((v == tim::CircularBuffer<int, min_allocator<int>>(50)));
		v.resize(200, 1);
		assert(v.size() == 200);
		assert(v.capacity() >= 200);
		assert(is_contiguous_container_asan_correct(v));
		for (unsigned i = 0; i < 50; ++i)
			assert(v[i] == 0);
		for (unsigned i = 50; i < 200; ++i)
			assert(v[i] == 1);
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v(100);
		v.resize(50, 1);
		assert(v.size() == 50);
		assert(v.capacity() == 100);
		assert(is_contiguous_container_asan_correct(v));
		v.resize(200, 1);
		assert(v.size() == 200);
		assert(v.capacity() >= 200);
		assert(is_contiguous_container_asan_correct(v));
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type n = 0; n != buf.size(); ++n)
			{
				auto b = buf.exact_copy();
				auto begin_idx = b.begin_index();
				auto cap = b.capacity();
				b.resize(n, Counter<int>(-1));
				assert(b.size() == n);
				assert(std::equal(b.begin(), b.end(), buf.begin(), buf.begin() + n));
				assert(begin_idx == b.begin_index());
				assert(cap == b.capacity());
				assert(b.end_index() == ((begin_idx + n) % cap));
			}
			for(size_type n = buf.size(); n != (buf.capacity() + 1); ++n)
			{
				auto b = buf.exact_copy();
				auto begin_idx = b.begin_index();
				auto cap = b.capacity();
				b.resize(n, Counter<int>(-1));
				assert(b.size() == n);
				assert(std::equal(b.begin(), b.begin() + buf.size(), buf.begin(), buf.end()));
				assert(std::all_of(b.begin() + buf.size(), b.end(), [](const auto& c) { return c == Counter<int>(-1); }));
				assert(begin_idx == b.begin_index());
				assert(cap == b.capacity());
				assert(cap == 0u || b.end_index() == ((begin_idx + n) % cap));
			}
			for(size_type n = buf.capacity() + 1; n != buf.capacity() * 2 + 1; ++n)
			{
				auto b = buf.exact_copy();
				auto cap = b.capacity();
				b.resize(n, Counter<int>(-1));
				assert(b.size() == n);
				assert(std::equal(b.begin(), b.begin() + buf.size(), buf.begin(), buf.end()));
				assert(std::all_of(b.begin() + buf.size(), b.end(), [](const auto& c) { return c == Counter<int>(-1); }));
				assert(b.capacity() > cap);
				assert(b.capacity() >= b.size());
			}
		}
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type n = 0; n != buf.size(); ++n)
			{
				auto b = buf.exact_copy();
				auto begin_idx = b.begin_index();
				auto end_idx = b.end_index();
				auto cap = b.capacity();
				b.resize_front(n, Counter<int>(-1));
				auto count_removed = buf.size() - n;
				assert(b.size() == n);
				assert(std::equal(b.begin(), b.end(), buf.begin() + count_removed, buf.end()));
				assert(b.begin_index() == (begin_idx + count_removed) % cap);
				assert(cap == b.capacity());
				assert(b.begin_index() == ((begin_idx + count_removed) % cap));
				assert(b.end_index() == end_idx);
			}
			for(size_type n = buf.size(); n != (buf.capacity() + 1); ++n)
			{
				auto b = buf.exact_copy();
				auto begin_idx = b.begin_index();
				auto end_idx = b.end_index();
				auto cap = b.capacity();
				b.resize_front(n, Counter<int>(-1));
				auto count_added = n - buf.size();
				assert(b.size() == n);
				assert(std::equal(b.begin() + count_added, b.end(), buf.begin(), buf.end()));
				assert(std::all_of(b.begin(), b.begin() + count_added, [](const auto& c) { return c == Counter<int>(-1); }));
				if(begin_idx >= count_added)
				{
					assert(b.begin_index() == begin_idx - count_added);
				}
				else
				{
					assert(b.begin_index() == (cap - (count_added - begin_idx))); 
				}
				assert(cap == b.capacity());
				assert(b.end_index() == end_idx);
			}
			for(size_type n = buf.capacity() + 1; n != buf.capacity() * 2 + 1; ++n)
			{
				auto b = buf.exact_copy();
				auto cap = b.capacity();
				b.resize_front(n, Counter<int>(-1));
				assert(b.size() == n);
				assert(std::equal(b.end() - buf.size(), b.end(), buf.begin(), buf.end()));
				assert(std::all_of(b.begin(), b.end() - buf.size(), [](const auto& c) { return c == Counter<int>(-1); }));
				assert(b.capacity() > cap);
				assert(b.capacity() >= b.size());
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	return 0;
}
