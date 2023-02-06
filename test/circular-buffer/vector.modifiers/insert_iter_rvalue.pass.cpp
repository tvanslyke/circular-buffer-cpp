//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// iterator insert(const_iterator position, value_type&& x);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <numeric>

#include "test_macros.h"
#include "test_allocator.h"
#include "MoveOnly.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"

int main(int, char**)
{
	{
		tim::CircularBuffer<MoveOnly> v(100);
		tim::CircularBuffer<MoveOnly>::iterator i = v.insert(v.cbegin() + 10, MoveOnly(3));
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == MoveOnly());
		assert(v[j] == MoveOnly(3));
		for (++j; j < 101; ++j)
			assert(v[j] == MoveOnly());
	}
	{
		tim::CircularBuffer<MoveOnly, limited_allocator<MoveOnly, 300> > v(100);
		tim::CircularBuffer<MoveOnly, limited_allocator<MoveOnly, 300> >::iterator i = v.insert(v.cbegin() + 10, MoveOnly(3));
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == MoveOnly());
		assert(v[j] == MoveOnly(3));
		for (++j; j < 101; ++j)
			assert(v[j] == MoveOnly());
	}
	{
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly>> v(100);
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly>>::iterator i = v.insert(v.cbegin() + 10, MoveOnly(3));
		assert(v.size() == 101);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == MoveOnly());
		assert(v[j] == MoveOnly(3));
		for (++j; j < 101; ++j)
			assert(v[j] == MoveOnly());
	}
	assert(test_allocator<MoveOnly>::alloc_count == 0);
	{
		for(auto maker : get_test_buffer_makers(test_allocator<MoveOnly>()))
		{
			auto buf = maker();
			std::iota(buf.begin(), buf.end(), 0);
			for(int i = 0; i < (int)buf.size(); ++i)
			{
				auto b = maker();
				std::iota(b.begin(), b.end(), 0);
				assert(b == buf);
				auto pos = b.insert(b.begin() + i, MoveOnly(-1));
				assert(pos == b.begin() + i);
				assert(std::equal(buf.begin(), buf.begin() + i, b.begin(), b.begin() + i));
				assert(std::equal(buf.begin() + i, buf.end(), b.begin() + (i + 1), b.end()));
				assert(*pos == MoveOnly(-1));
			}
		}
	}
	assert(test_allocator<MoveOnly>::alloc_count == 0);
	{
		for(auto maker : get_test_buffer_makers(test_allocator<MoveOnly>()))
		{
			auto buf = maker();
			std::iota(buf.begin(), buf.end(), 0);
			for(int i = 0; i < (int)buf.size(); ++i)
			{
				auto b = maker();
				std::iota(b.begin(), b.end(), 0);
				assert(b == buf);
				auto begin_idx = b.begin_index();
				auto end_idx = b.end_index();
				bool will_reallocate = b.size() == b.capacity();
				auto pos = b.insert_move_front(b.begin() + i, MoveOnly(-1));
				assert(pos == b.begin() + i);
				assert(std::equal(buf.begin(), buf.begin() + i, b.begin(), b.begin() + i));
				assert(std::equal(buf.begin() + i, buf.end(), b.begin() + (i + 1), b.end()));
				assert(*pos == MoveOnly(-1));
				if(!will_reallocate)
				{
					auto expected_new_begin_index = begin_idx == 0 ? (b.capacity() - 1) : begin_idx - 1;
					assert(b.begin_index() == expected_new_begin_index);
					assert(b.end_index() == end_idx);
				}
			}
		}
	}
	assert(test_allocator<MoveOnly>::alloc_count == 0);
	{
		for(auto maker : get_test_buffer_makers(test_allocator<MoveOnly>()))
		{
			auto buf = maker();
			std::iota(buf.begin(), buf.end(), 0);
			for(int i = 0; i < (int)buf.size(); ++i)
			{
				auto b = maker();
				std::iota(b.begin(), b.end(), 0);
				assert(b == buf);
				auto begin_idx = b.begin_index();
				auto end_idx = b.end_index();
				bool will_reallocate = b.size() == b.capacity();
				auto pos = b.insert_move_back(b.begin() + i, MoveOnly(-1));
				assert(pos == b.begin() + i);
				assert(std::equal(buf.begin(), buf.begin() + i, b.begin(), b.begin() + i));
				assert(std::equal(buf.begin() + i, buf.end(), b.begin() + (i + 1), b.end()));
				assert(*pos == MoveOnly(-1));
				if(!will_reallocate)
				{
					auto expected_new_end_index = end_idx == b.capacity() - 1 ? 0: end_idx + 1;
					assert(b.begin_index() == begin_idx);
					assert(b.end_index() == expected_new_end_index);
				}
			}
		}
	}
	assert(test_allocator<MoveOnly>::alloc_count == 0);


  return 0;
}
