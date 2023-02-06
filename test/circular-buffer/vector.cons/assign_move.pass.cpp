//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// vector& operator=(vector&& c);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include "test_macros.h"
#include "MoveOnly.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"
#include "Counter.h"

int main(int, char**)
{
	assert(test_allocator<MoveOnly>::alloc_count == 0);
	{
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l(test_allocator<MoveOnly>(5));
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > lo(test_allocator<MoveOnly>(5));
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l2(test_allocator<MoveOnly>(5));
		l2 = std::move(l);
		assert(l2 == lo);
		assert(l.empty());
		assert(l2.get_allocator() == lo.get_allocator());
		assert(is_contiguous_container_asan_correct(l2));
	}
	assert(test_allocator<MoveOnly>::alloc_count == 0);
	{
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l(test_allocator<MoveOnly>(5));
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > lo(test_allocator<MoveOnly>(5));
		assert(test_allocator<MoveOnly>::alloc_count == 0);
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		auto alloc_before = test_allocator<MoveOnly>::alloc_count;
		tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly> > l2(test_allocator<MoveOnly>(6));
		l2 = std::move(l);
		assert(test_allocator<MoveOnly>::alloc_count == (alloc_before + 1));
		assert(l2 == lo);
		assert(!l.empty());
		assert(l2.get_allocator() == test_allocator<MoveOnly>(6));
		assert(is_contiguous_container_asan_correct(l2));
	}
	assert(test_allocator<MoveOnly>::alloc_count == 0);
	{
		tim::CircularBuffer<MoveOnly, other_allocator<MoveOnly> > l(other_allocator<MoveOnly>(5));
		tim::CircularBuffer<MoveOnly, other_allocator<MoveOnly> > lo(other_allocator<MoveOnly>(5));
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		tim::CircularBuffer<MoveOnly, other_allocator<MoveOnly> > l2(other_allocator<MoveOnly>(6));
		l2 = std::move(l);
		assert(l2 == lo);
		assert(l.empty());
		assert(l2.get_allocator() == lo.get_allocator());
		assert(is_contiguous_container_asan_correct(l2));
	}
	assert(test_allocator<MoveOnly>::alloc_count == 0);
	{
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly> > l(min_allocator<MoveOnly>{});
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly> > lo(min_allocator<MoveOnly>{});
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		for (int i = 1; i <= 3; ++i)
		{
			l.push_back(i);
			lo.push_back(i);
		}
		assert(is_contiguous_container_asan_correct(l));
		assert(is_contiguous_container_asan_correct(lo));
		tim::CircularBuffer<MoveOnly, min_allocator<MoveOnly> > l2(min_allocator<MoveOnly>{});
		l2 = std::move(l);
		assert(l2 == lo);
		assert(l.empty());
		assert(l2.get_allocator() == lo.get_allocator());
		assert(is_contiguous_container_asan_correct(l2));
	}
	assert(test_allocator<MoveOnly>::alloc_count == 0);
	
	assert(test_allocator<int>::alloc_count == 0);
	for(auto buf : get_test_buffers())
	{
		tim::CircularBuffer<int> dest = {1,2,3};
		auto cap = buf.capacity();
		auto sz = buf.size();
		auto begin_idx = buf.begin_index();
		auto end_idx = buf.end_index();
		auto cpy = buf;
		dest = std::move(buf);
		dest._assert_invariants();
		buf._assert_invariants();
		assert(dest == cpy);
		assert(dest.capacity() == cap);
		assert(dest.size() == sz);
		assert(dest.begin_index() == begin_idx);
		assert(dest.end_index() == end_idx);
	}
	assert(test_allocator<int>::alloc_count == 0);
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
	{
		for(const auto& dest_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			auto dest = dest_buf.exact_copy();
			auto cap = buf.capacity();
			auto sz = buf.size();
			auto begin_idx = buf.begin_index();
			auto end_idx = buf.end_index();
			tim::CircularBuffer<Counter<int>, test_allocator<Counter<int>>> src(tim::tags::preserve_layout, buf);
			assert(src == buf);
			dest = std::move(src);
			dest._assert_invariants();
			buf._assert_invariants();
			src._assert_invariants();
			assert(dest == buf);
			assert(dest.capacity() == cap);
			assert(dest.size() == sz);
			assert(dest.begin_index() == begin_idx);
			assert(dest.end_index() == end_idx);
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	return 0;
}
