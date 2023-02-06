//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void reserve(size_type n);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"
#include "Counter.h"

int main(int, char**)
{
	{
		tim::CircularBuffer<int> v;
		v.reserve(10);
		assert(v.capacity() >= 10);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int> v(100);
		assert(v.capacity() == 100);
		v.reserve(50);
		assert(v.size() == 100);
		assert(v.capacity() == 100);
		v.reserve(150);
		assert(v.size() == 100);
		assert(v.capacity() == 150);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		// Add 1 for implementations that dynamically allocate a container proxy.
		tim::CircularBuffer<int, limited_allocator<int, 250 + 1> > v(100);
		assert(v.capacity() == 100);
		v.reserve(50);
		assert(v.size() == 100);
		assert(v.capacity() == 100);
		v.reserve(150);
		assert(v.size() == 100);
		assert(v.capacity() == 150);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v;
		v.reserve(10);
		assert(v.capacity() >= 10);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v(100);
		assert(v.capacity() == 100);
		v.reserve(50);
		assert(v.size() == 100);
		assert(v.capacity() == 100);
		v.reserve(150);
		assert(v.size() == 100);
		assert(v.capacity() == 150);
		assert(is_contiguous_container_asan_correct(v));
	}
	using alloc = test_allocator<Counter<int>>;
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		auto buf = get_empty_buffer(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_empty_buffer_with_capacity(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_unary_buffer(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_unary_buffer_with_back_slack(alloc()); assert(buf.capacity() > buf.size());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_unary_buffer_with_front_slack(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_simple_wrapped_buffer(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_simple_buffer_with_front_and_back_slack(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_large_wrapped_buffer(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_large_full_buffer(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	{
		auto buf = get_large_full_wrapped_buffer(alloc());
		assert(buf.capacity() >= buf.size());
		auto b = buf.exact_copy();
		b.reserve(buf.capacity());
		assert(b == buf);
		assert(b.size() == buf.size());
		assert(b.capacity() == buf.capacity());
		assert(b.begin_index() == buf.begin_index());
		assert(b.end_index() == buf.end_index());
		b.reserve(buf.capacity() + 1);
		assert(b.capacity() >= buf.capacity() + 1);
		assert(b == buf);
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);

  return 0;
}
