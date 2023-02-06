//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// size_type capacity() const;

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"

int main(int, char**)
{
	{
		tim::CircularBuffer<int> v;
		assert(v.capacity() == 0);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int> v(100);
		assert(v.capacity() == 100);
		v.push_back(0);
		assert(v.capacity() > 101);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v;
		assert(v.capacity() == 0);
		assert(is_contiguous_container_asan_correct(v));
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> v(100);
		assert(v.capacity() == 100);
		v.push_back(0);
		assert(v.capacity() > 101);
		assert(is_contiguous_container_asan_correct(v));
	}
	{ auto buf = get_empty_buffer(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_empty_buffer_with_capacity(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_unary_buffer(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_unary_buffer_with_back_slack(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_unary_buffer_with_front_slack(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_simple_wrapped_buffer(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_simple_buffer_with_front_and_back_slack(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_large_wrapped_buffer(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_large_full_buffer(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }
	{ auto buf = get_large_full_wrapped_buffer(); assert(buf.max_size() > 0u); assert(buf.size() <= buf.max_size()); }

  return 0;
}
