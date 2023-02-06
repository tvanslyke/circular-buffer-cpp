//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// class vector

// bool empty() const noexcept;

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "min_allocator.h"
#include "test_buffers.h"

int main(int, char**)
{
	{
		typedef tim::CircularBuffer<int> C;
		C c;
		ASSERT_NOEXCEPT(c.empty());
		assert(c.empty());
		c.push_back(C::value_type(1));
		assert(!c.empty());
		c.clear();
		assert(c.empty());
	}
	{
		typedef tim::CircularBuffer<int, min_allocator<int>> C;
		C c;
		ASSERT_NOEXCEPT(c.empty());
		assert(c.empty());
		c.push_back(C::value_type(1));
		assert(!c.empty());
		c.clear();
		assert(c.empty());
	}
	{ auto buf = get_empty_buffer(); assert(buf.empty()); }
	{ auto buf = get_empty_buffer_with_capacity(); assert(buf.empty()); }
	{ auto buf = get_unary_buffer(); assert(!buf.empty()); }
	{ auto buf = get_unary_buffer_with_back_slack(); assert(!buf.empty()); }
	{ auto buf = get_unary_buffer_with_front_slack(); assert(!buf.empty()); }
	{ auto buf = get_simple_wrapped_buffer(); assert(!buf.empty()); }
	{ auto buf = get_simple_buffer_with_front_and_back_slack(); assert(!buf.empty()); }
	{ auto buf = get_large_wrapped_buffer(); assert(!buf.empty()); }
	{ auto buf = get_large_full_buffer(); assert(!buf.empty()); }
	{ auto buf = get_large_full_wrapped_buffer(); assert(!buf.empty()); }

  return 0;
}
