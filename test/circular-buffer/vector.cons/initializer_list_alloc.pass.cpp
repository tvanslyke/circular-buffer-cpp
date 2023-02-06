//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// vector(initializer_list<value_type> il, const Allocator& a = allocator_type());

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "Counter.h"

int main(int, char**)
{
	{
		tim::CircularBuffer<int, test_allocator<int>> d({3, 4, 5, 6}, test_allocator<int>(3));
		assert(d.get_allocator() == test_allocator<int>(3));
		assert(d.size() == 4);
		assert(d[0] == 3);
		assert(d[1] == 4);
		assert(d[2] == 5);
		assert(d[3] == 6);
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> d({3, 4, 5, 6}, min_allocator<int>());
		assert(d.get_allocator() == min_allocator<int>());
		assert(d.size() == 4);
		assert(d[0] == 3);
		assert(d[1] == 4);
		assert(d[2] == 5);
		assert(d[3] == 6);
	}
	
	
	{
		tim::CircularBuffer<Counter<int>, test_allocator<Counter<int>>> d(std::initializer_list<Counter<int>>(), test_allocator<Counter<int>>(3));
		assert(d.size() == 0);
		assert(d.get_allocator() == test_allocator<Counter<int>>(3));
	}	
	{
		tim::CircularBuffer<Counter<int>, test_allocator<Counter<int>>> d({Counter<int>(3)}, test_allocator<Counter<int>>(7));
		assert(d.size() == 1);
		assert(d[0] == Counter<int>(3));
		assert(d.get_allocator() == test_allocator<Counter<int>>(7));
	}
	{
		tim::CircularBuffer<Counter<int>, test_allocator<Counter<int>>> d({3, 4, 5, 6}, test_allocator<Counter<int>>(100));
		assert(d.size() == 4);
		assert(d[0] == 3);
		assert(d[1] == 4);
		assert(d[2] == 5);
		assert(d[3] == 6);
		assert(d.get_allocator() == test_allocator<Counter<int>>(100));
	}


	return 0;
}
