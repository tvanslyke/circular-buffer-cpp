//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// vector(vector&&)
//		noexcept(is_nothrow_move_constructible<allocator_type>::value);

// This tests a conforming extension

// UNSUPPORTED: c++98, c++03

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "MoveOnly.h"
#include "test_allocator.h"

template <class T>
struct some_alloc
{
	typedef T value_type;
	some_alloc(const some_alloc&);
};

int main(int, char**)
{
	{
		typedef tim::CircularBuffer<MoveOnly> C;
		static_assert(std::is_nothrow_move_constructible<C>::value, "");
	}
	{
		typedef tim::CircularBuffer<MoveOnly, test_allocator<MoveOnly>> C;
		static_assert(std::is_nothrow_move_constructible<C>::value, "");
	}
	{
		typedef tim::CircularBuffer<MoveOnly, other_allocator<MoveOnly>> C;
		static_assert(std::is_nothrow_move_constructible<C>::value, "");
	}
	{
		typedef tim::CircularBuffer<MoveOnly, some_alloc<MoveOnly>> C;
		//  In C++17, move constructors for allocators are not allowed to throw
		static_assert( std::is_nothrow_move_constructible<C>::value, "");
	}

  return 0;
}
