//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// vector& operator=(const vector& c);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"

int main(int, char**)
{
    {
        tim::CircularBuffer<int, test_allocator<int> > l(3, 2, test_allocator<int>(5));
        tim::CircularBuffer<int, test_allocator<int> > l2(l, test_allocator<int>(3));
        l2 = l;
        assert(l2 == l);
        assert(l2.get_allocator() == test_allocator<int>(3));
    }
    {
        tim::CircularBuffer<int, other_allocator<int> > l(3, 2, other_allocator<int>(5));
        tim::CircularBuffer<int, other_allocator<int> > l2(l, other_allocator<int>(3));
        l2 = l;
        assert(l2 == l);
        assert(l2.get_allocator() == other_allocator<int>(5));
    }
#if TEST_STD_VER >= 11
    {
        tim::CircularBuffer<int, min_allocator<int> > l(3, 2, min_allocator<int>());
        tim::CircularBuffer<int, min_allocator<int> > l2(l, min_allocator<int>());
        l2 = l;
        assert(l2 == l);
        assert(l2.get_allocator() == min_allocator<int>());
    }
#endif

  return 0;
}
