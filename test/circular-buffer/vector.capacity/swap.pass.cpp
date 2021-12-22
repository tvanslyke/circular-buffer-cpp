//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void swap(vector& x);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"

int main(int, char**)
{
    {
        tim::CircularBuffer<int> v1(100);
        tim::CircularBuffer<int> v2(200);
        assert(is_contiguous_container_asan_correct(v1));
        assert(is_contiguous_container_asan_correct(v2));
        v1.swap(v2);
        assert(v1.size() == 200);
        assert(v1.capacity() == 200);
        assert(is_contiguous_container_asan_correct(v1));
        assert(v2.size() == 100);
        assert(v2.capacity() == 100);
        assert(is_contiguous_container_asan_correct(v2));
    }
#if TEST_STD_VER >= 11
    {
        tim::CircularBuffer<int, min_allocator<int>> v1(100);
        tim::CircularBuffer<int, min_allocator<int>> v2(200);
        assert(is_contiguous_container_asan_correct(v1));
        assert(is_contiguous_container_asan_correct(v2));
        v1.swap(v2);
        assert(v1.size() == 200);
        assert(v1.capacity() == 200);
        assert(is_contiguous_container_asan_correct(v1));
        assert(v2.size() == 100);
        assert(v2.capacity() == 100);
        assert(is_contiguous_container_asan_correct(v2));
    }
#endif

  return 0;
}
