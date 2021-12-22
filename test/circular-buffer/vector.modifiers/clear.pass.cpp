//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void clear() noexcept;

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>

#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"

int main(int, char**)
{
    {
    int a[] = {1, 2, 3};
    tim::CircularBuffer<int> c(a, a+3);
    ASSERT_NOEXCEPT(c.clear());
    c.clear();
    assert(c.empty());
    LIBCPP_ASSERT(c.__invariants());
    LIBCPP_ASSERT(is_contiguous_container_asan_correct(c));
    }
#if TEST_STD_VER >= 11
    {
    int a[] = {1, 2, 3};
    tim::CircularBuffer<int, min_allocator<int>> c(a, a+3);
    ASSERT_NOEXCEPT(c.clear());
    c.clear();
    assert(c.empty());
    LIBCPP_ASSERT(c.__invariants());
    LIBCPP_ASSERT(is_contiguous_container_asan_correct(c));
    }
#endif

  return 0;
}
