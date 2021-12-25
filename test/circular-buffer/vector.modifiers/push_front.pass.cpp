//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// void push_front(const value_type& x);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <cstddef>
#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"

int main(int, char**)
{
    {
        tim::CircularBuffer<int> c;
        c.push_front(0);
        assert(c.size() == 1);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(1);
        assert(c.size() == 2);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(2);
        assert(c.size() == 3);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(3);
        assert(c.size() == 4);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(4);
        assert(c.size() == 5);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
    }
    {
        // libc++ needs 15 because it grows by 2x (1 + 2 + 4 + 8).
        // Use 17 for implementations that dynamically allocate a container proxy
        // and grow by 1.5x (1 for proxy + 1 + 2 + 3 + 4 + 6).
        tim::CircularBuffer<int, limited_allocator<int, 17> > c;
        c.push_front(0);
        assert(c.size() == 1);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(1);
        assert(c.size() == 2);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(2);
        assert(c.size() == 3);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(3);
        assert(c.size() == 4);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(4);
        assert(c.size() == 5);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
    }
    {
        tim::CircularBuffer<int, min_allocator<int>> c;
        c.push_front(0);
        assert(c.size() == 1);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(1);
        assert(c.size() == 2);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(2);
        assert(c.size() == 3);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(3);
        assert(c.size() == 4);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
        c.push_front(4);
        assert(c.size() == 5);
        for (int j = 0; static_cast<std::size_t>(j) < c.size(); ++j)
            assert(c[j] == int(c.size() - (j + 1)));
    }

  return 0;
}
