//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>
//   The container's value type must be the same as the allocator's value type

#include "tim/circular-buffer/CircularBuffer.hpp"

int main(int, char**)
{
    tim::CircularBuffer<int, std::allocator<long> > v;

  return 0;
}
