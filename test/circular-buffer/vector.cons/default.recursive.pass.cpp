//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>
// class vector
// vector();

#include "tim/circular-buffer/CircularBuffer.hpp"

#include "test_macros.h"

struct X
{
	tim::CircularBuffer<X> q;
};

int main(int, char**)
{
	return 0;
}
