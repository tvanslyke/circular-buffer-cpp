//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef ASAN_TESTING_H
#define ASAN_TESTING_H

#include "test_macros.h"

template <typename T, typename Alloc>
bool is_contiguous_container_asan_correct ( const tim::CircularBuffer<T, Alloc> &)
{
    return true;
}


#endif  // ASAN_TESTING_H
