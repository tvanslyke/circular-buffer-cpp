//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// UNSUPPORTED: c++98, c++03, c++11, c++14, c++17

// <vector>

// template <class T, class Allocator, class Predicate>
//   void erase_if(vector<T, Allocator>& c, Predicate pred);  

#include "tim/circular-buffer/CircularBuffer.hpp"

#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"

template <class S, class Pred>
void
test0(S s, Pred p, S expected)
{
	ASSERT_SAME_TYPE(typename S::size_type, decltype(tim::erase_if(s, p)));
	tim::erase_if(s, p);
	assert(s == expected);
}

template <typename S>
void test()
{
	auto is1 = [](auto v) { return v == 1;};
	auto is2 = [](auto v) { return v == 2;};
	auto is3 = [](auto v) { return v == 3;};
	auto is4 = [](auto v) { return v == 4;};
	auto True  = [](auto) { return true; };
	auto False = [](auto) { return false; };
	
	test0(S(), is1, S());

	test0(S({1}), is1, S());
	test0(S({1}), is2, S({1}));

	test0(S({1,2}), is1, S({2}));
	test0(S({1,2}), is2, S({1}));
	test0(S({1,2}), is3, S({1,2}));
	test0(S({1,1}), is1, S());
	test0(S({1,1}), is3, S({1,1}));

	test0(S({1,2,3}), is1, S({2,3}));
	test0(S({1,2,3}), is2, S({1,3}));
	test0(S({1,2,3}), is3, S({1,2}));
	test0(S({1,2,3}), is4, S({1,2,3}));

	test0(S({1,1,1}), is1, S());
	test0(S({1,1,1}), is2, S({1,1,1}));
	test0(S({1,1,2}), is1, S({2}));
	test0(S({1,1,2}), is2, S({1,1}));
	test0(S({1,1,2}), is3, S({1,1,2}));
	test0(S({1,2,2}), is1, S({2,2}));
	test0(S({1,2,2}), is2, S({1}));
	test0(S({1,2,2}), is3, S({1,2,2}));
	
	test0(S({1,2,3}), True,  S());
	test0(S({1,2,3}), False, S({1,2,3}));
}

int main(int, char**)
{
	test<tim::CircularBuffer<int>>();
	test<tim::CircularBuffer<int, min_allocator<int>>> ();
	test<tim::CircularBuffer<int, test_allocator<int>>> ();

	test<tim::CircularBuffer<long>>();
	test<tim::CircularBuffer<double>>();

	return 0;
}
