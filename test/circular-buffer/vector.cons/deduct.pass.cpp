//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>
// UNSUPPORTED: c++98, c++03, c++11, c++14
// UNSUPPORTED: libcpp-no-deduction-guides


// template <class InputIterator, class Allocator = allocator<typename iterator_traits<InputIterator>::value_type>>
//	deque(InputIterator, InputIterator, Allocator = Allocator())
//	-> deque<typename iterator_traits<InputIterator>::value_type, Allocator>;
//


#include "tim/circular-buffer/CircularBuffer.hpp"
#include <iterator>
#include <cassert>
#include <cstddef>
#include <climits> // INT_MAX

#include "test_macros.h"
#include "test_iterators.h"
#include "test_allocator.h"

struct A {};

int main(int, char**)
{

//  Test the explicit deduction guides
	{
	const int arr[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	tim::CircularBuffer vec(std::begin(arr), std::end(arr));

	static_assert(std::is_same_v<decltype(vec), tim::CircularBuffer<int>>, "");
	assert(std::equal(vec.begin(), vec.end(), std::begin(arr), std::end(arr)));
	}

	{
	const long arr[] = {INT_MAX, 1L, 2L, 3L };
	tim::CircularBuffer vec(std::begin(arr), std::end(arr), std::allocator<long>());
	static_assert(std::is_same_v<decltype(vec)::value_type, long>, "");
	assert(vec.size() == 4);
	assert(vec[0] == INT_MAX);
	assert(vec[1] == 1L);
	assert(vec[2] == 2L);
	}

//  Test the implicit deduction guides

	{
//  We don't expect this one to work.
//  tim::CircularBuffer vec(std::allocator<int>()); // vector (allocator &)
	}

	{
		tim::CircularBuffer vec(1, A{}); // vector (size_type, T)
		static_assert(std::is_same_v<decltype(vec)::value_type, A>, "");
		static_assert(std::is_same_v<decltype(vec)::allocator_type, std::allocator<A>>, "");
		assert(vec.size() == 1);
	}

	{
		tim::CircularBuffer vec(1, A{}, test_allocator<A>()); // vector (size_type, T, allocator)
		static_assert(std::is_same_v<decltype(vec)::value_type, A>, "");
		static_assert(std::is_same_v<decltype(vec)::allocator_type, test_allocator<A>>, "");
		assert(vec.size() == 1);
	}

	{
		tim::CircularBuffer vec{1U, 2U, 3U, 4U, 5U}; // vector(initializer-list)
		static_assert(std::is_same_v<decltype(vec)::value_type, unsigned>, "");
		assert(vec.size() == 5);
		assert(vec[2] == 3U);
	}

	{
		tim::CircularBuffer vec({1.0, 2.0, 3.0, 4.0}, test_allocator<double>()); // vector(initializer-list, allocator)
		static_assert(std::is_same_v<decltype(vec)::value_type, double>, "");
		static_assert(std::is_same_v<decltype(vec)::allocator_type, test_allocator<double>>, "");
		assert(vec.size() == 4);
		assert(vec[3] == 4.0);
	}

	{
		tim::CircularBuffer<long double> source;
		tim::CircularBuffer vec(source); // vector(vector &)
		static_assert(std::is_same_v<decltype(vec)::value_type, long double>, "");
		static_assert(std::is_same_v<decltype(vec)::allocator_type, std::allocator<long double>>, "");
		assert(vec.size() == 0);
	}


	{
		tim::CircularBuffer vec(3, true); // vector(initializer-list)
		static_assert(std::is_same_v<decltype(vec)::value_type, bool>, "");
		static_assert(std::is_same_v<decltype(vec)::allocator_type, std::allocator<bool>>, "");
		assert(vec.size() == 3);
		assert(vec[0] && vec[1] && vec[2]);
	}

	{
		tim::CircularBuffer<bool> source;
		tim::CircularBuffer vec(source); // vector(vector &)
		static_assert(std::is_same_v<decltype(vec)::value_type, bool>, "");
		static_assert(std::is_same_v<decltype(vec)::allocator_type, std::allocator<bool>>, "");
		assert(vec.size() == 0);
	}

	return 0;
}
