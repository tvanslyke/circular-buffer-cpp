//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

//	   reference operator[](size_type __i);
// const_reference operator[](size_type __i) const;
//
//	   reference at(size_type __i);
// const_reference at(size_type __i) const;
//
//	   reference front();
// const_reference front() const;
//
//	   reference back();
// const_reference back() const;
// libc++ marks these as 'noexcept' (except 'at')

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <algorithm>
#include <numeric>

#include "min_allocator.h"
#include "test_macros.h"
#include "test_allocator.h"
#include "Counter.h"
#include "test_buffers.h"

template <class C>
C
make(int size, int start = 0)
{
	C c;
	for (int i = 0; i < size; ++i)
		c.push_back(start + i);
	return c;
}

int main(int, char**)
{
	{
		typedef tim::CircularBuffer<int> C;
		C c = make<C>(10);
		// at() is NOT noexcept
		ASSERT_SAME_TYPE(C::reference, decltype(c[0]));
		ASSERT_SAME_TYPE(C::reference, decltype(c.at(0)));
		ASSERT_SAME_TYPE(C::reference, decltype(c.front()));
		ASSERT_SAME_TYPE(C::reference, decltype(c.back()));
		for (int i = 0; i < 10; ++i)
			assert(c[i] == i);
		for (int i = 0; i < 10; ++i)
			assert(c.at(i) == i);
		assert(c.front() == 0);
		assert(c.back() == 9);
	}
	{
		typedef tim::CircularBuffer<int> C;
		const int N = 5;
		const C c = make<C>(10, N);
		// at() is NOT noexcept
		ASSERT_SAME_TYPE(C::const_reference, decltype(c[0]));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.at(0)));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.front()));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.back()));
		for (int i = 0; i < 10; ++i)
			assert(c[i] == N + i);
		for (int i = 0; i < 10; ++i)
			assert(c.at(i) == N + i);
		assert(c.front() == N);
		assert(c.back() == N + 9);
	}
	{
		typedef tim::CircularBuffer<int, min_allocator<int>> C;
		const int N = 34;
		C c = make<C>(10, N);
		// at() is NOT noexcept
		ASSERT_SAME_TYPE(C::reference, decltype(c[0]));
		ASSERT_SAME_TYPE(C::reference, decltype(c.at(0)));
		ASSERT_SAME_TYPE(C::reference, decltype(c.front()));
		ASSERT_SAME_TYPE(C::reference, decltype(c.back()));
		for (int i = 0; i < 10; ++i)
			assert(c[i] == N + i);
		for (int i = 0; i < 10; ++i)
			assert(c.at(i) == N + i);
		assert(c.front() == N);
		assert(c.back() == N + 9);
	}
	{
		typedef tim::CircularBuffer<int, min_allocator<int>> C;
		const int N = 23;
		const C c = make<C>(10, N);
		// at() is NOT noexcept
		ASSERT_SAME_TYPE(C::const_reference, decltype(c[0]));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.at(0)));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.front()));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.back()));
		for (int i = 0; i < 10; ++i)
			assert(c[i] == N + i);
		for (int i = 0; i < 10; ++i)
			assert(c.at(i) == N + i);
		assert(c.front() == N);
		assert(c.back() == N + 9);
	}
	{
		typedef tim::CircularBuffer<int, min_allocator<int>> C;
		const int N = 23;
		const C c = make<C>(10, N);
		// at() is NOT noexcept
		ASSERT_SAME_TYPE(C::const_reference, decltype(c[0]));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.at(0)));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.front()));
		ASSERT_SAME_TYPE(C::const_reference, decltype(c.back()));
		for (int i = 0; i < 10; ++i)
			assert(c[i] == N + i);
		for (int i = 0; i < 10; ++i)
			assert(c.at(i) == N + i);
		assert(c.front() == N);
		assert(c.back() == N + 9);
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type i = 0; i != buf.size(); ++i)
			{
				assert(buf[i] == Counter<int>(i));
				assert(buf.at(i) == Counter<int>(i));
			}
			if(!buf.empty())
			{
				assert(buf.front() == Counter<int>(0));
				assert(buf.back() == Counter<int>((int)(buf.size() - 1)));
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);


	return 0;
}
