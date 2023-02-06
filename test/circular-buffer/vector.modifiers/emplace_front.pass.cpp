//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// template <class... Args> reference emplace_front(Args&&... args);
// return type is 'reference' in C++17; 'void' before

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <numeric>
#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "test_allocator.h"
#include "asan_testing.h"
#include "Counter.h"
#include "test_buffers.h"

class A
{
	int i_;
	double d_;

	A(const A&);
	A& operator=(const A&);
public:
	A(int i, double d)
		: i_(i), d_(d) {}

	A(A&& a)
		: i_(a.i_),
		  d_(a.d_)
	{
		a.i_ = 0;
		a.d_ = 0;
	}

	A& operator=(A&& a)
	{
		i_ = a.i_;
		d_ = a.d_;
		a.i_ = 0;
		a.d_ = 0;
		return *this;
	}

	int geti() const {return i_;}
	double getd() const {return d_;}
};

int main(int, char**)
{
	{
		tim::CircularBuffer<A> c;
		A& r1 = c.emplace_front(2, 3.5);
		assert(c.size() == 1);
		assert(&r1 == &c.front());
		assert(c.back().geti() == 2);
		assert(c.back().getd() == 3.5);
		A& r2 = c.emplace_front(3, 4.5);
		assert(c.size() == 2);
		assert(&r2 == &c.front());
		assert(c.back().geti() == 2);
		assert(c.back().getd() == 3.5);
		assert(c.front().geti() == 3);
		assert(c.front().getd() == 4.5);
	}
	{
		tim::CircularBuffer<A, limited_allocator<A, 4> > c;
		A& r1 = c.emplace_front(2, 3.5);
		assert(c.size() == 1);
		assert(&r1 == &c.front());
		assert(c.back().geti() == 2);
		assert(c.back().getd() == 3.5);
		A& r2 = c.emplace_front(3, 4.5);
		assert(c.size() == 2);
		assert(&r2 == &c.front());
		assert(c.back().geti() == 2);
		assert(c.back().getd() == 3.5);
		assert(c.front().geti() == 3);
		assert(c.front().getd() == 4.5);
	}
	{
		tim::CircularBuffer<A, min_allocator<A>> c;
		A& r1 = c.emplace_front(2, 3.5);
		assert(c.size() == 1);
		assert(&r1 == &c.front());
		assert(c.back().geti() == 2);
		assert(c.back().getd() == 3.5);
		A& r2 = c.emplace_front(3, 4.5);
		assert(c.size() == 2);
		assert(&r2 == &c.front());
		assert(c.back().geti() == 2);
		assert(c.back().getd() == 3.5);
		assert(c.front().geti() == 3);
		assert(c.front().getd() == 4.5);
	}
	{
		tim::CircularBuffer<Tag_X, TaggingAllocator<Tag_X>> c;
		c.emplace_front();
		assert(c.size() == 1);
		c.emplace_front(1, 2, 3);
		assert(c.size() == 2);
	}

	{ // LWG 2164
		int arr[] = {0, 1, 2, 3, 4};
		int sz = 5;
		tim::CircularBuffer<int> c(arr, arr+sz);
		c.reserve(8);
		while (c.size() < c.capacity())
			c.push_back(sz++);
		c.emplace_front(c.front());
		--c.front();
		for (int i = 0; i < int(c.size()); ++i)
			assert(c[i] == i-1);
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& buf: get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto test_buf = buf.exact_copy();
			std::iota(test_buf.begin(), test_buf.end(), 0); 
			auto b = test_buf.exact_copy();
			b.emplace_back(-1);
			assert(b.size() == test_buf.size() + 1);
			assert(std::equal(b.begin(), b.end()-1, test_buf.begin(), test_buf.end()));
			assert(b.back() == Counter<int>(-1));
		}
	}
	struct Throws
	{
		Throws() = default;

		explicit Throws(bool ShouldThrow) : ShouldThrow(ShouldThrow) {}
		Throws(const Throws& other)
		{
			if (other.ShouldThrow)
			{
				throw std::runtime_error("Throws(Throws(true))");
			}
		}

		constexpr auto operator<=>(const Throws&) const = default;

		bool ShouldThrow = false;
	};

	assert(test_allocator<Counter<Throws>>::alloc_count == 0);
	assert(Counter<Throws>::gConstructed == 0);
	{
		for(const auto& buf: get_test_buffers(test_allocator<Counter<Throws>>()))
		{
			auto test_buf = buf.exact_copy();
			auto b = test_buf.exact_copy();
			auto constructed_before = Counter<Throws>::gConstructed;
			try
			{
				b.emplace_back(Throws(true));
			}
			catch(const std::runtime_error& err)
			{
				assert(std::string_view(err.what()) == "Throws(Throws(true))");
			}
			b._assert_invariants();
			assert(Counter<Throws>::gConstructed == constructed_before);
			assert(b == test_buf);
		}
	}
	assert(test_allocator<Counter<Throws>>::alloc_count == 0);
	assert(Counter<Throws>::gConstructed == 0);

	return 0;
}
