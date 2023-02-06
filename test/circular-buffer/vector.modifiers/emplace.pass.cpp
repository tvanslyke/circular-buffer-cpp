//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// template <class... Args> iterator emplace(const_iterator pos, Args&&... args);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <string_view>
#include <numeric>

#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"
#include "Counter.h"

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
		tim::CircularBuffer<A>::iterator i = c.emplace(c.cbegin(), 2, 3.5);
		assert(i == c.begin());
		assert(c.size() == 1);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		assert(is_contiguous_container_asan_correct(c));
		i = c.emplace(c.cend(), 3, 4.5);
		assert(i == c.end()-1);
		assert(c.size() == 2);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		assert(c.back().geti() == 3);
		assert(c.back().getd() == 4.5);
		assert(is_contiguous_container_asan_correct(c));
		i = c.emplace(c.cbegin()+1, 4, 6.5);
		assert(i == c.begin()+1);
		assert(c.size() == 3);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		assert(c[1].geti() == 4);
		assert(c[1].getd() == 6.5);
		assert(c.back().geti() == 3);
		assert(c.back().getd() == 4.5);
		assert(is_contiguous_container_asan_correct(c));
	}
	{
		tim::CircularBuffer<A, limited_allocator<A, 7> > c;
		tim::CircularBuffer<A, limited_allocator<A, 7> >::iterator i = c.emplace(c.cbegin(), 2, 3.5);
		assert(i == c.begin());
		assert(c.size() == 1);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		assert(is_contiguous_container_asan_correct(c));
		i = c.emplace(c.cend(), 3, 4.5);
		assert(i == c.end()-1);
		assert(c.size() == 2);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		assert(c.back().geti() == 3);
		assert(c.back().getd() == 4.5);
		assert(is_contiguous_container_asan_correct(c));
		i = c.emplace(c.cbegin()+1, 4, 6.5);
		assert(i == c.begin()+1);
		assert(c.size() == 3);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		assert(c[1].geti() == 4);
		assert(c[1].getd() == 6.5);
		assert(c.back().geti() == 3);
		assert(c.back().getd() == 4.5);
		assert(is_contiguous_container_asan_correct(c));
	}
	{
		tim::CircularBuffer<A, min_allocator<A>> c;
		tim::CircularBuffer<A, min_allocator<A>>::iterator i = c.emplace(c.cbegin(), 2, 3.5);
		assert(i == c.begin());
		assert(c.size() == 1);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		i = c.emplace(c.cend(), 3, 4.5);
		assert(i == c.end()-1);
		assert(c.size() == 2);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		assert(c.back().geti() == 3);
		assert(c.back().getd() == 4.5);
		i = c.emplace(c.cbegin()+1, 4, 6.5);
		assert(i == c.begin()+1);
		assert(c.size() == 3);
		assert(c.front().geti() == 2);
		assert(c.front().getd() == 3.5);
		assert(c[1].geti() == 4);
		assert(c[1].getd() == 6.5);
		assert(c.back().geti() == 3);
		assert(c.back().getd() == 4.5);
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& buf: get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto test_buf = buf.exact_copy();
			std::iota(test_buf.begin(), test_buf.end(), 0); 
			for(int i = 0; i <= (int)test_buf.size(); ++i)
			{
				auto b = test_buf.exact_copy();
				b.emplace(b.begin() + i, -1);
				assert(b.size() == test_buf.size() + 1);
				assert(std::equal(b.begin(), b.begin() + i, test_buf.begin(), test_buf.begin() + i));
				assert(b[i] == Counter<int>(-1));
				assert(std::equal(std::next(b.begin() + i), b.end(), test_buf.begin() + i, test_buf.end()));
			}
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
			for(int i = 0; i < (int)test_buf.size(); ++i)
			{
				auto b = test_buf.exact_copy();
				auto constructed_before = Counter<Throws>::gConstructed;
				try
				{
					b.emplace(b.begin() + i, Throws(true));
				}
				catch(const std::runtime_error& err)
				{
					assert(std::string_view(err.what()) == "Throws(Throws(true))");
				}
				b._assert_invariants();
				if(i == 0 || i == (int)test_buf.size())
				{
					// emplacing an element at either end should have no effects if an exception is thrown.
					assert(b == test_buf);
					assert(Counter<Throws>::gConstructed == constructed_before);
				}
			}
		}
	}
	assert(test_allocator<Counter<Throws>>::alloc_count == 0);
	assert(Counter<Throws>::gConstructed == 0);

	return 0;
}
