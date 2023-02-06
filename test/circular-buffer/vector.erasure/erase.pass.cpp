//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// UNSUPPORTED: c++98, c++03, c++11, c++14, c++17

// <vector>

// template <class T, class Allocator, class U>
//   void erase(vector<T, Allocator>& c, const U& value);
  

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <optional>
#include <numeric>
#include <algorithm>
#include <bitset>

#include "test_macros.h"
#include "test_allocator.h"
#include "min_allocator.h"
#include "Counter.h"
#include "test_buffers.h"

template <class S, class U>
void
test0(S s,  U val, S expected)
{
	ASSERT_SAME_TYPE(typename S::size_type, decltype(tim::erase(s, val)));
	tim::erase(s, val);
	assert(s == expected);
}

template <class S>
void test()
{

	test0(S(), 1, S());

	test0(S({1}), 1, S());
	test0(S({1}), 2, S({1}));

	test0(S({1,2}), 1, S({2}));
	test0(S({1,2}), 2, S({1}));
	test0(S({1,2}), 3, S({1,2}));
	test0(S({1,1}), 1, S());
	test0(S({1,1}), 3, S({1,1}));

	test0(S({1,2,3}), 1, S({2,3}));
	test0(S({1,2,3}), 2, S({1,3}));
	test0(S({1,2,3}), 3, S({1,2}));
	test0(S({1,2,3}), 4, S({1,2,3}));

	test0(S({1,1,1}), 1, S());
	test0(S({1,1,1}), 2, S({1,1,1}));
	test0(S({1,1,2}), 1, S({2}));
	test0(S({1,1,2}), 2, S({1,1}));
	test0(S({1,1,2}), 3, S({1,1,2}));
	test0(S({1,2,2}), 1, S({2,2}));
	test0(S({1,2,2}), 2, S({1}));
	test0(S({1,2,2}), 3, S({1,2,2}));

//  Test cross-type erasure
	using opt = std::optional<typename S::value_type>;
	test0(S({1,2,1}), opt(),  S({1,2,1}));
	test0(S({1,2,1}), opt(1), S({2}));
	test0(S({1,2,1}), opt(2), S({1,1}));
	test0(S({1,2,1}), opt(3), S({1,2,1}));
}


int main(int, char**)
{
	test<tim::CircularBuffer<int>>();
	test<tim::CircularBuffer<int, min_allocator<int>>> ();
	test<tim::CircularBuffer<int, test_allocator<int>>> ();

	test<tim::CircularBuffer<long>>();
	test<tim::CircularBuffer<double>>();
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		auto test_buf = get_empty_buffer(test_allocator<Counter<int>>());
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase(b, -1);
			assert(count == 0u);
			assert(b.empty());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return true; });
			assert(count == 0u);
			assert(b.empty());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return false; });
			assert(count == 0u);
			assert(b.empty());
		}
	}
	{
		auto test_buf = get_empty_buffer_with_capacity(test_allocator<Counter<int>>());
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase(b, -1);
			assert(count == 0u);
			assert(b.empty());
			assert(b.capacity() == test_buf.capacity());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return true; });
			assert(count == 0u);
			assert(b.empty());
			assert(b.capacity() == test_buf.capacity());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return false; });
			assert(count == 0u);
			assert(b.empty());
			assert(b.capacity() == test_buf.capacity());
		}
	}
	{
		auto test_buf = get_unary_buffer(test_allocator<Counter<int>>());
		std::iota(test_buf.begin(), test_buf.end(), 0);
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase(b, -1);
			assert(count == 0);
			assert(b == test_buf);
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase(b, 0);
			assert(count == 1);
			assert(b.empty());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return true; });
			assert(count == test_buf.size());
			assert(b.empty());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return false; });
			assert(count == 0u);
			assert(b == test_buf);
			assert(b.capacity() == test_buf.capacity());
		}
	}
	{
		auto test_buf = get_unary_buffer_with_back_slack(test_allocator<Counter<int>>());
		std::iota(test_buf.begin(), test_buf.end(), 0);
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase(b, -1);
			assert(count == 0);
			assert(b == test_buf);
			assert(b.capacity() == test_buf.capacity());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase(b, 0);
			assert(count == 1);
			assert(b.empty());
			assert(b.capacity() == test_buf.capacity());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return true; });
			assert(count == test_buf.size());
			assert(b.empty());
			assert(b.capacity() == test_buf.capacity());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return false; });
			assert(count == 0u);
			assert(b == test_buf);
			assert(b.capacity() == test_buf.capacity());
		}
	}
	{
		auto test_buf = get_unary_buffer_with_front_slack(test_allocator<Counter<int>>());
		std::iota(test_buf.begin(), test_buf.end(), 0);
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase(b, -1);
			assert(count == 0);
			assert(b == test_buf);
			assert(b.capacity() == test_buf.capacity());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase(b, 0);
			assert(count == 1);
			assert(b.empty());
			assert(b.capacity() == test_buf.capacity());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return true; });
			assert(count == test_buf.size());
			assert(b.empty());
			assert(b.capacity() == test_buf.capacity());
		}
		{
			auto b = test_buf.exact_copy();
			auto count = tim::erase_if(b, [](const auto&) { return false; });
			assert(count == 0u);
			assert(b == test_buf);
			assert(b.capacity() == test_buf.capacity());
		}
	}
	{
		
		auto test_buf = get_simple_wrapped_buffer(test_allocator<Counter<int>>());
		std::iota(test_buf.begin(), test_buf.end(), 0);
		for(std::size_t k= 0; k< (1ull << test_buf.size()); ++k)
		{
			std::bitset<64> key(k);
			{
				auto b = test_buf.exact_copy();
				auto cnt = tim::erase_if(b, [key](const auto& c) { return key[c.get()]; });
				assert(key.count() == cnt);
				auto it = b.begin();
				for(int i = 0; i < (int)test_buf.size(); ++i)
				{
					if(!key[i])
					{
						assert(it != b.end());
						assert(*it == test_buf[i]);
						++it;
					}
				}
				assert(it == b.end());
			}
		}
		{
			auto b = test_buf.exact_copy();
			std::fill(b.begin(), b.end(), Counter<int>(-1));
			auto cnt = tim::erase(b, Counter<int>(-1));
			assert(cnt == test_buf.size());
			assert(b.empty());
		}
		{
			auto b = test_buf.exact_copy();
			std::fill(b.begin(), b.end(), Counter<int>(-1));
			auto cnt = tim::erase(b, Counter<int>(0));
			assert(cnt == 0);
			assert(b.size() == test_buf.size());
			assert(std::all_of(b.begin(), b.end(), [](const auto& v) { return v == Counter<int>(-1); }));
		}
		{
			for(int i = 0; i < (int)test_buf.size(); ++i)
			{
				auto b = test_buf.exact_copy();
				auto cnt = tim::erase(b, Counter<int>(i));
				assert(cnt == 1);
				assert(b.size() == test_buf.size() - 1);
				assert(std::find(b.begin(), b.end(), Counter<int>(i)) == b.end());
			}
		}
	}
	{
		auto test_buf = get_simple_buffer_with_front_and_back_slack(test_allocator<Counter<int>>());
		assert(test_buf.begin_index() > 0);
		assert(test_buf.end_index() < test_buf.capacity());
		std::iota(test_buf.begin(), test_buf.end(), 0);
		for(std::size_t key = 0; key < (1ull << test_buf.size()); ++key)
		{
			{
				auto b = test_buf.exact_copy();
				auto cnt = tim::erase_if(b, [key](const auto& c) { return ((key >> c.get()) & 0x01u) != 0u; });
				assert(std::popcount(key) == (int)cnt);
				auto it = b.begin();
				for(int i = 0; i < (int)test_buf.size(); ++i)
				{
					if(((key >> i) & 0x01u) == 0u)
					{
						assert(it != b.end());
						assert(*it == test_buf[i]);
						++it;
					}
				}
				assert(it == b.end());
			}
		}
		{
			auto b = test_buf.exact_copy();
			std::fill(b.begin(), b.end(), Counter<int>(-1));
			auto cnt = tim::erase(b, Counter<int>(-1));
			assert(cnt == test_buf.size());
			assert(b.empty());
		}
		{
			auto b = test_buf.exact_copy();
			std::fill(b.begin(), b.end(), Counter<int>(-1));
			auto cnt = tim::erase(b, Counter<int>(0));
			assert(cnt == 0);
			assert(b.size() == test_buf.size());
			assert(std::all_of(b.begin(), b.end(), [](const auto& v) { return v == Counter<int>(-1); }));
		}
		{
			for(int i = 0; i < (int)test_buf.size(); ++i)
			{
				auto b = test_buf.exact_copy();
				auto cnt = tim::erase(b, Counter<int>(i));
				assert(cnt == 1);
				assert(b.size() == test_buf.size() - 1);
				assert(std::find(b.begin(), b.end(), Counter<int>(i)) == b.end());
			}
		}
	}
	{
		auto test_buf = get_large_wrapped_buffer(test_allocator<Counter<int>>());
		{
			auto b = test_buf.exact_copy();
			std::fill(b.begin(), b.end(), Counter<int>(-1));
			auto cnt = tim::erase(b, Counter<int>(-1));
			assert(cnt == test_buf.size());
			assert(b.empty());
		}
		{
			auto b = test_buf.exact_copy();
			std::fill(b.begin(), b.end(), Counter<int>(-1));
			auto cnt = tim::erase(b, Counter<int>(0));
			assert(cnt == 0);
			assert(b.size() == test_buf.size());
			assert(std::all_of(b.begin(), b.end(), [](const auto& v) { return v == Counter<int>(-1); }));
		}
		{
			for(int i = 0; i < (int)test_buf.size(); ++i)
			{
				auto b = test_buf.exact_copy();
				std::iota(b.begin(), b.end(), 0);
				auto cnt = tim::erase(b, Counter<int>(i));
				assert(cnt == 1);
				assert(b.size() == test_buf.size() - 1);
				assert(std::find(b.begin(), b.end(), Counter<int>(i)) == b.end());
			}
		}
		{
			auto b = test_buf.exact_copy();
			std::iota(b.begin(), b.end(), 0);
			auto sz = b.size();
			auto pred = [sz](const auto& v) { return v.get() < (int)(sz / 3); };
			auto cnt = tim::erase_if(b, pred);
			assert(cnt == (sz - b.size()));
			assert(cnt == (sz / 3));
			assert(std::none_of(b.begin(), b.end(), pred));
		}
		{
			auto b = test_buf.exact_copy();
			std::iota(b.begin(), b.end(), 0);
			auto sz = b.size();
			auto pred = [sz](const auto& v) { return v.get() < (int)(2 * sz / 3); };
			auto cnt = tim::erase_if(b, pred);
			assert(cnt == (sz - b.size()));
			assert(cnt == (2 * sz / 3));
			assert(std::none_of(b.begin(), b.end(), pred));
		}
		{
			auto b = test_buf.exact_copy();
			std::iota(b.begin(), b.end(), 0);
			auto sz = b.size();
			auto pred = [sz](const auto& v) { return v.get() >= (int)(sz / 3); };
			auto cnt = tim::erase_if(b, pred);
			assert(cnt == (sz - b.size()));
			assert(cnt == (sz - (sz / 3)));
			assert(std::none_of(b.begin(), b.end(), pred));
		}
	}
	{
		for(const auto& buf: get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto test_buf = buf.exact_copy();
			std::iota(test_buf.begin(), test_buf.end(), 0); 
			{
				auto b = test_buf.exact_copy();
				auto count = tim::erase(b, -1);
				assert(count == 0);
				assert(b == test_buf);
				assert(b.capacity() == test_buf.capacity());
			}
			{
				auto b = test_buf.exact_copy();
				auto count = tim::erase(b, 0);
				assert(count == 1 || test_buf.empty());
				assert(b.size() == (test_buf.empty() ? 0u : test_buf.size() - 1));
				assert(b.capacity() == test_buf.capacity());
			}
			{
				auto b = test_buf.exact_copy();
				auto count = tim::erase_if(b, [](const auto&) { return true; });
				assert(count == test_buf.size());
				assert(b.empty());
				assert(b.capacity() == test_buf.capacity());
			}
			{
				auto b = test_buf.exact_copy();
				auto count = tim::erase_if(b, [](const auto&) { return false; });
				assert(count == 0u);
				assert(b == test_buf);
				assert(b.capacity() == test_buf.capacity());
			}		
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);

	return 0;
}
