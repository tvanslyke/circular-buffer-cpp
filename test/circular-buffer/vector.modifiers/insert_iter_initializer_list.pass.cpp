//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++98, c++03

// <vector>

// iterator insert(const_iterator p, initializer_list<value_type> il);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <algorithm>
#include <numeric>

#include "test_macros.h"
#include "min_allocator.h"
#include "asan_testing.h"
#include "test_buffers.h"
#include "Counter.h"
#include "test_allocator.h"

constexpr std::size_t max_init_list_size = 100;
template <class T, std::size_t ... I>
std::initializer_list<T> make_initializer_list_from_indices(const T& value, std::index_sequence<I...>)
{
	return {((void)I, value)...};
}
template <std::size_t N, class T>
std::initializer_list<T> make_initializer_list_helper(const T& value, std::size_t count)
{
	if constexpr(N > max_init_list_size)
	{
		assert(false);
		return {};
	}
	else
	{
		if(N == count)
		{
			return make_initializer_list_from_indices(value, std::make_index_sequence<N>{});
		}
		else
		{
			constexpr auto next_N = (N + 1 == max_init_list_size) ? max_init_list_size : N + 1;
			return make_initializer_list_helper<next_N>(value, count);
		}
	}
}


template <class T>
std::initializer_list<T> make_initializer_list(const T& value, std::size_t count)
{
	return make_initializer_list_helper<0>(value, count);
}


int main(int, char**)
{
	{
		tim::CircularBuffer<int> d(10, 1);
		tim::CircularBuffer<int>::iterator i = d.insert(d.cbegin() + 2, {3, 4, 5, 6});
		assert(d.size() == 14);
		assert(is_contiguous_container_asan_correct(d));
		assert(i == d.begin() + 2);
		assert(d[0] == 1);
		assert(d[1] == 1);
		assert(d[2] == 3);
		assert(d[3] == 4);
		assert(d[4] == 5);
		assert(d[5] == 6);
		assert(d[6] == 1);
		assert(d[7] == 1);
		assert(d[8] == 1);
		assert(d[9] == 1);
		assert(d[10] == 1);
		assert(d[11] == 1);
		assert(d[12] == 1);
		assert(d[13] == 1);
	}
	{
		tim::CircularBuffer<int, min_allocator<int>> d(10, 1);
		tim::CircularBuffer<int, min_allocator<int>>::iterator i = d.insert(d.cbegin() + 2, {3, 4, 5, 6});
		assert(d.size() == 14);
		assert(is_contiguous_container_asan_correct(d));
		assert(i == d.begin() + 2);
		assert(d[0] == 1);
		assert(d[1] == 1);
		assert(d[2] == 3);
		assert(d[3] == 4);
		assert(d[4] == 5);
		assert(d[5] == 6);
		assert(d[6] == 1);
		assert(d[7] == 1);
		assert(d[8] == 1);
		assert(d[9] == 1);
		assert(d[10] == 1);
		assert(d[11] == 1);
		assert(d[12] == 1);
		assert(d[13] == 1);
	}
	
	
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type i= 0; i != buf.size(); ++i)
			{
				{
					auto b = buf.exact_copy();
					auto p = b.insert(b.begin()+i, std::initializer_list<Counter<int>>{});
					assert(p == b.begin()+i);
					assert(b == buf);
					assert(std::equal(b.begin(), b.end(), buf.begin(), buf.end()));
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1};
					auto b = buf.exact_copy();
					auto p = b.insert(b.begin()+i, ilist);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2};
					auto b = buf.exact_copy();
					auto p = b.insert(b.begin()+i, ilist);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3};
					auto b = buf.exact_copy();
					auto p = b.insert(b.begin()+i, ilist);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3, -4};
					auto b = buf.exact_copy();
					auto p = b.insert(b.begin()+i, ilist);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3, -4, -5, -6, -7, -8, -9, -10};
					auto b = buf.exact_copy();
					auto p = b.insert(b.begin()+i, ilist);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
				}
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type i= 0; i != buf.size(); ++i)
			{
				{
					std::initializer_list<Counter<int>> ilist = {};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_front(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_begin_index = begin_idx < (int)ilist.size() ? (b.capacity() - ((int)ilist.size() - begin_idx)) : begin_idx - (int)ilist.size();
						assert(b.begin_index() == expected_new_begin_index);
						assert(b.end_index() == end_idx);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_front(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_begin_index = begin_idx < (int)ilist.size() ? (b.capacity() - ((int)ilist.size() - begin_idx)) : begin_idx - (int)ilist.size();
						assert(b.begin_index() == expected_new_begin_index);
						assert(b.end_index() == end_idx);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_front(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_begin_index = begin_idx < (int)ilist.size() ? (b.capacity() - ((int)ilist.size() - begin_idx)) : begin_idx - (int)ilist.size();
						assert(b.begin_index() == expected_new_begin_index);
						assert(b.end_index() == end_idx);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_front(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_begin_index = begin_idx < (int)ilist.size() ? (b.capacity() - ((int)ilist.size() - begin_idx)) : begin_idx - (int)ilist.size();
						assert(b.begin_index() == expected_new_begin_index);
						assert(b.end_index() == end_idx);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3, -4};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_front(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_begin_index = begin_idx < (int)ilist.size() ? (b.capacity() - ((int)ilist.size() - begin_idx)) : begin_idx - (int)ilist.size();
						assert(b.begin_index() == expected_new_begin_index);
						assert(b.end_index() == end_idx);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3, -4, -5, -6, -7, -8, -9, -10};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_front(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_begin_index = begin_idx < (int)ilist.size() ? (b.capacity() - ((int)ilist.size() - begin_idx)) : begin_idx - (int)ilist.size();
						assert(b.begin_index() == expected_new_begin_index);
						assert(b.end_index() == end_idx);
					}
				}
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			using size_type = test_allocator<Counter<int>>::size_type;
			for(size_type i= 0; i != buf.size(); ++i)
			{
				{
					std::initializer_list<Counter<int>> ilist = {};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_back(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_end_index = (end_idx + (int)ilist.size()) % b.capacity();
						assert(b.begin_index() == begin_idx); 
						assert(b.end_index() == expected_new_end_index);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_back(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_end_index = (end_idx + (int)ilist.size()) % b.capacity();
						assert(b.begin_index() == begin_idx); 
						assert(b.end_index() == expected_new_end_index);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_back(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_end_index = (end_idx + (int)ilist.size()) % b.capacity();
						assert(b.begin_index() == begin_idx); 
						assert(b.end_index() == expected_new_end_index);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_back(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_end_index = (end_idx + (int)ilist.size()) % b.capacity();
						assert(b.begin_index() == begin_idx); 
						assert(b.end_index() == expected_new_end_index);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3, -4};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_back(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_end_index = (end_idx + (int)ilist.size()) % b.capacity();
						assert(b.begin_index() == begin_idx); 
						assert(b.end_index() == expected_new_end_index);
					}
				}
				{
					std::initializer_list<Counter<int>> ilist = {-1, -2, -3, -4, -5, -6, -7, -8, -9, -10};
					auto b = buf.exact_copy();
					auto begin_idx = b.begin_index();
					auto end_idx = b.end_index();
					bool will_reallocate = b.size() + (int)ilist.size() >= b.capacity();
					auto p = b.insert_move_back(b.begin()+i, ilist);
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + (int)ilist.size()), ilist.begin(), ilist.end()));
					assert(std::equal(b.begin() + (i + (int)ilist.size()), b.end(), buf.begin() + i, buf.end()));
					if(!will_reallocate)
					{
						auto expected_new_end_index = (end_idx + (int)ilist.size()) % b.capacity();
						assert(b.begin_index() == begin_idx); 
						assert(b.end_index() == expected_new_end_index);
					}
				}
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);

	return 0;
}
