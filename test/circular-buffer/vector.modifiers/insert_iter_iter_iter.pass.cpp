//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// <vector>

// template <class Iter>
//   iterator insert(const_iterator position, Iter first, Iter last);

#include "tim/circular-buffer/CircularBuffer.hpp"
#include <cassert>
#include <cstddef>
#include <numeric>

#include "test_macros.h"
#include "test_allocator.h"
#include "test_iterators.h"
#include "min_allocator.h"
#include "test_buffers.h"
#include "asan_testing.h"
#include "Counter.h"

int main(int, char**)
{
	{
		typedef tim::CircularBuffer<int> V;
		V v(100);
		int a[] = {1, 2, 3, 4, 5};
		const int N = sizeof(a)/sizeof(a[0]);
		V::iterator i = v.insert(v.cbegin() + 10, input_iterator<const int*>(a),
								 input_iterator<const int*>(a+N));
		assert(v.size() == 100 + N);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (std::size_t k = 0; k < N; ++j, ++k)
			assert(v[j] == a[k]);
		for (; j < 105; ++j)
			assert(v[j] == 0);
	}
	{
		typedef tim::CircularBuffer<int> V;
		V v(100);
		int a[] = {1, 2, 3, 4, 5};
		const int N = sizeof(a)/sizeof(a[0]);
		V::iterator i = v.insert(v.cbegin() + 10, forward_iterator<const int*>(a),
								 forward_iterator<const int*>(a+N));
		assert(v.size() == 100 + N);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (std::size_t k = 0; k < N; ++j, ++k)
			assert(v[j] == a[k]);
		for (; j < 105; ++j)
			assert(v[j] == 0);
	}
	{
		typedef tim::CircularBuffer<int> V;
		V v(100);
		while(v.size() < v.capacity()) v.push_back(0); // force reallocation
		size_t sz = v.size();
		int a[] = {1, 2, 3, 4, 5};
		const unsigned N = sizeof(a)/sizeof(a[0]);
		V::iterator i = v.insert(v.cbegin() + 10, forward_iterator<const int*>(a),
								 forward_iterator<const int*>(a+N));
		assert(v.size() == sz + N);
		assert(i == v.begin() + 10);
		std::size_t j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (std::size_t k = 0; k < N; ++j, ++k)
			assert(v[j] == a[k]);
		for (; j < v.size(); ++j)
			assert(v[j] == 0);
	}
	{
		typedef tim::CircularBuffer<int> V;
		V v(100);
		v.reserve(128); // force no reallocation
		size_t sz = v.size();
		int a[] = {1, 2, 3, 4, 5};
		const unsigned N = sizeof(a)/sizeof(a[0]);
		V::iterator i = v.insert(v.cbegin() + 10, forward_iterator<const int*>(a),
								 forward_iterator<const int*>(a+N));
		assert(v.size() == sz + N);
		assert(i == v.begin() + 10);
		std::size_t j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (std::size_t k = 0; k < N; ++j, ++k)
			assert(v[j] == a[k]);
		for (; j < v.size(); ++j)
			assert(v[j] == 0);
	}
	{
		typedef tim::CircularBuffer<int, limited_allocator<int, 308> > V;
		V v(100);
		int a[] = {1, 2, 3, 4, 5};
		const int N = sizeof(a)/sizeof(a[0]);
		V::iterator i = v.insert(v.cbegin() + 10, input_iterator<const int*>(a),
								 input_iterator<const int*>(a+N));
		assert(v.size() == 100 + N);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (std::size_t k = 0; k < N; ++j, ++k)
			assert(v[j] == a[k]);
		for (; j < 105; ++j)
			assert(v[j] == 0);
	}
	{
		typedef tim::CircularBuffer<int, limited_allocator<int, 300> > V;
		V v(100);
		int a[] = {1, 2, 3, 4, 5};
		const int N = sizeof(a)/sizeof(a[0]);
		V::iterator i = v.insert(v.cbegin() + 10, forward_iterator<const int*>(a),
								 forward_iterator<const int*>(a+N));
		assert(v.size() == 100 + N);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (std::size_t k = 0; k < N; ++j, ++k)
			assert(v[j] == a[k]);
		for (; j < 105; ++j)
			assert(v[j] == 0);
	}
	{
		typedef tim::CircularBuffer<int, min_allocator<int> > V;
		V v(100);
		int a[] = {1, 2, 3, 4, 5};
		const int N = sizeof(a)/sizeof(a[0]);
		V::iterator i = v.insert(v.cbegin() + 10, input_iterator<const int*>(a),
								 input_iterator<const int*>(a+N));
		assert(v.size() == 100 + N);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (std::size_t k = 0; k < N; ++j, ++k)
			assert(v[j] == a[k]);
		for (; j < 105; ++j)
			assert(v[j] == 0);
	}
	{
		typedef tim::CircularBuffer<int, min_allocator<int> > V;
		V v(100);
		int a[] = {1, 2, 3, 4, 5};
		const int N = sizeof(a)/sizeof(a[0]);
		V::iterator i = v.insert(v.cbegin() + 10, forward_iterator<const int*>(a),
								 forward_iterator<const int*>(a+N));
		assert(v.size() == 100 + N);
		assert(is_contiguous_container_asan_correct(v));
		assert(i == v.begin() + 10);
		int j;
		for (j = 0; j < 10; ++j)
			assert(v[j] == 0);
		for (std::size_t k = 0; k < N; ++j, ++k)
			assert(v[j] == a[k]);
		for (; j < 105; ++j)
			assert(v[j] == 0);
	}
	
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{
		for(const auto& test_buf : get_test_buffers(test_allocator<Counter<int>>()))
		{
			auto buf = test_buf.exact_copy();
			std::iota(buf.begin(), buf.end(), 0);
			for(const auto& test_src : get_test_buffers(test_allocator<Counter<int>>()))
			{
				auto src = test_src.exact_copy();
				std::iota(src.begin(), src.end(), (int)buf.size());
				for(int i = 0; i != (int)buf.size(); ++i)
				{
					auto b = buf.exact_copy();
					auto p = b.insert(b.begin()+i, src.begin(), src.end());
					assert(p == b.begin()+i);
					assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
					assert(std::equal(b.begin() + i, b.begin() + (i + src.size()), src.begin(), src.end()));
					assert(std::equal(b.begin() + (i + src.size()), b.end(), buf.begin() + i, buf.end()));
				}
			}
			{
				std::array<Counter<int>, 4> range{Counter<int>{0}, Counter<int>{1}, Counter<int>{2}, Counter<int>{3}};
				for(int j = 0; j != range.size(); ++j)
				{
					for(int i = 0; i != (int)buf.size(); ++i)
					{
						auto b = buf.exact_copy();
						auto p = b.insert(b.begin()+i, range.begin(), range.begin() + j);
						assert(p == b.begin()+i);
						assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
						assert(std::equal(b.begin() + i, b.begin() + (i + j), range.begin(), range.begin()+j));
						assert(std::equal(b.begin() + (i + j), b.end(), buf.begin() + i, buf.end()));
					}
					for(int i = 0; i != (int)buf.size(); ++i)
					{
						auto b = buf.exact_copy();
						auto p = b.insert(b.begin()+i, input_iterator<const Counter<int>*>(range.data()), input_iterator<const Counter<int>*>(range.data()+j));
						assert(p == b.begin()+i);
						assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
						assert(std::equal(b.begin() + i, b.begin() + (i + j), range.begin(), range.begin()+j));
						assert(std::equal(b.begin() + (i + j), b.end(), buf.begin() + i, buf.end()));
					}
					for(int i = 0; i != (int)buf.size(); ++i)
					{
						auto b = buf.exact_copy();
						auto p = b.insert(b.begin()+i, forward_iterator<const Counter<int>*>(range.data()), forward_iterator<const Counter<int>*>(range.data()+j));
						assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
						assert(std::equal(b.begin() + i, b.begin() + (i + j), range.begin(), range.begin()+j));
						assert(std::equal(b.begin() + (i + j), b.end(), buf.begin() + i, buf.end()));
					}
					for(int i = 0; i != (int)buf.size(); ++i)
					{
						auto b = buf.exact_copy();
						auto p = b.insert(b.begin()+i, bidirectional_iterator<const Counter<int>*>(range.data()), bidirectional_iterator<const Counter<int>*>(range.data()+j));
						assert(p == b.begin()+i);
						assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
						assert(std::equal(b.begin() + i, b.begin() + (i + j), range.begin(), range.begin()+j));
						assert(std::equal(b.begin() + (i + j), b.end(), buf.begin() + i, buf.end()));
					}
				}
			}
			{
				std::vector<std::size_t> sizes_to_insert;
				auto slack = buf.capacity() - buf.size();
				sizes_to_insert.push_back(slack+1);
				sizes_to_insert.push_back(slack);
				if(slack != 0)
				{
					sizes_to_insert.push_back(slack - 1);
				}
				if(sizes_to_insert.back() != 0)
				{
					sizes_to_insert.push_back(0);
				}
				for(auto count_to_insert :  sizes_to_insert)
				{
					std::vector<Counter<int>> range(count_to_insert);
					std::iota(range.begin(), range.end(), 0);
					for(int i = 0; i != (int)buf.size(); ++i)
					{
						auto b = buf.exact_copy();
						auto p = b.insert(b.begin()+i, range.begin(), range.end());
						assert(p == b.begin()+i);
						assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
						assert(std::equal(b.begin() + i, b.begin() + (i + (int)range.size()), range.begin(), range.end()));
						assert(std::equal(b.begin() + (i + (int)range.size()), b.end(), buf.begin() + i, buf.end()));
					}
					for(int i = 0; i != (int)buf.size(); ++i)
					{
						auto b = buf.exact_copy();
						auto p = b.insert(b.begin()+i, input_iterator<const Counter<int>*>(range.data()), input_iterator<const Counter<int>*>(range.data()+range.size()));
						assert(p == b.begin()+i);
						assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
						assert(std::equal(b.begin() + i, b.begin() + (i + (int)range.size()), range.begin(), range.end()));
						assert(std::equal(b.begin() + (i + (int)range.size()), b.end(), buf.begin() + i, buf.end()));
					}
					for(int i = 0; i != (int)buf.size(); ++i)
					{
						auto b = buf.exact_copy();
						auto p = b.insert(b.begin()+i, forward_iterator<const Counter<int>*>(range.data()), forward_iterator<const Counter<int>*>(range.data()+range.size()));
						assert(p == b.begin()+i);
						assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
						assert(std::equal(b.begin() + i, b.begin() + (i + (int)range.size()), range.begin(), range.end()));
						assert(std::equal(b.begin() + (i + (int)range.size()), b.end(), buf.begin() + i, buf.end()));
					}
					for(int i = 0; i != (int)buf.size(); ++i)
					{
						auto b = buf.exact_copy();
						auto p = b.insert(b.begin()+i, bidirectional_iterator<const Counter<int>*>(range.data()), bidirectional_iterator<const Counter<int>*>(range.data()+range.size()));
						assert(p == b.begin()+i);
						assert(std::equal(b.begin(), b.begin() + i, buf.begin(), buf.begin()+i));
						assert(std::equal(b.begin() + i, b.begin() + (i + (int)range.size()), range.begin(), range.end()));
						assert(std::equal(b.begin() + (i + (int)range.size()), b.end(), buf.begin() + i, buf.end()));
					}
				}
			}
		}
	}
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	return 0;
}
