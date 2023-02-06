#include <cassert>

#include "test_macros.h"
#include "test_allocator.h"
#include "Counter.h"
#include "test_buffers.h"
#include "tim/circular-buffer/CircularBuffer.hpp"


int main(int, char**)
{
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_empty_buffer(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_empty_buffer_with_capacity(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_unary_buffer(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_unary_buffer_with_back_slack(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_unary_buffer_with_front_slack(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_simple_wrapped_buffer(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_simple_buffer_with_front_and_back_slack(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_large_wrapped_buffer(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_large_full_buffer(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);
	{ auto buf = get_large_full_wrapped_buffer(test_allocator<Counter<int>>{}); }
	assert(test_allocator<Counter<int>>::alloc_count == 0);
	assert(Counter<int>::gConstructed == 0);

  return 0;
}

