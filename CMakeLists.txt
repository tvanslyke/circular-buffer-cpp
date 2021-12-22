cmake_minimum_required(VERSION 3.12)

project(circular-buffer-cpp VERSION 1.0.0 LANGUAGES CXX)

option(CIRCULAR_BUFFER_ENABLE_TESTS "Enable tests." ON)

add_library(circular-buffer-cpp INTERFACE)

set(CXXSTD 20 CACHE STRING "C++ standard to use, default C++14")

enable_testing()

target_include_directories(circular-buffer-cpp INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_sources(circular-buffer-cpp INTERFACE
	${CMAKE_CURRENT_SOURCE_DIR}/include/tim/circular-buffer/CircularBuffer.hpp)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

if(CIRCULAR_BUFFER_ENABLE_TESTS)

	function(AddFailingTest NAME SOURCES)
		add_executable(test_${NAME} ${SOURCES} ${CMAKE_CURRENT_SOURCE_DIR}/include/tim/circular-buffer/CircularBuffer.natvis)
		target_compile_definitions(test_${NAME} PRIVATE TEST_STD_VER=${CXXSTD})
		set_property(TARGET test_${NAME} PROPERTY CXX_STANDARD ${CXXSTD})
		target_link_libraries(test_${NAME} circular-buffer-cpp)
		target_include_directories(test_${NAME} PRIVATE ${PROJECT_SOURCE_DIR}/test/support)
		if(MSVC)
			target_compile_options(test_${NAME} PRIVATE /W4)
		else()
			target_compile_options(test_${NAME} PRIVATE -Wall -Wextra -pedantic)
		endif() 
		set_target_properties(test_${NAME} PROPERTIES
			EXCLUDE_FROM_ALL TRUE
			EXCLUDE_FROM_DEFAULT_BUILD TRUE
			LINKER_LANGUAGE CXX)
		add_test(NAME ${NAME}
			COMMAND ${CMAKE_COMMAND} --build . --target test_${NAME}
			WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
		set_tests_properties(${NAME} PROPERTIES WILL_FAIL TRUE)
	endfunction(AddFailingTest)

	function(AddPassingTest NAME SOURCES)
		add_executable(test_${NAME} ${SOURCES} ${CMAKE_CURRENT_SOURCE_DIR}/include/tim/circular-buffer/CircularBuffer.natvis)
		target_compile_definitions(test_${NAME} PRIVATE TEST_STD_VER=${CXXSTD})
		set_property(TARGET test_${NAME} PROPERTY CXX_STANDARD ${CXXSTD})
		target_link_libraries(test_${NAME} circular-buffer-cpp)
		target_include_directories(test_${NAME} PRIVATE ${PROJECT_SOURCE_DIR}/test/support)
		if(MSVC)
			target_compile_options(test_${NAME} PRIVATE /W4)
		else()
			target_compile_options(test_${NAME} PRIVATE -Wall -Wextra -pedantic)
		endif()
		set_target_properties(test_${NAME} PROPERTIES LINKER_LANGUAGE CXX)
		add_test(NAME ${NAME}
			COMMAND ${CMAKE_BINARY_DIR}/bin/test_${NAME}
			WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
	endfunction(AddPassingTest)

	# Make test executable
	# set(TEST_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/tests/optional/main.cpp
	# 	${CMAKE_CURRENT_SOURCE_DIR}/tests/optional/constructors.cpp)




	# Tests that should fail to compile:
	AddFailingTest(allocator_mismatch_fail                                   ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/allocator_mismatch.fail.cpp)
	AddFailingTest(circular_buffer_capacity_empty_fail                       ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/empty.fail.cpp)
	AddFailingTest(circular_buffer_cons_deduct_fail                          ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/deduct.fail.cpp)
	AddFailingTest(circular_buffer_modifiers_resize_not_move_insertable_fail ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/resize_not_move_insertable.fail.cpp)

	# Tests that should pass normally:

	AddPassingTest(access_pass                                                 ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/access.pass.cpp)
	AddPassingTest(contiguous_pass                                             ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/contiguous.pass.cpp)
	AddPassingTest(iterators_pass                                              ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/iterators.pass.cpp)
	AddPassingTest(types_pass                                                  ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/types.pass.cpp)
	AddPassingTest(circular_buffer_capacity_capacity_pass                      ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/capacity.pass.cpp)
	AddPassingTest(circular_buffer_capacity_empty_pass                         ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/empty.pass.cpp)
	AddPassingTest(circular_buffer_capacity_max_size_pass                      ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/max_size.pass.cpp)
	AddPassingTest(circular_buffer_capacity_reserve_pass                       ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/reserve.pass.cpp)
	AddPassingTest(circular_buffer_capacity_resize_size_pass                   ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/resize_size.pass.cpp)
	AddPassingTest(circular_buffer_capacity_resize_size_value_pass             ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/resize_size_value.pass.cpp)
	AddPassingTest(circular_buffer_capacity_shrink_to_fit_pass                 ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/shrink_to_fit.pass.cpp)
	AddPassingTest(circular_buffer_capacity_size_pass                          ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/size.pass.cpp)
	AddPassingTest(circular_buffer_capacity_swap_pass                          ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.capacity/swap.pass.cpp)
	AddPassingTest(circular_buffer_cons_assign_copy_pass                       ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/assign_copy.pass.cpp)
	AddPassingTest(circular_buffer_cons_assign_initializer_list_pass           ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/assign_initializer_list.pass.cpp)
	AddPassingTest(circular_buffer_cons_assign_iter_iter_pass                  ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/assign_iter_iter.pass.cpp)
	AddPassingTest(circular_buffer_cons_assign_move_pass                       ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/assign_move.pass.cpp)
	AddPassingTest(circular_buffer_cons_assign_size_value_pass                 ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/assign_size_value.pass.cpp)
	AddPassingTest(circular_buffer_cons_construct_default_pass                 ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/construct_default.pass.cpp)
	AddPassingTest(circular_buffer_cons_construct_iter_iter_pass               ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/construct_iter_iter.pass.cpp)
	AddPassingTest(circular_buffer_cons_construct_iter_iter_alloc_pass         ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/construct_iter_iter_alloc.pass.cpp)
	AddPassingTest(circular_buffer_cons_construct_size_pass                    ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/construct_size.pass.cpp)
	AddPassingTest(circular_buffer_cons_construct_size_value_pass              ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/construct_size_value.pass.cpp)
	AddPassingTest(circular_buffer_cons_construct_size_value_alloc_pass        ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/construct_size_value_alloc.pass.cpp)
	AddPassingTest(circular_buffer_cons_copy_pass                              ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/copy.pass.cpp)
	AddPassingTest(circular_buffer_cons_copy_alloc_pass                        ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/copy_alloc.pass.cpp)
	AddPassingTest(circular_buffer_cons_deduct_pass                            ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/deduct.pass.cpp)
	AddPassingTest(circular_buffer_cons_default_recursive_pass                 ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/default.recursive.pass.cpp)
	AddPassingTest(circular_buffer_cons_default_noexcept_pass                  ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/default_noexcept.pass.cpp)
	AddPassingTest(circular_buffer_cons_dtor_noexcept_pass                     ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/dtor_noexcept.pass.cpp)
	AddPassingTest(circular_buffer_cons_initializer_list_pass                  ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/initializer_list.pass.cpp)
	AddPassingTest(circular_buffer_cons_initializer_list_alloc_pass            ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/initializer_list_alloc.pass.cpp)
	AddPassingTest(circular_buffer_cons_move_pass                              ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/move.pass.cpp)
	AddPassingTest(circular_buffer_cons_move_alloc_pass                        ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/move_alloc.pass.cpp)
	AddPassingTest(circular_buffer_cons_move_assign_noexcept_pass              ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/move_assign_noexcept.pass.cpp)
	AddPassingTest(circular_buffer_cons_move_noexcept_pass                     ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/move_noexcept.pass.cpp)
	AddPassingTest(circular_buffer_cons_op_equal_initializer_list_pass         ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.cons/op_equal_initializer_list.pass.cpp)
	AddPassingTest(circular_buffer_erasure_erase_pass                          ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.erasure/erase.pass.cpp)
	AddPassingTest(circular_buffer_erasure_erase_if_pass                       ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.erasure/erase_if.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_clear_pass                        ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/clear.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_emplace_pass                      ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/emplace.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_emplace_back_pass                 ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/emplace_back.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_emplace_extra_pass                ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/emplace_extra.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_erase_iter_pass                   ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/erase_iter.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_erase_iter_iter_pass              ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/erase_iter_iter.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_insert_iter_initializer_list_pass ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/insert_iter_initializer_list.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_insert_iter_iter_iter_pass        ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/insert_iter_iter_iter.pass.cpp)
	AddPassingTest(circular_buffer_modifiers_insert_iter_rvalue_pass           ${CMAKE_CURRENT_SOURCE_DIR}/test/circular-buffer/vector.modifiers/insert_iter_rvalue.pass.cpp)
endif(CIRCULAR_BUFFER_ENABLE_TESTS)
