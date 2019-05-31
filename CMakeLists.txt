cmake_minimum_required(VERSION 3.1.3)
project(oneof CXX)
enable_testing()

include(CTest)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY build)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY build)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY tests)

if(MINGW)
	add_definitions(-DMINGW_HAS_SECURE_API)
endif()

if(NOT MSVC)
	set(CMAKE_CXX_STANDARD 14)
	set(CMAKE_CXX_EXTENSIONS OFF)
	add_definitions(-Wall -Wconversion -Wsign-conversion -Wsign-compare)
else()
	add_definitions(/std:c++latest)
endif()

include_directories(include)

file(GLOB tests_srcs tests/*.cc)

if(BUILD_TESTING)
	add_executable(run ${tests_srcs})
	add_test(testall tests/run)
endif()
