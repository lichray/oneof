cmake_minimum_required(VERSION 3.1.3)
project(oneof CXX)

if(NOT MSVC)
	set(CMAKE_CXX_STANDARD 14)
	set(CMAKE_CXX_EXTENSIONS OFF)
	add_definitions(-Wall -Wconversion -Wsign-conversion -Wsign-compare)
else()
	add_definitions(/std:c++latest)
endif()

include_directories(include)

add_executable(a a.cc)
