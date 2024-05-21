cmake_minimum_required(VERSION 3.10)
project(efficient_cpp)

# Set C++ standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Include Boost
find_package(Boost 1.85 REQUIRED)

# Include directories
include_directories(include ${Boost_INCLUDE_DIRS})

# Add executable for testing
add_executable(test_s test/test_s.cpp)

# Link the library and Boost to the test executable
target_link_libraries(test_s ${Boost_LIBRARIES})
