cmake_minimum_required(VERSION 3.20)
project(T1)

add_subdirectory(function)

add_executable(main main.cpp)
target_link_libraries(main function)