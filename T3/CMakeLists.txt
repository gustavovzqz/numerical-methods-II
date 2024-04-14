cmake_minimum_required(VERSION 3.20)
project(T3)

add_subdirectory(integral)

add_executable(t3 main.cpp)
target_link_libraries(t3 function integral)